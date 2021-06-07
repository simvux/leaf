use super::{AstType, LocationLinkedStore, Span};
use colored::Colorize;
use itertools::Itertools;
use lumina_env::Environment;
use lumina_parser::{ast, DefType, FileParser, Handler, HandlerError, TypeParams};
use lumina_util::{Location, LocationError, PFlags, Pathing, Tr};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

const TYPE_PREALLOC: usize = 20;
const TRAIT_PREALLOC: usize = 10;
const IMPL_PREALLOC: usize = 20;
const FUNC_PREALLOC: usize = 50;

/// Collector takes a stream of raw AST and stores it in data structures.
///
/// The Collector also expands top-level decleration identifiers and index's them.
pub struct Collector<'s> {
    storage: &'s mut Storage,
    files: HashSet<Rc<Location>>,

    pub origin: Rc<Location>,
    env: Rc<Environment>,
}

impl<'s> Collector<'s> {
    pub fn new(env: Rc<Environment>, origin: Rc<Location>, storage: &'s mut Storage) -> Self {
        let mut files = HashSet::new();
        files.insert(origin.clone());

        Self {
            origin,
            files,
            storage,
            env,
        }
    }

    /// Use the current `origin` as a module to load and collect
    pub fn include(&mut self) -> Result<(), HandlerError<Error>> {
        let (src, path) = self
            .origin
            .read_source_code(&self.env)
            .map_err(|e| HandlerError::Handler(CouldNotReadSource(e)))?;

        FileParser::new(self).parse(&src, &path.to_str().unwrap_or(""))
    }
}

#[derive(Debug)]
pub enum Error {
    Positioned(Span, Box<Self>),

    InvalidTypeName(LocationError),
    InvalidFuncName(LocationError),
    InvalidImportName(LocationError),

    FuncExists(Box<Function>),
    TraitExists(DefinedTrait),
    TypeExists(DefinedType),
    LinkExists(Tr<Rc<Location>>),

    CouldNotReadSource(std::io::Error),
}
use Error::*;

impl Error {
    /// Tag this error with a position in source code
    pub fn position(self, span: Span) -> Self {
        match self {
            e @ Self::Positioned(_, _) => e,
            other => Self::Positioned(span, Box::new(other)),
        }
    }
}

impl std::error::Error for Error {}

impl lumina_util::ToError for Error {
    fn name(&self) -> &'static str {
        "placeholder-name"
    }

    fn span(&self) -> Option<Span> {
        match self {
            Self::Positioned(span, _) => Some(span.clone()),
            _ => None,
        }
    }

    fn text(&self) -> String {
        match self {
            Error::Positioned(_, inner) => inner.to_string(),
            other => other.to_string(),
        }
    }
}

/// A defined type is a collected user-defined type with it's name expanded,
/// and types lowered to type IDs.
#[derive(Debug, PartialEq)]
pub struct DefinedType {
    pub span: Span,
    pub location: Rc<Location>,
    pub module: Rc<Location>,

    pub type_params: Vec<Tr<u8>>,
    pub body: DefinedTypeBody,
}

/// The body of an defined type.
///
/// Contains the fields/variants of the struct/enum.
#[derive(Debug, PartialEq, Clone)]
pub enum DefinedTypeBody {
    Struct(lumina_parser::Struct),
    Enum(lumina_parser::Enum),
}

/// A defined trait is a collected user-defined trait with it's name expanded,
/// and methods lowered to have typing consisting of type IDs
#[derive(Debug, PartialEq, Clone)]
pub struct DefinedTrait {
    pub span: Span,
    pub location: Rc<Location>,
    pub module: Rc<Location>,

    pub type_params: Vec<Tr<u8>>,
    pub body: lumina_parser::Trait,
}

/// A defined function is a collected user-defined function with it's name expanded,
/// and it's headers types lowered to have typing consisting of type IDs.
///
/// The body of the function is still unlowered raw AST.
#[derive(Debug, PartialEq)]
pub struct Function {
    pub span: Span,
    pub location: Rc<Location>,
    pub module: Rc<Location>,

    pub header: ast::function::Header,

    pub kind: FunctionKind,
}

#[derive(Debug, PartialEq)]
pub enum FunctionKind {
    Normal(ast::function::Body),
    Accessor { struct_: Rc<Location>, field: usize },
    Constructor { enum_: Rc<Location>, variant: usize },
    Method { trait_: Rc<Location>, mid: usize },
}

/// ImplTemplate is the AST representation of a trait implementation with absolute 0 certainty
/// regarding it's validity
#[derive(Debug, Default, Clone)]
pub struct ImplTemplate {
    pub span: Span,

    // TODO: Bounds?
    pub trait_: Tr<Location>,
    pub trait_tp: Vec<Tr<AstType>>,

    pub impltor: Tr<AstType>,
    pub impl_: HashMap<String, Tr<ast::Function>>,

    pub bounds: ast::Bounds<AstType>,

    pub origin: Rc<Location>,
}

/// Storage contains all of our types/functions/traits/implementations/imports after it's all been
/// expanded to be in the same scope with absolute top-level identifiers, and it's identifiers
/// having been linked to IDs.
#[derive(Debug, Default)]
pub struct Storage {
    pub types: LocationLinkedStore<DefinedType>,
    pub traits: LocationLinkedStore<DefinedTrait>,

    pub implementations: Vec<ImplTemplate>,

    pub functions: LocationLinkedStore<Function>,

    pub imports: Imports,
}

/// Imports links absolute paths to other absolute paths
#[derive(Debug, Default)]
pub struct Imports {
    links: HashMap<Location, Tr<Rc<Location>>>,
}

impl Imports {
    pub fn new() -> Imports {
        Imports {
            links: HashMap::new(),
        }
    }

    pub fn link(&mut self, src: Location, dst: Tr<Rc<Location>>) -> Result<(), Error> {
        match self.links.insert(src, dst) {
            Some(existing) => Err(LinkExists(existing)),
            None => Ok(()),
        }
    }

    pub fn expand_mut(&self, abs: &mut Location) {
        debug_assert!([Pathing::Project, Pathing::Library].contains(&abs.pathing));
        if let Some(dst) = self.links.get(abs) {
            *abs = (***dst).clone();
            self.expand_mut(abs)
        }
    }

    pub fn import<S: AsRef<str>>(
        &mut self,
        src: Location,
        dst: Tr<Rc<Location>>,
        exposing: &[Tr<S>],
    ) -> Result<(), Error> {
        self.link(src.clone(), dst.clone())?;

        exposing.iter().try_for_each(|exposed| {
            let mut src = src.clone();
            src.push(&exposed.inner);

            let mut dst = Tr::tr(exposed.span(), (**dst).clone());
            dst.push(&exposed.inner);

            self.link(src, dst.map(Rc::new))
        })
    }
}

impl Storage {
    pub fn new() -> Self {
        Self {
            types: LocationLinkedStore::with_capacity(TYPE_PREALLOC),
            traits: LocationLinkedStore::with_capacity(TRAIT_PREALLOC),
            implementations: Vec::with_capacity(IMPL_PREALLOC),
            functions: LocationLinkedStore::with_capacity(FUNC_PREALLOC),
            imports: Imports::new(),
        }
    }

    fn store_type(
        &mut self,
        span: Span,
        module: Rc<Location>,
        location: Rc<Location>,
        body: DefinedTypeBody,
        type_params: Vec<Tr<u8>>,
    ) -> Result<(), Error> {
        match self.types.insert(
            location.clone(),
            DefinedType {
                location,
                module,
                span: span.clone(),
                type_params,
                body,
            },
        ) {
            Some(existing) => Err(TypeExists(existing).position(span)),
            None => Ok(()),
        }
    }

    fn store_struct_accessors(
        &mut self,
        tlocation: Rc<Location>,
        module: Rc<Location>,
        struc: &lumina_parser::Struct,
        type_params: &[Tr<u8>],
    ) -> Result<(), Error> {
        struc
            .fields
            .iter()
            .enumerate()
            .try_for_each(|(fieldid, (name, type_))| {
                self.store_struct_accessor(
                    fieldid,
                    tlocation.clone(),
                    module.clone(),
                    name.as_ref().map(String::as_str),
                    type_gen_func_convert_type(type_.clone()),
                    type_params,
                )
                .map_err(|e| e.position(name.span()))
            })
    }

    // struct both a b
    //   x a
    //   y b
    //
    // fn x v (both a b -> a)
    // fn y v (both a b -> b)
    fn store_struct_accessor(
        &mut self,
        field: usize,
        tlocation: Rc<Location>,
        module: Rc<Location>,
        name: Tr<&str>,
        type_: Tr<AstType>,
        type_params: &[Tr<u8>],
    ) -> Result<(), Error> {
        let header = ast::function::Header {
            pnames: vec![],
            ptypes: vec![Tr::new(AstType::Defined(
                (*tlocation).clone(),
                type_params
                    .iter()
                    .map(|b| Tr::tr(b.span.clone(), AstType::Generic(b.inner, vec![])))
                    .collect(),
            ))],
            pflags: PFlags::new(),
            returns: type_gen_func_convert_type(type_),
            bounds: ast::Bounds::new(),
            attributes: vec![],
        };

        let mut accessor = (*module).clone();
        accessor.push(&**name);
        let accessor = Rc::new(accessor);

        self.store_func(
            name.span(),
            accessor,
            module,
            header,
            FunctionKind::Accessor {
                struct_: tlocation,
                field,
            },
        )
    }

    fn store_enum_constructors(
        &mut self,
        location: Rc<Location>,
        module: Rc<Location>,
        enm: &lumina_parser::Enum,
        type_parameters: &[Tr<u8>],
    ) -> Result<(), Error> {
        enm.variants
            .iter()
            .enumerate()
            .try_for_each(|(i, (name, tps))| {
                self.store_enum_constructor(
                    i,
                    location.clone(),
                    module.clone(),
                    name.as_ref().map(String::as_str),
                    tps,
                    type_parameters,
                )
                .map_err(|e| e.position(name.span()))
            })
    }
    fn store_enum_constructor(
        &mut self,
        variant: usize,
        tlocation: Rc<Location>,
        module: Rc<Location>,
        name: Tr<&str>,
        variant_type_params: &TypeParams,
        type_params: &[Tr<u8>],
    ) -> Result<(), Error> {
        // enum result a e
        //   ok  a
        //   err e
        //
        // fn ok  a (a -> result a e)
        // fn err a (e -> result a e)
        let ptypes: Vec<Tr<AstType>> = variant_type_params
            .iter()
            .cloned()
            .map(type_gen_func_convert_type)
            .collect();

        // TODO: `name.span` might cause missleading errors
        let returns = Tr::tr(
            name.span.clone(),
            AstType::Defined(
                (*tlocation).clone(),
                type_params
                    .iter()
                    .map(|b| Tr::tr(b.span.clone(), AstType::Generic(b.inner, vec![])))
                    .collect(),
            ),
        );

        let header = ast::function::Header {
            pnames: ('a'..)
                .take(variant_type_params.len())
                .map(String::from)
                .enumerate()
                .map(|(i, s)| Tr::tr(ptypes[i].span(), s))
                .collect(),
            ptypes,
            pflags: PFlags::new(),
            returns,
            bounds: ast::Bounds::new(),
            attributes: vec![],
        };

        let mut constructor = (*module).clone();
        constructor.push(&**name);
        let constructor = Rc::new(constructor);

        self.store_func(
            name.span(),
            constructor,
            module,
            header,
            FunctionKind::Constructor {
                enum_: tlocation,
                variant,
            },
        )
    }

    fn store_trait(
        &mut self,
        span: Span,
        module: Rc<Location>,
        location: Rc<Location>,
        body: lumina_parser::Trait,
        type_params: Vec<Tr<u8>>,
    ) -> Result<(), Error> {
        let defined = DefinedTrait {
            location: location.clone(),
            module,
            span: span.clone(),
            type_params,
            body,
        };
        self.store_trait_method(&defined)?;

        match self.traits.insert(location, defined) {
            Some(existing) => Err(TraitExists(existing).position(span)),
            None => Ok(()),
        }
    }

    fn store_trait_method(&mut self, trait_: &DefinedTrait) -> Result<(), Error> {
        for (mname, (header, mid)) in trait_.body.functions.iter() {
            let mut mlocation = (*trait_.location).clone();
            mlocation.push(mname);
            let mlocation = Rc::new(mlocation);

            self.store_func(
                header.span(),
                mlocation,
                trait_.module.clone(),
                header.inner.clone(),
                FunctionKind::Method {
                    trait_: trait_.location.clone(),
                    mid: *mid,
                },
            )?;
        }

        Ok(())
    }

    fn store_func(
        &mut self,
        span: Span,
        location: Rc<Location>,
        module: Rc<Location>,
        header: ast::function::Header,
        kind: FunctionKind,
    ) -> Result<(), Error> {
        match self.functions.insert(
            location.clone(),
            Function {
                span: span.clone(),
                location,
                module,
                header,
                kind,
            },
        ) {
            Some(existing) => Err(FuncExists(Box::new(existing)).position(span)),
            None => Ok(()),
        }
    }
}

impl Handler<Error> for Collector<'_> {
    fn on_use(
        &mut self,
        assign_to: Tr<String>,
        import: Tr<Location>,
        exposing: Vec<Tr<String>>,
    ) -> Result<(), HandlerError<Error>> {
        let span = import.span();

        // Create an absolute location of the current module + the value we assign the import to
        let src = self.origin.expand(
            Location::from_string(assign_to.inner)
                .map_err(|loc| HandlerError::Handler(Error::InvalidFuncName(loc)))?,
        );
        // Expand the destination import. This is required for relative imports to work.
        let dst = import.map(|l| Rc::new(self.origin.expand(l)));

        if self.env.output.ast {
            println!(
                "use {} as {}",
                src.to_string().green(),
                dst.to_string().green()
            );
        }

        self.storage
            .imports
            .import(src, dst.clone(), &exposing)
            .map_err(|e| e.position(span))
            .map_err(HandlerError::Handler)?;

        if self.env.output.ast {
            println!("including {}", &dst.inner.to_string().green());
        }

        // Don't open and parse the file if it's already been parsed
        if self.files.contains(&self.origin) {
            return Ok(());
        }

        Collector::new(self.env.clone(), dst.inner, &mut self.storage).include()
    }

    fn on_func(&mut self, func: Tr<ast::Function>) -> Result<(), Error> {
        let span = func.span();

        if self.env.output.ast {
            println!("{}", &func);
        }

        let expanded = Rc::new(
            self.origin.expand(
                Location::from_string(func.inner.name)
                    .map_err(|e| Error::InvalidFuncName(e).position(span.clone()))?,
            ),
        );

        self.storage.store_func(
            span,
            expanded,
            self.origin.clone(),
            func.inner.header,
            FunctionKind::Normal(func.inner.body),
        )
    }

    fn on_type(&mut self, type_: DefType) -> Result<(), Error> {
        let span = type_.span.clone().inner;

        if self.env.output.ast {
            println!("{}", &type_);
        }

        let expanded = Rc::new(
            self.origin.expand(
                Location::from_string(type_.name)
                    .map_err(|e| Error::InvalidTypeName(e).position(span.clone()))?,
            ),
        );

        match type_.kind {
            lumina_parser::TypeKind::Enum(t) => {
                self.storage.store_enum_constructors(
                    expanded.clone(),
                    self.origin.clone(),
                    &t,
                    &type_.type_parameters,
                )?;
                self.storage
                    .store_type(
                        type_.span.inner,
                        self.origin.clone(),
                        expanded,
                        DefinedTypeBody::Enum(t),
                        type_.type_parameters,
                    )
                    .map_err(|e| e.position(span))
            }
            lumina_parser::TypeKind::Struct(t) => {
                self.storage.store_struct_accessors(
                    expanded.clone(),
                    self.origin.clone(),
                    &t,
                    &type_.type_parameters,
                )?;
                self.storage
                    .store_type(
                        type_.span.inner,
                        self.origin.clone(),
                        expanded,
                        DefinedTypeBody::Struct(t),
                        type_.type_parameters,
                    )
                    .map_err(|e| e.position(span))
            }
            lumina_parser::TypeKind::Trait(tr) => self
                .storage
                .store_trait(
                    type_.span.inner,
                    self.origin.clone(),
                    expanded,
                    tr,
                    type_.type_parameters,
                )
                .map_err(|e| e.position(span)),
        }
    }

    fn on_impl(
        &mut self,
        decl_span: Span,
        trait_: Tr<Location>,
        type_parameters: Vec<Tr<AstType>>,
        bounds: ast::Bounds<AstType>,
        for_t: Tr<AstType>,
        impl_: HashMap<String, Tr<ast::Function>>,
    ) -> Result<(), Error> {
        self.storage.implementations.push(ImplTemplate {
            span: decl_span,
            trait_,
            trait_tp: type_parameters,
            impltor: for_t,
            impl_,
            bounds,
            origin: self.origin.clone(),
        });
        Ok(())
    }
}

// We need to convert AstType::TypeParameter into AstType::Generic when creating
// accessor/constructors.
//
// TODO: Do we also need to do this on the `tp` of other `AstType` variants?
fn type_gen_func_convert_type(trt: Tr<AstType>) -> Tr<AstType> {
    trt.map(|t| match t {
        AstType::TypeParameter(_, gid, tp) => AstType::Generic(
            gid,
            tp.into_iter().map(type_gen_func_convert_type).collect(),
        ),
        other => other,
    })
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl fmt::Display for DefinedTypeBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Struct(s) => s.fmt(f),
            Self::Enum(e) => e.fmt(f),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {} ({} -> {})",
            "fn".purple(),
            self.location.to_string().green(),
            self.header.pnames.iter().format(" "),
            self.header.ptypes.iter().format(" "),
            self.header.returns,
        )?;
        if let FunctionKind::Normal(body) = &self.kind {
            write!(f, " {}", body)?;
        }
        Ok(())
    }
}

impl fmt::Display for DefinedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} => {}",
            "type".purple(),
            self.location.to_string().green(),
            self.body,
        )
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use lumina_parser::{Attr, Struct, Trait, TypeKind};
    use lumina_util::Ign;

    pub fn init(storage: &mut Storage) -> Collector<'_> {
        let origin = Rc::new(Location::from_str("mod:test").unwrap());
        Collector::new(Rc::new(Environment::test_env()), origin, storage)
    }

    pub const SPAN: Span = 5..6;

    pub fn simple_test_function(name: &str) -> ast::Function {
        let param = |name| {
            Tr::new(ast::Entity::Identifier {
                takes: Location::from_str(name).unwrap(),
                params: vec![],
            })
        };

        ast::Function {
            header: ast::function::Header {
                pnames: vec![pn('x'), pn('y')],
                ptypes: vec![Tr::tr(SPAN, AstType::Int), Tr::new(AstType::Int)],
                returns: Tr::tr(SPAN, AstType::Int),
                bounds: ast::Bounds::new(),
                pflags: PFlags::new(),
                attributes: vec![Attr::Linux],
            },

            body: ast::function::Body {
                wheres: vec![],
                entity: Tr::tr(
                    SPAN,
                    ast::Entity::Identifier {
                        takes: Location::from_str(name).unwrap(),
                        params: vec![param("x"), param("y")],
                    },
                ),
            },
            name: String::from(name),
        }
    }

    pub fn simple_test_type(name: &str) -> DefType {
        complex_test_type(name, vec![])
    }

    pub fn complex_test_type(name: &str, tp: Vec<Tr<u8>>) -> DefType {
        DefType {
            name: String::from(name),
            span: Ign::new(SPAN),
            attributes: vec![Attr::Linux],
            type_parameters: tp,
            kind: TypeKind::Struct(simple_test_struct(format!("{}-field", name))),
        }
    }

    fn pn(c: char) -> Tr<String> {
        Tr::tr(SPAN, String::from(c))
    }

    pub fn complex_test_trait(name: &str, tp: Vec<Tr<u8>>) -> DefType {
        DefType {
            name: String::from(name),
            span: Ign::new(SPAN),
            attributes: vec![Attr::Linux],
            type_parameters: tp,
            kind: TypeKind::Trait(Trait {
                functions: vec![(
                    String::from("trait-func"),
                    (
                        Tr::tr(
                            SPAN,
                            ast::function::Header {
                                pnames: vec![pn('x'), pn('y')],
                                ptypes: vec![Tr::tr(SPAN, AstType::Int), Tr::new(AstType::Int)],
                                returns: Tr::tr(SPAN, AstType::Int),
                                bounds: ast::Bounds::new(),
                                attributes: vec![Attr::Linux],
                                pflags: PFlags::new(),
                            },
                        ),
                        0,
                    ),
                )]
                .into_iter()
                .collect(),
            }),
        }
    }
    pub fn simple_test_struct(fname: String) -> Struct {
        Struct {
            fields: vec![(Tr::tr(SPAN, fname), Tr::tr(SPAN, AstType::Int))],
        }
    }

    pub fn simple_test_trait(name: &str) -> DefType {
        DefType {
            name: String::from(name),
            span: Ign::new(SPAN),
            attributes: vec![Attr::Linux],
            type_parameters: vec![],
            kind: TypeKind::Trait(simple_test_trait_()),
        }
    }
    pub fn simple_test_trait_() -> Trait {
        Trait {
            functions: vec![(
                "map".to_string(),
                (
                    Tr::tr(
                        SPAN,
                        ast::function::Header {
                            pnames: vec![pn('f'), pn('x')],
                            ptypes: vec![Tr::tr(SPAN, AstType::Int), Tr::tr(SPAN, AstType::Int)],
                            bounds: ast::Bounds::new(),
                            returns: Tr::tr(SPAN, AstType::Int),
                            attributes: vec![Attr::Linux],
                            pflags: PFlags::new(),
                        },
                    ),
                    0,
                ),
            )]
            .into_iter()
            .collect::<HashMap<_, _>>(),
        }
    }

    #[test]
    fn function() {
        let mut storage = Storage::new();
        let mut coll = init(&mut storage);
        let origin = coll.origin.clone();

        let f = simple_test_function("foo");
        let floc = Rc::new(Location::from_str("mod:test:foo").unwrap());

        coll.on_func(Tr::tr(SPAN, f.clone())).unwrap();

        let lookup = storage.functions.get_from_key(&floc);

        let expected = Function {
            span: SPAN,
            location: floc,
            module: origin,
            header: f.header,
            kind: FunctionKind::Normal(f.body),
        };

        assert_eq!(lookup, Some(&expected));
    }

    #[test]
    fn type_() {
        let mut storage = Storage::new();
        let mut coll = init(&mut storage);
        let origin = coll.origin.clone();

        let t = simple_test_type("bar");
        let tloc = Rc::new(Location::from_str("mod:test:bar").unwrap());

        coll.on_type(t).unwrap();

        let lookup = storage.types.get_from_key(&tloc);

        let expected = DefinedType {
            span: SPAN,
            location: tloc,
            module: origin,

            type_params: vec![],
            body: DefinedTypeBody::Struct(simple_test_struct("bar-field".into())),
        };

        assert_eq!(lookup, Some(&expected));
    }

    #[test]
    fn trait_() {
        let mut storage = Storage::new();
        let mut coll = init(&mut storage);
        let origin = coll.origin.clone();

        let t = simple_test_trait("Functor");
        let tloc = Rc::new(Location::from_str("mod:test:Functor").unwrap());

        coll.on_type(t).unwrap();

        let lookup = storage.traits.get_from_key(&tloc);

        let expected = DefinedTrait {
            span: SPAN,
            location: tloc,
            module: origin,

            type_params: vec![],
            body: simple_test_trait_(),
        };

        assert_eq!(lookup, Some(&expected));
    }
}
