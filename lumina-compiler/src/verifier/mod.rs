use super::{callstack::FCall, collector, hir, CallStack};
use lumina_parser::ast;
use lumina_typesystem::{GenBuffer, ITp, Tp, TraitImpls, Typed};
use lumina_util::{debug, Location, ToError, Tr};
use std::collections::HashMap;
use std::rc::Rc;

mod settings;
pub use settings::Settings;

mod error;
pub use error::Error;

pub mod function;
use function::StackEntry;
pub mod r#type;

pub mod r#impl;

pub type FID = usize;

mod r#trait;

enum EntryBuilder<E> {
    Ok(Vec<E>),
    Poisoned(Vec<Option<E>>),
}

impl<E> EntryBuilder<E> {
    fn push(&mut self, entity: E) {
        match self {
            Self::Ok(entities) => entities.push(entity),
            Self::Poisoned(entities) => entities.push(Some(entity)),
        }
    }

    fn push_fail(&mut self) {
        take_mut::take(self, |with| match with {
            Self::Ok(entities) => {
                let mut entities = entities.into_iter().map(Some).collect::<Vec<_>>();
                entities.push(None);
                Self::Poisoned(entities)
            }
            Self::Poisoned(mut entities) => {
                entities.push(None);
                Self::Poisoned(entities)
            }
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FKey {
    Function(usize),
    Lambda { parent: FID, instance: usize },
    Let { parent: FID, instance: usize },
    Where { parent: FID, instance: usize },
}

pub struct Verifier<'ast, 'hir> {
    env: Rc<lumina_env::Environment>,
    settings: Settings,

    ast: &'ast collector::Storage,
    hir: &'hir mut Storage,

    errors: &'hir mut Vec<lumina_util::Error>,
}

pub fn init_and_verify(
    env: Rc<lumina_env::Environment>,
    settings: Settings,
    ast: &collector::Storage,
) -> (Vec<lumina_util::Error>, Storage) {
    let mut hir_storage = Storage {
        functions: HashMap::with_capacity(ast.functions.len()),
        types: HashMap::with_capacity(ast.types.len()),
        read_only: Vec::new(),
        impls: TraitImpls::new(),
        traits: HashMap::new(),
    };
    let mut errors = Vec::new();

    Verifier::new(env, settings, ast, &mut hir_storage, &mut errors).run();

    (errors, hir_storage)
}

pub struct Storage {
    pub functions: HashMap<FKey, Rc<hir::Function>>,
    pub read_only: Vec<ReadOnly>,
    pub types: HashMap<usize, hir::UserType>,
    pub traits: HashMap<usize, hir::UserTrait>,
    pub impls: TraitImpls<()>,
}

pub enum ReadOnly {}

impl<'ast, 'hir> Verifier<'ast, 'hir> {
    pub fn new(
        env: Rc<lumina_env::Environment>,
        settings: Settings,
        ast: &'ast collector::Storage,
        hir: &'hir mut Storage,
        errors: &'hir mut Vec<lumina_util::Error>,
    ) -> Self {
        Self {
            env,
            settings,
            ast,
            hir,
            errors,
        }
    }

    /// in the future we're gonna keep the source code of files around and then refer to them in
    /// the lumina_parser directly through lifetime'd strings. However until then; we simply
    /// re-open the source file to generate the error message.
    pub fn hackily_load_error<E: ToError>(
        &self,
        loc: &lumina_util::Location,
        e: E,
    ) -> lumina_util::Error {
        let (src, path) = loc.read_source_code(&self.env).unwrap();
        e.into_lumina_error(&src, path.to_string_lossy().to_string())
    }

    pub fn copy(&mut self) -> Verifier<'ast, '_> {
        Verifier {
            env: self.env.clone(),
            settings: self.settings.clone(),

            ast: self.ast,
            hir: self.hir,
            errors: self.errors,
        }
    }

    // Verify all functions, types, traits and implementations.
    //
    // Results are stored in self.{errors,storage}
    pub fn run(mut self) {
        self.run_all_traits();
        self.run_all_types();
        self.run_all_implementations();
        self.run_all_functions();
    }

    // verify the types in the trait decleration and convert it to HIR.
    //
    // note that this is only the trait and it's headers, not any implementations.
    fn run_all_traits(&mut self) {
        // the result we want to create is an `HashMap<TRID, { methods: Vec<hir::function::Header> }>`
        let trids = 0..self.ast.traits.len();

        trids.for_each(|trid| {
            let trait_ = self.ast.traits.get_from_idx(trid).unwrap();
            self.include_trait(trid, trait_);
        })
    }

    pub fn include_trait(
        &mut self,
        trid: usize,
        trait_: &collector::DefinedTrait,
    ) -> &mut hir::UserTrait {
        let verifier = self.copy();

        let pass = r#trait::Pass::new(verifier, trait_);
        let verified = pass.run();

        self.hir.traits.entry(trid).or_insert(verified)
    }

    // we need to index implementations ahead of time.
    //
    // so; perhaps we should turn TraitImpls<Implementation> into TraitImpls<usize> so we can
    // post-pone checking the actual bodies of the implementations
    //
    // actually; we can use FKey::Implementation(usize) and then TraitImpls<usize> which should
    // work perfectly
    //
    // currently the functions are stored as
    // collector.implementations: Vec<ImplTemplate> where ImplTemplate: trait+methods
    //
    // so; I guess we want to pop off all the impltemplate from it and then compile the hir stuff.
    // create `lumina_typesystem::TraitImpls<IMPLID>`
    // after functions we can then compile all the methods and insert as `FKey`s onto `storage.functions`
    //
    // // TODO: Fuck; the collector is imutable so we can't pop. But; do we need to?
    // since we're not moving or compiling the actual bodies I think we'll be fine. We need to
    // convert the rest regardless.
    fn run_all_implementations(&mut self) {
        let implids = 0..self.ast.implementations.len();

        implids.for_each(|implid| {
            let impl_ = &self.ast.implementations[implid];
            self.index_impl(implid, impl_);
        })
    }

    fn run_all_types(&mut self) {
        let tids = 0..self.ast.types.len();

        tids.for_each(|tid| {
            let t = self.ast.types.get_from_idx(tid).unwrap();
            self.include_type(tid, t);
        })
    }

    fn include_type(&mut self, tid: usize, t: &'ast collector::DefinedType) -> &mut hir::UserType {
        match &t.body {
            collector::DefinedTypeBody::Struct(struct_) => self.include_struct(tid, t, struct_),
            collector::DefinedTypeBody::Enum(enum_) => self.include_enum(tid, t, enum_),
        }
    }

    fn include_struct(
        &mut self,
        tid: usize,
        t: &'ast collector::DefinedType,
        struct_: &'ast lumina_parser::Struct,
    ) -> &mut hir::UserType {
        let verifier = self.copy();

        let pass =
            r#type::r#struct::Pass::new(verifier, r#type::r#struct::Builder::new(t, struct_));
        let verified = pass.run();

        self.hir.types.entry(tid).or_insert(verified)
    }

    fn include_enum(
        &mut self,
        tid: usize,
        t: &'ast collector::DefinedType,
        enum_: &'ast lumina_parser::Enum,
    ) -> &mut hir::UserType {
        let verifier = self.copy();

        let pass = r#type::r#enum::Pass::new(verifier, r#type::r#enum::Builder::new(t, enum_));
        let verified = pass.run();

        self.hir.types.entry(tid).or_insert(verified)
    }

    fn run_all_functions(&mut self) {
        let fids = 0..self.ast.functions.len();

        fids.for_each(|fid| {
            let f = self.ast.functions.get_from_idx(fid).unwrap();
            if let collector::FunctionKind::Normal(body) = &f.kind {
                self.include_function(fid, f, body, &mut CallStack::new());
            }
        })
    }

    pub fn include_function<'a>(
        &mut self,
        fid: usize,
        f: &'ast collector::Function,
        body: &'ast ast::function::Body,
        callstack: &'a mut CallStack<FCall, StackEntry<'ast>>,
    ) -> Rc<hir::Function> {
        let verified = self.run_function_from_scratch(fid, f, body, callstack);

        self.hir
            .functions
            .entry(FKey::Function(fid))
            .or_insert_with(|| Rc::new(verified))
            .clone()
    }

    pub fn run_function_from_scratch<'a>(
        &mut self,
        fid: usize,
        f: &'ast collector::Function,
        body: &'ast ast::function::Body,
        callstack: &'a mut CallStack<FCall, StackEntry<'ast>>,
    ) -> hir::Function {
        debug!("verifier", "{} starting new function: {}", fid, f);

        // Attempt AST -> HIR conversion of type annotation to index types and verify their existance
        let (ptypes, returns, header_error) =
            r#type::TypeVerifier::new(self.copy(), FunctionTypingInit { current: f })
                .typing_from_ast(&f.header.ptypes, f.header.returns.as_ref());

        let typing = function::PendingTyping::new(ptypes, returns);

        // The type annotation of the function is invalid. So let's store err and poison.
        if !header_error.is_empty() {
            for err in header_error.into_iter() {
                let luminerr = self.hackily_load_error(&f.module, err);
                self.errors.push(luminerr);
            }

            // early-return with the poisoned function
            return hir::Function {
                span: f.span.clone(),
                header: hir::function::Header::Failed(typing),
                kind: hir::function::Kind::Poisoned,
            };
        }

        let pass = function::Pass::new(
            callstack,
            self.copy(),
            function::Builder::new(FCall::Function(fid), typing, &f),
            // TODO: For the function iterations (not when we recursively descent) we could reuse
            // the same genbuffer. However; currently we don't since it's dropped at the end of
            // this function.
            GenBuffer::new(),
        );

        let func = pass.run(body);

        if self.env.output.hir {
            println!("{}", func);
        }

        func
    }

    pub fn expand(&self, origin: &Location, loc: &Location) -> Location {
        let mut loc = loc.clone();
        self.expand_mut(origin, &mut loc);
        loc
    }

    pub fn expand_mut(&self, origin: &Location, loc: &mut Location) {
        origin.expand_mut(loc);
        self.ast.imports.expand_mut(loc);
    }
}

#[derive(Clone, Copy)]
struct FunctionTypingInit<'ast> {
    current: &'ast collector::Function,
}

pub trait ConstraintVisitor = FnMut(
    Verifier<'_, '_>,
    &Location,
    usize,
    &[Tr<lumina_parser::Type>],
) -> Result<Tp<usize>, r#type::TypeVerificationError>;

fn unique_handler_attach_bound(
    gid: u8,
    verifier: Verifier<'_, '_>,
    origin: &Location,
    bounds: &ast::Bounds<lumina_parser::Type>,
    f: impl ConstraintVisitor,
) -> Result<hir::Type, r#type::TypeVerificationError> {
    match bounds.get(&gid) {
        Some(bound) => unique_handler_verify_bound(verifier, origin, bound, f)
            .map(|hir_constraint| Typed::bound(gid, hir_constraint)),
        None => Ok(Typed::unbound(gid)),
    }
}

fn unique_handler_verify_bound(
    mut verifier: Verifier<'_, '_>,
    origin: &Location,
    constraints: &[ast::GenBound<lumina_parser::Type>],
    mut f: impl ConstraintVisitor,
) -> Result<Vec<Tp<usize>>, r#type::TypeVerificationError> {
    constraints
        .iter()
        .map(|(bloc, btp)| {
            let abs_bloc = verifier.expand(origin, bloc);
            let trid = verifier
                .ast
                .traits
                .resolve(&abs_bloc)
                .ok_or_else(|| r#type::TypeVerificationError::TraitNotFound(abs_bloc))?;

            f(verifier.copy(), origin, trid, btp)
        })
        .collect()
}

/*
fn unique_handler_verify_constraint<H: r#type::UniqueHandler>(
    verifier: Verifier<'_, '_>,
    handler: H,
    origin: &Location,
    bloc: &Location,
    btp: &[Tr<lumina_parser::Type>],
) -> Result<Tp<usize>, r#type::TypeVerificationError> {
    let abs_bloc = verifier.expand(origin, bloc);
    let trid = verifier
        .ast
        .traits
        .resolve(&abs_bloc)
        .ok_or_else(|| r#type::TypeVerificationError::TraitNotFound(abs_bloc))?;

    // let handler = todo!("we want to make a custom handler for the trait bounds I think?");
    //            ^ this handler would need to refuse any generics that aren't in the parent i think?
    //            let's check in rust/haskell
    //
    //            it doesn't. however; it does need to be able to access the set bound constraints
    //            i think we do want the handler to be passed as parameter

    let mut recursive_lookup = r#type::TypeVerifier::new(verifier, handler);
    let tp = recursive_lookup.verify_types(&btp)?;

    Ok(Tp::new(trid, tp))
}
*/

/*
fn unique_handler_verify_bound<
    'ast,
    'hir,
    V: r#type::UniqueHandler<Root = hir::Type, Tree = Tp<hir::Type>>,
    F,
>(
    mut verifier: Verifier<'ast, 'hir>,
    origin: &Location,
    bound: &[ast::GenBound<lumina_parser::Type>],
    mut rec: F,
) -> Result<Vec<Tp<usize>>, r#type::TypeVerificationError>
where
    F: FnMut() -> V,
{
    bound
        .iter()
        .map(|(bloc, btp)| {
            let abs_bloc = verifier.expand(origin, bloc);

            let traitid = verifier
                .ast
                .traits
                .resolve(&abs_bloc)
                .ok_or_else(|| r#type::TypeVerificationError::TraitNotFound(abs_bloc))?;

            let mut recursive_lookup = r#type::TypeVerifier::new(verifier.copy(), rec());
            let tp = recursive_lookup.verify_types(&btp)?;

            Ok(Tp::new(traitid, tp))
        })
        .collect::<Result<Vec<Tp<usize>>, r#type::TypeVerificationError>>()
}
*/

impl<'ast> r#type::UniqueHandler for FunctionTypingInit<'ast> {
    type Root = hir::IType;
    type Tree = ITp<hir::IType>;

    fn generic(
        &mut self,
        mut verifier: Verifier<'_, '_>,
        gid: u8,
        _: &lumina_parser::TypeParams,
    ) -> Result<hir::IType, r#type::TypeVerificationError> {
        unique_handler_attach_bound(
            gid,
            verifier.copy(),
            &self.current.module,
            &self.current.header.bounds,
            |ver: Verifier<'_, '_>,
             _origin: &'_ Location,
             trid: usize,
             btp: &'_ [Tr<lumina_parser::Type>]| {
                let mut rec = r#type::TypeVerifier::new(
                    ver,
                    FunctionTypingInit {
                        current: self.current,
                    },
                );

                let tp = rec.verify_types(&btp).and_then(|tp| {
                    tp.into_iter()
                        .map(|trt| trt.to_known().map(|t| Tr::tr(trt.span.clone(), t)))
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|span| {
                            let mut err = r#type::TypeVerificationError::ForbiddenInference;
                            if let Some(span) = span {
                                err = err.position(span);
                            }
                            err
                        })
                })?;

                Ok(Tp::new(trid, tp))
            },
        )
        .map(hir::IType::True)
        /*
        FunctionTypingInit {
            current: self.current,
        },
        */
    }

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree {
        ITp::new(root, params)
    }
}

trait PoisonableType {
    fn poison_fallback() -> Self;
}

impl PoisonableType for hir::IType {
    fn poison_fallback() -> Self {
        hir::IType::infer()
    }
}
impl PoisonableType for hir::Type {
    fn poison_fallback() -> Self {
        // TODO: will this cause issues? depends how we handle encountering poisoned implementations
        hir::Type::unit()
    }
}

impl<'ast, 'hir, R: Typed + PoisonableType, H: r#type::UniqueHandler<Root = R>>
    r#type::TypeVerifier<'ast, 'hir, H>
{
    // Verify the type annotation of a function
    fn typing_from_ast(
        mut self,
        ptypes: &'ast [Tr<lumina_parser::Type>],
        returns: Tr<&'ast lumina_parser::Type>,
    ) -> (
        Vec<Tr<H::Tree>>,
        Tr<H::Tree>,
        Vec<r#type::TypeVerificationError>,
    ) {
        let mut errors = Vec::new();

        let ptypes = ptypes
            .iter()
            .map(
                |t| match self.verify_type(t).map_err(|e| e.position(t.span.clone())) {
                    Ok(new_type) => Tr::tr(t.span.clone(), new_type),
                    Err(e) => {
                        errors.push(e);
                        let new_type = H::combinator(R::poison_fallback(), vec![]);
                        Tr::tr(t.span.clone(), new_type)
                    }
                },
            )
            .collect();

        let ret = match self.verify_type(&*returns) {
            Ok(t) => t,
            Err(e) => {
                errors.push(e.position(returns.span.clone()));
                H::combinator(R::poison_fallback(), vec![])
            }
        };

        (ptypes, Tr::tr(returns.span.clone(), ret), errors)
    }
}
