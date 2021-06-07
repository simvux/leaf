use super::*;

pub struct Builder<'ast> {
    base: &'ast collector::DefinedType,
    struct_: &'ast lumina_parser::Struct,

    fields: FieldsBuilder,
}

impl<'ast> UniqueHandler for &mut Builder<'ast> {
    type Root = hir::Type;
    type Tree = Tp<hir::Type>;

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree {
        Tp::new(root, params)
    }
}

type Field = Tr<Tp<hir::Type>>;

type FieldsBuilder = EntryBuilder<Field>;

impl FieldsBuilder {
    fn into_kind(self) -> hir::UserTypeKind {
        match self {
            Self::Ok(fields) => hir::UserTypeKind::Struct(fields),
            Self::Poisoned(fields) => hir::UserTypeKind::PoisonedStruct(fields),
        }
    }
}

impl<'ast> Builder<'ast> {
    pub fn new(base: &'ast collector::DefinedType, struct_: &'ast lumina_parser::Struct) -> Self {
        Self {
            base,
            fields: FieldsBuilder::Ok(Vec::with_capacity(struct_.fields.len())),
            struct_,
        }
    }
}

/// State for the verification of a struct
pub struct Pass<'ast, 'hir> {
    verifier: Verifier<'ast, 'hir>,
    current: Builder<'ast>,
}

impl<'ast, 'hir> Pass<'ast, 'hir> {
    pub fn new(verifier: Verifier<'ast, 'hir>, current: Builder<'ast>) -> Self {
        Self { verifier, current }
    }

    pub fn run(mut self) -> hir::UserType {
        for (_name, type_) in self.current.struct_.fields.iter() {
            match self
                .verify_type(&**type_)
                .map(|t| Tr::tr(type_.span.clone(), t))
            {
                Ok(ok) => self.current.fields.push(ok),
                Err(e) => {
                    let luminerr = self.verifier.hackily_load_error(
                        &self.current.base.location,
                        e.position(type_.span.clone()),
                    );
                    self.verifier.errors.push(luminerr);
                    self.current.fields.push_fail()
                }
            }
        }

        hir::UserType {
            span: self.current.base.span.clone(),
            location: self.current.base.location.clone(),
            kind: self.current.fields.into_kind(),
            type_params: self.current.base.type_params.clone(),
        }
    }

    fn verify_type(&mut self, t: &AstType) -> Result<Tp<hir::Type>, TypeVerificationError> {
        let verifier = self.verifier.copy();
        let mut tver = TypeVerifier::new(verifier, &mut self.current);
        tver.verify_type(t)
    }
}
