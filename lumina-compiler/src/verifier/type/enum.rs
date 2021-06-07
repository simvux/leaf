use super::*;

pub struct Builder<'ast> {
    base: &'ast collector::DefinedType,
    enum_: &'ast lumina_parser::Enum,

    variants: VariantsBuilder,
}

impl<'ast> UniqueHandler for &mut Builder<'ast> {
    type Root = hir::Type;
    type Tree = Tp<hir::Type>;

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree {
        Tp::new(root, params)
    }
}

type Variant = Vec<Tr<Tp<hir::Type>>>;

type VariantsBuilder = EntryBuilder<Variant>;

impl VariantsBuilder {
    fn into_kind(self) -> hir::UserTypeKind {
        match self {
            Self::Ok(variants) => hir::UserTypeKind::Enum(variants),
            Self::Poisoned(variants) => hir::UserTypeKind::PoisonedEnum(variants),
        }
    }
}

impl<'ast> Builder<'ast> {
    pub fn new(base: &'ast collector::DefinedType, enum_: &'ast lumina_parser::Enum) -> Self {
        Self {
            base,
            variants: VariantsBuilder::Ok(Vec::with_capacity(enum_.variants.len())),
            enum_,
        }
    }
}

/// State for the verification of an enum
pub struct Pass<'ast, 'hir> {
    verifier: Verifier<'ast, 'hir>,
    current: Builder<'ast>,
}

impl<'ast, 'hir> Pass<'ast, 'hir> {
    pub fn new(verifier: Verifier<'ast, 'hir>, current: Builder<'ast>) -> Self {
        Self { verifier, current }
    }

    pub fn run(mut self) -> hir::UserType {
        for (_name, types) in self.current.enum_.variants.iter() {
            match types
                .iter()
                .map(|t| {
                    match self
                        .verify_type(t)
                        .map_err(|e| e.position(t.span.clone()))
                        .map(|r| Tr::tr(t.span.clone(), r))
                    {
                        Ok(t) => Some(t),
                        Err(e) => {
                            let luminerr = self.verifier.hackily_load_error(
                                &self.current.base.location,
                                e.position(t.span.clone()),
                            );
                            self.verifier.errors.push(luminerr);
                            None
                        }
                    }
                })
                .collect::<Option<Vec<Tr<Tp<hir::Type>>>>>()
            {
                Some(variant) => self.current.variants.push(variant),
                None => self.current.variants.push_fail(),
            }
        }

        hir::UserType {
            span: self.current.base.span.clone(),
            location: self.current.base.location.clone(),
            kind: self.current.variants.into_kind(),
            type_params: self.current.base.type_params.clone(),
        }
    }

    fn verify_type(&mut self, t: &AstType) -> Result<Tp<hir::Type>, TypeVerificationError> {
        let verifier = self.verifier.copy();
        let mut tver = TypeVerifier::new(verifier, &mut self.current);
        tver.verify_type(t)
    }
}
