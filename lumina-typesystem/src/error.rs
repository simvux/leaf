use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Error<I> {
    AlreadyExists { existing: I },
    NoImplementations(TRID),

    NotImplemented(Type),

    TodoLookup(LookupError<I>),
}

impl<I> Error<I> {
    pub fn context(e: LookupError<I>, _tid: TRID) -> Self {
        Self::TodoLookup(e)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LookupError<I> {
    NotImplemented(Type),
    NoTVariantImplsForDTMatch {
        existing_variants: impls::direct::Variants<I>,
        given_trtp: TypeParameters,
    },
    MismatchedTypeParameterForImplementorOntoTRTP {
        impltor_impls_for_trait_variant: impls::ImplementorVariants<I>,
        missing_impltortp: TypeParameters,
        impltor: Direct,
    },
    NoBlankedImpl(Tp<Type>),

    Temp(I),
}
