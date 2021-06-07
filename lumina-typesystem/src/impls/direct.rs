use super::{
    Direct, Error, HashMap, ImplementorVariants, LookupError, Scorer, Tp, TraitImpls,
    TraitVariants, TypeParameters, TypeParams,
};

#[derive(Debug, Default)]
pub struct Impls<I> {
    types: HashMap<Direct, Variants<I>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Variants<I> {
    trait_variants: TraitVariants<ImplementorVariants<I>>,
}

impl<I: Default + Clone> Impls<I> {
    pub fn implement(
        &mut self,
        traittp: TypeParameters,
        implementor: Tp<Direct>,
        implementation: I,
    ) -> Result<(), Error<I>> {
        let direct_variants = self
            .types
            .entry(implementor.root)
            .or_insert_with(Variants::default);

        let implementor_variants = direct_variants
            .trait_variants
            .entry(traittp)
            .or_insert_with(super::Variants::default);

        if let Some(existing) = implementor_variants.insert(implementor.params, implementation) {
            return Err(Error::AlreadyExists { existing });
        }

        Ok(())
    }
}

impl<I: Default + Clone + std::fmt::Debug> TraitImpls<I> {
    pub fn lookup_direct<'i, 'g>(
        &self,
        scorer: &mut Scorer<'i, 'g, I>,
        impls: &'i Impls<I>,
        dt: &Direct,
        impltortp: &TypeParams,
        trtp: &TypeParams,
    ) -> Result<&'i I, LookupError<I>> {
        let tv = impls
            .types
            .get(dt)
            .ok_or_else(|| LookupError::NotImplemented(super::Type::Direct(dt.clone())))?;

        let (impl_variants, _) =
            scorer
                .select_variant(&tv.trait_variants, trtp)
                .ok_or_else(|| LookupError::NoTVariantImplsForDTMatch {
                    existing_variants: tv.clone(),
                    given_trtp: trtp.to_vec(),
                })?;

        scorer
            .select_variant(impl_variants, impltortp)
            .ok_or_else(
                || LookupError::MismatchedTypeParameterForImplementorOntoTRTP {
                    impltor_impls_for_trait_variant: impl_variants.clone(),
                    missing_impltortp: impltortp.to_vec(),
                    impltor: dt.clone(),
                },
            )
            .map(|(impls, _)| impls)
    }
}
