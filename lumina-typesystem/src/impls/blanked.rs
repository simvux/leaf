use super::{
    Error, Generic, LookupError, Scorer, Tp, Tr, TraitImpls, TraitVariants, Type, TypeParameters,
    TypeParams,
};

#[derive(Debug, Default)]
pub struct Impls<I> {
    implementors: TraitVariants<Vec<(Tr<Tp<Generic>>, I)>>,
}

impl<I: Default> Impls<I> {
    pub fn implement(
        &mut self,
        traittp: TypeParameters,
        implementor: Tr<Generic>,
        implementor_tp: TypeParameters,
        implementation: I,
    ) -> Result<(), Error<I>> {
        let trait_variant = self.implementors.entry(traittp).or_insert_with(Vec::new);

        trait_variant.push((
            // Tr::tr(implementor.span, Tp::new(super::Type::Generic(implementor.root), implementor.params)),
            implementor.map(|root| Tp::new(root, implementor_tp)),
            implementation,
        ));

        Ok(())
    }
}

impl<I: Default + Clone + std::fmt::Debug> TraitImpls<I> {
    pub fn lookup_blanked<'i, 'g>(
        &self,
        scorer: &mut Scorer<'i, 'g, I>,
        impls: &'i Impls<I>,
        impltor: Tr<&super::Type>,
        impltor_tp: &TypeParams,
        trtp: &TypeParams,
    ) -> Result<&'i I, LookupError<I>> {
        // Get all trait variants compatible with our `trtp`
        let mut compatible_trait_variants = scorer.compatible_variants(&impls.implementors, trtp);
        // And sort them by specialization
        compatible_trait_variants.sort_by(|y, x| x.0.cmp(&y.0));

        for (_spec, mut variant_genbuffer, tvar) in compatible_trait_variants.into_iter() {
            dbg!(&variant_genbuffer);
            // Check if any of the implementations in this trait variant are compatible with our
            // given implementor.
            //
            // If multiple implementations are compatible, it picks the greatest by specialization
            if let Some(hit) = tvar
                .iter()
                .enumerate()
                .filter_map(|(i, (ttp, imp))| {
                    Scorer {
                        generics: &mut variant_genbuffer,
                        impls: scorer.impls,
                    }
                    .ttp_by_specialization(
                        Tr::tr(ttp.span.clone(), &Type::Generic(ttp.root.clone())),
                        &ttp.params,
                        impltor.clone(),
                        &impltor_tp,
                    )
                    .map(|s| (i, s, imp))
                })
                .max_by(|(_, aspec, _), (_, bspec, _)| aspec.cmp(bspec))
            {
                // We overwrite the active genbuffer to the highest ranked implementation of this
                // trait variant.
                *scorer.generics = variant_genbuffer;
                return Ok(hit.2);
            }
        }

        // TODO: This error might be ignored in the callstack. Meaning that we definitely shouldn't
        // allocate with it's construction.
        Err(LookupError::NoBlankedImpl(Tp::new(
            impltor.inner.clone(),
            impltor_tp.to_vec(),
        )))
    }
}
