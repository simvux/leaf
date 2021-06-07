use super::*;
use lumina_util::debug;
#[cfg(test)]
mod tests;

mod specialization;
pub use specialization::{Scorer, Specialization};
pub mod blanked;
pub mod direct;

#[derive(Debug)]
pub struct TraitImpls<I> {
    traits: HashMap<TRID, (impls::direct::Impls<I>, impls::blanked::Impls<I>)>,
}

pub type ImplementorVariants<I> = Variants<I>;

pub type TraitVariants<T> = Variants<T>;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Variants<OF> {
    inner: HashMap<TypeParameters, OF>,
}

impl<OF> Deref for Variants<OF> {
    type Target = HashMap<TypeParameters, OF>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<OF> DerefMut for Variants<OF> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<I> Default for TraitImpls<I> {
    fn default() -> Self {
        Self {
            traits: HashMap::with_capacity(20),
        }
    }
}

impl<I> TraitImpls<I> {
    pub fn new() -> Self {
        TraitImpls::default()
    }
}

impl<I: Default + Clone + std::fmt::Debug> TraitImpls<I> {
    pub fn implement(
        &mut self,
        trait_: Tr<Tp<TRID>>,
        impltor: Tr<Tp<Type>>,
        implementation: I,
    ) -> Result<(), Error<I>> {
        let (direct, blanked) = self
            .traits
            .entry(trait_.inner.root)
            .or_insert_with(|| (direct::Impls::default(), blanked::Impls::default()));

        let iparams = impltor.inner.params;

        match impltor.inner.root {
            Type::Direct(dt) => {
                direct.implement(trait_.inner.params, Tp::new(dt, iparams), implementation)
            }
            Type::Generic(gt) => blanked.implement(
                trait_.inner.params,
                Tr::tr(impltor.span, gt),
                iparams,
                implementation,
            ),
        }
    }

    pub fn lookup<'i>(
        &'i self,
        tid: TRID,
        trtp: &'i TypeParams,
        impltor: Tr<&Type>,
        impltor_tp: &TypeParams,
        generics: &mut GenBuffer,
    ) -> Result<&I, Error<I>> {
        let impls = self.traits.get(&tid).ok_or(Error::NoImplementations(tid))?;

        let mut scorer = Scorer::new(self, generics);

        match &impltor.inner {
            Type::Direct(dt) => {
                match self.lookup_direct(&mut scorer, &impls.0, dt, impltor_tp, trtp) {
                    Ok(v) => Ok(v),
                    Err(_) => {
                        debug!(
                            "typechecker",
                            "`{} {} for {} {}`: direct implementation not found, trying blanked",
                            tid,
                            trtp.iter().format(" "),
                            impltor,
                            impltor_tp.iter().format(" ")
                        );

                        self.lookup_blanked(&mut scorer, &impls.1, impltor, impltor_tp, trtp)
                            .map_err(|e| Error::context(e, tid))
                    }
                }
            }
            Type::Generic(_) => self
                .lookup_blanked(&mut scorer, &impls.1, impltor, impltor_tp, trtp)
                .map_err(|e| Error::context(e, tid)),
        }
    }

    pub fn clean_lookup_direct<'i>(
        &'i self,
        tid: TRID,
        trtp: &TypeParams,
        impltor: Tr<&Direct>,
        impltor_tp: &TypeParams,
        generics: &mut GenBuffer,
    ) -> Result<&I, Error<I>> {
        let impls = self.traits.get(&tid).ok_or(Error::NoImplementations(tid))?;

        let mut scorer = Scorer::new(self, generics);

        self.lookup_direct(&mut scorer, &impls.0, &*impltor, impltor_tp, trtp)
            .map_err(|e| Error::context(e, tid))
    }
}
