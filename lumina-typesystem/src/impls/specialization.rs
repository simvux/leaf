use super::{Bound, GenID, Generic, Tp, TraitImpls, Type, TypeParams, Variants};
use crate::GenBuffer;
use itertools::Itertools;
use lumina_util::debug;
use lumina_util::Tr;
use std::cmp::{Ord, Ordering};

// When deciding which variant to use, we want as many *directs* as possible, and fall back to
// whichever has the most *indirects*.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Default)]
pub struct Specialization {
    directs: usize,
    indirects: usize,
}

impl Specialization {
    pub fn direct() -> Self {
        Self {
            directs: 1,
            indirects: 0,
        }
    }

    pub fn indirect(bounds: usize) -> Self {
        Self {
            directs: 0,
            indirects: bounds,
        }
    }
}

impl std::ops::Add for Specialization {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            directs: self.directs + other.directs,
            indirects: self.indirects + other.indirects,
        }
    }
}

impl PartialOrd for Specialization {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.directs.cmp(&other.directs) {
            Ordering::Less => Some(Ordering::Less),
            Ordering::Greater => Some(Ordering::Greater),
            Ordering::Equal => self.indirects.partial_cmp(&other.indirects),
        }
    }
}
impl Ord for Specialization {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub struct Scorer<'i, 'g, I> {
    pub(crate) impls: &'i TraitImpls<I>,
    pub(crate) generics: &'g mut GenBuffer,
}

impl<'i, 'g, I: Clone + Default + std::fmt::Debug> Scorer<'i, 'g, I> {
    // Verifies that the stored generic matches the secondary given value
    // Or if this value isn't secondary; assign the generic.
    //
    // NOTE: Also handles HKT
    fn check_generic_or_new(
        &mut self,
        gid: GenID,
        exp_tp: &TypeParams,
        bounds: &[Bound],
        got: Tr<&Type>,
        got_tp: &TypeParams,
    ) -> Option<Specialization> {
        match self.generics.lookup(gid.inner) {
            // simple generic check
            Some(t) if exp_tp.is_empty() && t.root == **got && t.params == got_tp => {
                Some(Specialization::direct())
            }

            // if the stored generic is an higher-kinded type
            Some(t) if !exp_tp.is_empty() && t.params.is_empty() && t.root == **got => self
                .tp_by_specialization(exp_tp, got_tp)
                .map(|spec| spec + Specialization::direct()),
            Some(_) if !exp_tp.is_empty() => panic!("hkt mixed?"),

            // fallback to miss
            Some(_) => None,

            // if there isn't a generic assigned to this GID.
            None => {
                // is it a higher kinded type?
                if !exp_tp.is_empty() {
                    // let's first make a sanity check whether it has the same amount of tp's
                    // as the template
                    if exp_tp.len() != got_tp.len() {
                        return None;
                    }

                    // And then if it does have the same amount, make a recursive call to first
                    // check if all the type-parameters are compatible, and if they are, assign the
                    // generic and add the specializations
                    let spec = self.tp_by_specialization(exp_tp, got_tp).and_then(|spec| {
                        self.new_generic(gid, bounds.to_vec(), got, got_tp, false)
                            .map(|s| s + spec)
                    })?;
                    debug!("typechecker", "found match: {:#?}", spec);
                    Some(spec)
                } else {
                    self.new_generic(gid, bounds.to_vec(), got, got_tp, true)
                }
            }
        }
    }

    // Assigns a new generic upon verifying type-parameters
    fn new_generic(
        &mut self,
        gid: GenID,
        bounds: Vec<Bound>,
        got: Tr<&Type>,
        got_tp: &TypeParams,
        keep_tp_in_generic: bool,
    ) -> Option<Specialization> {
        let verify = |tridtp: &Bound| {
            debug!(
                "TEMP",
                "testing if ({} {:?}) satisfies the bound {}", got, got_tp, tridtp
            );

            self.impls
                .lookup(
                    tridtp.root,
                    &tridtp.params,
                    got.clone(),
                    got_tp,
                    self.generics,
                )
                .is_ok()
        };

        if bounds.iter().all(verify) {
            // TODO: This is probably wrong.
            let assigned_tps = if keep_tp_in_generic {
                got_tp.to_vec()
            } else {
                vec![]
            };
            self.generics.assign(
                gid.inner,
                Tr::tr(got.span, Tp::new(got.inner.clone(), assigned_tps)),
            );
            Some(Specialization::indirect(1 + bounds.len()))
        } else {
            None
        }
    }

    /// Verifies whether this works as an valid implementation, and if it does, gives it a
    /// specialization score.
    pub fn ttp_by_specialization(
        &mut self,
        exp_root: Tr<&Type>,
        exp_tp: &TypeParams,
        got_root: Tr<&Type>,
        got_tp: &TypeParams,
    ) -> Option<Specialization> {
        debug!(
            "typechecker",
            "checking exp {} {} against got {} {}",
            &exp_root,
            exp_tp.iter().join(" "),
            &got_root,
            got_tp.iter().join(" ")
        );

        match (&exp_root.inner, &got_root.inner) {
            (Type::Direct(edt), Type::Direct(gdt)) if edt == gdt => {
                debug!("typechecker", "valid match, progressing to checkig TPS");
                self.tp_by_specialization(exp_tp, got_tp)
            }
            (Type::Generic(Generic::Unbound(gid)), _) => {
                self.check_generic_or_new(gid.clone(), exp_tp, &[], got_root, got_tp)
            }
            (Type::Generic(Generic::Bound(gid, bounds)), _) => {
                self.check_generic_or_new(gid.clone(), exp_tp, bounds, got_root, got_tp)
            }
            _ => None,
        }
    }

    fn tp_by_specialization(
        &mut self,
        exp_tp: &TypeParams,
        got_tp: &TypeParams,
    ) -> Option<Specialization> {
        self.all(exp_tp.iter(), got_tp.iter())
    }

    fn all<'a, S: Iterator<Item = &'a Tr<Tp<Type>>>>(
        &mut self,
        exp: S,
        got: S,
    ) -> Option<Specialization> {
        let mut score = Specialization::default();

        for (e, g) in exp.zip(got) {
            score = score
                + self.ttp_by_specialization(
                    Tr::tr(e.span.clone(), &e.root),
                    &e.params,
                    Tr::tr(g.span.clone(), &g.root),
                    &g.params,
                )?
        }

        Some(score)
    }

    /// Searches through all variants of a trait to find the one (if exists) with the highest
    /// specialization score.
    pub fn select_variant<OF: std::fmt::Debug>(
        &mut self,
        variants: &'i Variants<OF>,
        tp: &TypeParams,
    ) -> Option<(&'i OF, Specialization)> {
        variants
            .inner
            .iter()
            .map(|(k, v)| {
                // We can't reuse genbuffers accross multiple *different* variant checks. Since if
                // they use the same GID's then it'll be corupt.
                let mut genbuffer = self.generics.clone();

                Scorer {
                    impls: self.impls,
                    generics: &mut genbuffer,
                }
                .all(k.iter(), tp.iter())
                .map(|spec| {
                    // On the variant we select, we want to go with those generics.
                    *self.generics = genbuffer;
                    (spec, k, v)
                })
            })
            .flatten()
            .max_by(|x, y| x.0.cmp(&y.0))
            .map(|(spec, _, v)| (v, spec))
    }

    /// Searches through all variants and returns those which are a combination with `tp`
    pub fn compatible_variants<OF: std::fmt::Debug>(
        &mut self,
        variants: &'i Variants<OF>,
        tp: &TypeParams,
    ) -> Vec<(Specialization, GenBuffer, &'i OF)> {
        variants
            .inner
            .iter()
            .map(|(k, v)| {
                let mut genbuffer = self.generics.clone();

                Scorer {
                    impls: self.impls,
                    generics: &mut genbuffer,
                }
                .all(k.iter(), tp.iter())
                .map(|spec| (spec, genbuffer, v))
            })
            .flatten()
            .collect()
    }
}

impl<'i, 'g, I> Scorer<'i, 'g, I> {
    pub fn new(impls: &'i TraitImpls<I>, generics: &'g mut GenBuffer) -> Self {
        Self { generics, impls }
    }
}
