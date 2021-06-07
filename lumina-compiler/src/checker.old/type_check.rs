use super::*;
use lumina_typesystem::{Generic, TraitImpls};

// TODO: Move this to `checker` instead.

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    NotImplemented(Tr<Tp<Type>>, usize),
}

pub struct TypeCheck<'a, 'hir, I> {
    genbuffer: &'a mut GenBuffer,
    impls: &'hir TraitImpls<I>,
}

impl<'a, 'hir, I: Clone + Default + std::fmt::Debug> TypeCheck<'a, 'hir, I> {
    pub fn new(genbuffer: &'a mut GenBuffer, impls: &'hir TraitImpls<I>) -> Self {
        Self { genbuffer, impls }
    }

    pub fn type_check(&mut self, exp: Tr<&Tp<Type>>, got: Tr<&Tp<Type>>) -> Result<(), TypeError> {
        match &exp.root {
            // non-hkt generic
            Type::Generic(Generic::Bound(gid, bounds)) if exp.params.is_empty() => {
                for bound in bounds.iter() {
                    self.impls
                        .lookup(
                            bound.root,
                            &bound.params,
                            Tr::tr(got.span.clone(), &got.root),
                            &got.params,
                            self.genbuffer,
                        )
                        // TODO: We're discarding *a lot* of specific information in this map. This
                        // is temporary
                        .map_err(|_| {
                            TypeError::NotImplemented(got.clone().map(Clone::clone), bound.root)
                        })?;
                }

                // TODO: we're forgetting to actually check if it's already been assigned no?
                self.genbuffer.assign(**gid, got.map(Clone::clone));

                Ok(())
            }

            Type::Generic(Generic::Unbound(gid)) if exp.params.is_empty() => {
                // TODO: we're forgetting to actually check if it's already been assigned no?
                self.genbuffer.assign(**gid, got.map(Clone::clone));

                Ok(())
            }

            // hkt generic
            Type::Generic(gen) => {
                match gen {
                    // TODO: We've forgotten to normalize generics on some `a` generic somewhere.
                    //
                    // Probably when assigning unused
                    //
                    // Might be okay though.
                    Generic::Bound(gid, _) | Generic::Unbound(gid) => {
                        println!("gid: {}", (**gid + b'a') as char)
                    }
                }
                dbg!(&gen, &got);
                todo!();
            }

            Type::Direct(dt) if got.root == Type::Direct(dt.clone()) => exp
                .params
                .iter()
                .zip(got.params.iter())
                .try_for_each(|(e, g)| self.type_check(e.as_ref(), g.as_ref())),

            _ => panic!("ET: {} != {}", exp, got),
        }
    }
}

impl<'hir, 'held> CheckerPass<'hir, 'held> {
    pub fn type_check(
        &mut self,
        genbuffer: &mut GenBuffer,
        exp: Tr<&Tp<Type>>,
        got: Tr<&Tp<Type>>,
    ) -> Result<(), Error> {
        TypeCheck {
            genbuffer,
            impls: &self.verified.impls,
        }
        .type_check(exp, got)
        .map_err(Error::TypeError)
    }
}
