use super::*;
use hir::{IType, Type};
use lumina_typesystem::{Generic::*, Typed};
use smallvec::{smallvec, SmallVec};

impl<'calls, 'ast, 'hir> Pass<'calls, 'ast, 'hir> {
    // when type-checking a parameter against it's expected value there's a lot more things
    // regarding inference we need to keep in mind. Both the current functions return type and the
    // parameter in question can be at an Infer state.
    pub fn type_check_parameter_with_inference(
        &mut self,
        pid: usize,
        params: &[Tr<ast::Entity>], // parameters given in the use of parameter `pid` (HOF)
        expected: Expect<'_>,
    ) -> Result<Vec<Tr<hir::Entity>>, Error> {
        let has_params = !params.is_empty();

        let typing = &mut self.current.as_mut().unwrap().typing;
        let t = &mut typing.ptypes[pid];

        match (&t.root, expected) {
            // parameter as function
            //
            // --
            // it's explicitely a function-param
            // we know the expected return type of the function-param
            (IType::True(t), Expect::Exact(exp)) if *t == Type::fn_pointer() => {
                todo!();
            }
            // it's explicitely a function-param
            // the return type should be the same as the current function's return type
            (IType::True(t), Expect::RootExpr) if *t == Type::fn_pointer() => {
                todo!();
            }
            // it has parameters therefore it should infer into a function-param
            // we know the expected return type of the function-param
            (IType::Infer, Expect::Exact(exp)) if has_params => {
                todo!();
            }
            // it has parameters therefore it should infer into a function-param
            // the return type should be the same as the current function's return type
            (IType::Infer, Expect::RootExpr) if has_params => {
                todo!();
            }

            // parameter as value
            //
            // --
            // we know what type it's supposed to be
            (_, Expect::Exact(exp)) => {
                assert!(!has_params);

                ParamCheckPass::new(&mut self.genbuffer, typing, pid)
                    .generic_comparison_against_child(exp.as_ref())?;

                Ok(no_params())
            }
            // the type should match the current function's return type
            (_, Expect::RootExpr) => {
                assert!(!has_params);
                bidirectional_type_inference(typing, pid)?;
                Ok(no_params())
            }
        }
    }

    pub fn type_check_value(
        &mut self,
        v: Tr<Tp<hir::Type>>,
        expect: Expect<'_>,
    ) -> Result<(), Error> {
        match expect {
            Expect::RootExpr => {
                let typing = &mut self.current.as_mut().unwrap().typing;
                match typing.returns.root {
                    // check `v` against `typing.returns`
                    IType::True(_) => comparison_as_root_expr(
                        &mut self.genbuffer,
                        v.as_ref(),
                        typing.returns.as_mut(),
                    ),

                    // overwrite the entire return type with `v`
                    IType::Infer if typing.returns.params.is_empty() => {
                        typing.returns.inner = v.inner.clone().into_unknown();
                        Ok(())
                    }

                    // infer typing.returns.root into v.root then check the tp
                    // (as well as infer return type's tp's if needed)
                    IType::Infer => {
                        if v.params.len() == typing.returns.params.len() {
                            todo!();
                        } else {
                            panic!("ET: Wrong amount of parameters");
                        }
                    }
                }
            }
            Expect::Exact(exp) => ValueCheckPass {
                genbuffer: &mut self.genbuffer,
            }
            .generic_comparison_against_child(v.as_ref(), exp.as_ref()),
        }
    }
}

fn expect_or_assign(
    genbuffer: &mut GenBuffer,
    gid: u8,
    t: Tr<&Tp<hir::Type>>,
) -> Result<(), Error> {
    genbuffer
        .expect_or_assign(gid, t.clone())
        .map_err(|expected| Error::GenericAssignedDifferently {
            gid,
            expected,
            got: t.cloned(),
        })
}

fn comparison_as_root_expr(
    genbuffer: &mut GenBuffer,
    got: Tr<&Tp<hir::Type>>,
    mut expecting: Tr<&mut ITp<hir::IType>>,
) -> Result<(), Error> {
    match (&got.root, &expecting.root) {
        (g, IType::Infer) => expecting.root = IType::True(g.clone()),
        // TODO: we need to manually handle generics here again. The question is; can we
        // encapsulate that behavior?
        //
        // ACTUALLY; No; since this is against *root*; i think we're just supposed to do direct
        // comparison
        (g, IType::True(exp)) => {
            if g != exp {
                return Err(Error::ReturnTypeMissmatch {
                    exp: expecting.cloned_mut(),
                    got: got.map(|t| t.clone().into_unknown()),
                });
            }
        }
    }

    comparison_as_root_expr_tp(genbuffer, &got.params, &mut expecting.params)
}

fn comparison_as_root_expr_tp(
    genbuffer: &mut GenBuffer,
    got: &[Tr<Tp<hir::Type>>],
    expecting: &mut [Tr<ITp<hir::IType>>],
) -> Result<(), Error> {
    assert_eq!(got.len(), expecting.len());
    got.iter()
        .zip(expecting.iter_mut())
        .try_for_each(|(g, e)| comparison_as_root_expr(genbuffer, g.as_ref(), e.as_mut()))
}

struct ValueCheckPass<'a> {
    genbuffer: &'a mut GenBuffer,
}

impl<'a> ValueCheckPass<'a> {
    fn generic_comparison_against_child(
        &mut self,
        giving: Tr<&Tp<hir::Type>>,
        expecting: Tr<&Tp<hir::Type>>,
    ) -> Result<(), Error> {
        match (&giving.root, &expecting.root) {
            // higher-kinded-type without constraint
            (_, Type::Generic(Unbound(gid))) if !expecting.params.is_empty() => {
                todo!("expect_or_assign the `got` against `git` but also handle hkt somehow (i think lumina_typesystem does that for us)");
            }
            // higher-kinded-type with constraint
            (got, Type::Generic(Bound(gid, bounds))) if !expecting.params.is_empty() => {
                todo!();
            }
            // non-hkt generics
            (got, Type::Generic(Unbound(gid))) => expect_or_assign(self.genbuffer, **gid, giving),
            (got, Type::Generic(Bound(gid, bounds))) => {
                // TODO:
                //
                // in order to verify whether `got` satisfies bounds we need to have access to lots of
                // state which we don't have.
                //
                // oh wait, we haven't even checked the implementations yet so there's nothing to query.
                //
                // I guess we'll have to use TraitImpls<ImplID> instead so we may index the
                // implementations before checking them.
                todo!("uh oh");
            }
            (got, exp) => todo!(),
        }

        // generic_comparison_against_child_tp(genbuffer, &giving.params, &expecting.params)
    }

    fn generic_comparison_against_child_tp(
        &mut self,
        giving: &[Tr<Tp<hir::Type>>],
        expecting: &[Tr<Tp<hir::Type>>],
    ) -> Result<(), Error> {
        assert_eq!(giving.len(), expecting.len());
        giving
            .iter()
            .zip(expecting.iter())
            .try_for_each(|(g, e)| self.generic_comparison_against_child(g.as_ref(), e.as_ref()))
    }
}

struct ParamCheckPass<'a> {
    genbuffer: &'a mut GenBuffer,
    current: &'a mut PendingTyping,
    pointer: Pointer,
}

// We need PendingTyping to generate `first_unused_generic` but we also need to walk the type of it's
// parameters mutably. So; mutably borrowing won't work since that can corrupt the reference we use
// for `first_unused_generic` (thanks borrow-checker)
//
// Therefore; we have this fairly ugly solution of storing indices to re-entry each time and borrow
// the entire PendingTyping mutably once.
//
// I might redo this with a more efficient unsafe version in the future.
struct Pointer {
    pid: usize,
    depth: SmallVec<[usize; 4]>,
}

impl Pointer {
    fn read<'a>(&self, current: &'a PendingTyping) -> Tr<&'a ITp<IType>> {
        let mut base = &current.ptypes[self.pid];
        for ppid in &self.depth {
            base = &base.params[*ppid];
        }

        base.as_ref()
    }

    fn read_mut<'a>(&self, current: &'a mut PendingTyping) -> Tr<&'a mut ITp<IType>> {
        let mut base = &mut current.ptypes[self.pid];
        for ppid in &self.depth {
            base = &mut base.params[*ppid];
        }

        base.as_mut()
    }
}

impl<'a> ParamCheckPass<'a> {
    fn new(genbuffer: &'a mut GenBuffer, current: &'a mut PendingTyping, pid: usize) -> Self {
        Self {
            genbuffer,
            current,
            pointer: Pointer {
                pid,
                depth: SmallVec::new(),
            },
        }
    }

    fn generic_comparison_against_child(
        &mut self,
        expecting: Tr<&Tp<hir::Type>>,
    ) -> Result<(), Error> {
        let mut giving = self.pointer.read_mut(&mut self.current);

        let exp_paramsn = expecting.inner.params.len();
        let expects_params = exp_paramsn > 0;
        let given_paramsn = giving.params.len();
        let got_params = given_paramsn > 0;

        match (&mut giving.inner.root, &expecting.root) {
            // applicating inferable parameters onto generic expectations
            //
            // Since `gid` is in the generics namespace of the child function we cannot use the same
            // gid as it might conflict with existing `gid`s.
            // Therefore; we must generate a new unused `gid` from the current functions scope instead.
            //
            // --
            // our `_` is applied onto a generic value where the generic is higher-kinded
            (IType::Infer, Type::Generic(Unbound(gid))) if expects_params => {
                forbid_inf_if(got_params, giving.span.clone())?;

                todo!("higher-kinded types");
            }
            // our `_` is applied onto a generic value with trait bounds we must replicate where the generic is higher-kinded
            (IType::Infer, Type::Generic(Bound(gid, bounds))) if expects_params => {
                forbid_inf_if(got_params, giving.span.clone())?;

                todo!("higher-kinded types");
            }
            // our `_` is applied onto a generic value
            (IType::Infer, Type::Generic(Unbound(gid))) => {
                forbid_inf_if(got_params, giving.span.clone())?;

                let newgid = first_unused_generic(0, &self.current);
                self.pointer.read_mut(&mut self.current).inner.root = IType::unbound(newgid);

                Ok(())
            }
            // our `_` is applied onto a generic value with trait bounds we must replicate
            (IType::Infer, Type::Generic(Bound(gid, bounds))) => {
                forbid_inf_if(got_params, giving.span.clone())?;

                let newgid = first_unused_generic(0, &self.current);
                self.pointer.read_mut(&mut self.current).inner.root =
                    IType::bound(newgid, bounds.clone());

                Ok(())
            }

            // applicating inferable parameters onto direct expectations
            //
            // --
            // our `_` doesn't have parameters so we overwrite it with the entirety of `exp`
            // including it's parameters.
            (IType::Infer, exp) if !got_params => {
                *giving.inner = expecting.inner.clone().into_unknown();
                Ok(())
            }
            // our `_` has parameters like `_ int`. So; we want to recursively check and call it's
            // parameters against the `exp` parameters.
            (IType::Infer, exp) => {
                if given_paramsn != exp_paramsn {
                    panic!(
                    "ET: `_` either needs no parameters or have the same amount as the expected"
                );
                }
                // TODO: is this naive?
                giving.root = IType::True(exp.clone());

                self.generic_comparison_against_child_tp(given_paramsn, &expecting.params)?;

                Ok(())
            }

            // applicating known parameters onto generics
            //
            // --
            //
            (IType::True(got), Type::Generic(Unbound(gid))) => {
                if given_paramsn == exp_paramsn {
                    todo!();
                } else {
                    todo!();
                }
            }
            (IType::True(got), Type::Generic(Bound(gid, bounds))) => {
                if given_paramsn == exp_paramsn {
                    todo!();
                } else {
                    todo!();
                }
            }
            (IType::True(got), exp) => {
                todo!();
            }
        }
    }

    fn generic_comparison_against_child_tp(
        &mut self,
        ppid_len: usize,
        expecting: &[Tr<Tp<hir::Type>>],
    ) -> Result<(), Error> {
        assert_eq!(ppid_len, expecting.len());

        for (e, ppid) in expecting.iter().zip(0..ppid_len) {
            self.pointer.depth.push(ppid);
            self.generic_comparison_against_child(e.as_ref())?;
            self.pointer.depth.pop();
        }

        Ok(())
    }
}

// for when a parameter is directly returnt by the function so both the return type and parameter
// need to infer from eachother and then become the same type.
fn bidirectional_type_inference(typing: &mut PendingTyping, param: usize) -> Result<(), Error> {
    unsafe fn run(
        // we need to be able to reach the root *while* walking the tree in order to generate first_unused_generic
        typing: *const PendingTyping,
        left: &mut Tr<ITp<IType>>,
        right: &mut Tr<ITp<IType>>,
    ) -> Result<(), Error> {
        match (&mut left.root, &mut right.root) {
            (IType::Infer, IType::Infer) => {
                let gid = first_unused_generic(typing);
                let t = IType::unbound(gid);
                left.root = t.clone();
                right.root = t;
            }
            (IType::True(t), IType::Infer) => {
                right.root = IType::True(t.clone());
            }
            (IType::Infer, IType::True(t)) => {
                left.root = IType::True(t.clone());
            }
            (IType::True(l), IType::True(r)) => {
                if l != r {
                    panic!("ET: return type missmatch");
                }
            }
        }

        if left.params.len() != right.params.len() {
            panic!("ET: return type tp length missmatch");
        }

        left.params
            .iter_mut()
            .zip(right.params.iter_mut())
            .try_for_each(|(l, r)| run(typing, l, r))?;

        Ok(())
    }

    unsafe fn first_unused_generic(typing: *const PendingTyping) -> u8 {
        super::first_unused_generic(0, &*typing)
    }

    let typing_backdoor = typing as *const PendingTyping;

    let left = &mut typing.ptypes[param];
    let right = &mut typing.returns;

    unsafe { run(typing_backdoor, left, right) }
}

fn forbid_inf_if(cond: bool, span: std::ops::Range<usize>) -> Result<(), Error> {
    if cond {
        Err(Error::InferenceForbidden.position(span))
    } else {
        Ok(())
    }
}

fn infer_fail(span: Option<std::ops::Range<usize>>) -> Error {
    match span {
        Some(span) => Error::InferenceFailure.position(span),
        None => Error::InferenceFailure,
    }
}

fn new_fn_pointer(types: Vec<Tr<Tp<hir::Type>>>) -> Tp<hir::Type> {
    Tp::new(hir::Type::fn_pointer(), types)
}

fn no_params<T>() -> Vec<T> {
    Vec::new()
}
