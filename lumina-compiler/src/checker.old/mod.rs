use super::*;

mod error;
pub use error::{Error, ErrorCompiler};
mod type_check;
pub use type_check::TypeCheck;

pub mod patterns;

use hir::function::Identifier;
use lumina_parser::ast::Inlinable;
use lumina_typesystem::{Direct, GenBuffer, Generic, Tp, Type, Typed};
use lumina_util::Tr;
use std::collections::HashMap;

type Expression = mir::Expression<Tp<Type>>;

/// The checker is responsible for converting HIR to MIR
///
/// * Applies full type-checking
/// * Removes sugars such as lambdas and let-binds
/// ?? * do we desugar match expression and pattern matching?
/// ?? * i think it's a better idea to turn `if expr` into `match` and then make opti on match
pub struct Checker<'hir> {
    verified: &'hir verifier::Storage,
    dynfuncs: Vec<DynFunc>,
}

impl<'hir> Checker<'hir> {
    pub fn new(verified: &'hir verifier::Storage) -> Self {
        Self {
            verified,
            dynfuncs: Vec::new(),
        }
    }
}

// TODO:
//
// I kinda would like to make this iteration based. So; would it still be possible to do function
// ID assignment for lambdas and closures in the verifier and then link them using the ID instead?
//
// We'd effectively do the lifting there; but still mark it as a lambda for the error message
// benefits
//
// Ye let's see if lambda lifting is fully doable in the verifier stage.
//
// TODO: Actually; let's just have an internal state for generated functions here and then assign
// ID's seperately in here. No reason to *move* that to the verifier.

impl<'hir> Checker<'hir> {
    pub fn function<'held, 'g>(
        &mut self,
        body: &'held hir::function::Body,
        header: &'held hir::function::Header,
        genbuffer: &'g mut GenBuffer,
    ) -> Result<mir::Entity, Error> {
        let expr = CheckerPass {
            verified: &self.verified,
            dynfuncs: &mut self.dynfuncs,
            wheres: &body.wheres,
            header,
        }
        .run(&body.entity)?;

        if *header.typing.returns != expr.meta {
            panic!(
                "ET: return type mismatch. exp {} got {}",
                header.typing.returns, expr.meta.root
            );
        }

        Ok(expr.entity)
    }
}

// This approach is flawed. There's a difference between checking type compatibility for
// parameters and checking that the return value of a function is correct.
//
// We want GenericBuffer ń shit for the parameters. And we want to be a lot more strict for the
// return value.
//
// Ye because we do want to generic-decode the return value of the child function; but then
// direct-match that against our annotated function return value for the top-level (parent).

// There's another issue.
//
// This layout assumes one function maps to one function. But; since we want to lambda-lift at this
// stage this isn't true.

// I think we're in the verifier gonna store lambdas/lets seperately from other functions and
// assign ID's in hir::Storage from 0

struct CheckerPass<'hir, 'held> {
    verified: &'hir verifier::Storage,
    wheres: &'held HashMap<usize, hir::function::WhereBinding>,
    header: &'held hir::function::Header,

    dynfuncs: &'held mut Vec<DynFunc>,
}

/// A function which is generated by the compiler
///
/// Such as lifted lambda expressions
pub struct DynFunc {
    entity: mir::Entity,
}

impl<'hir, 'held> CheckerPass<'hir, 'held> {
    // TODO: Where do we append spans to the errors?
    fn run(&mut self, value: &hir::Entity) -> Result<Expression, Error> {
        match value {
            // TODO: I don't think these `if` checks are valid since they won't take generics into
            // count. We currently never actually *assign* generics. Which we definitely need to.
            hir::Entity::Inlined(Inlinable::Int(n)) => {
                let t = Tp::none(Type::int());
                Ok(Expression::new(mir::Entity::Int(*n), t))
            }
            hir::Entity::Inlined(Inlinable::Float(n)) => {
                let t = Tp::none(Type::float());
                Ok(Expression::new(mir::Entity::Float(*n), t))
            }

            hir::Entity::Identifier(Identifier::Param(pid), params) => {
                let param_type = &self.header.typing.ptypes[*pid];

                // destruct for function-parameter since type rules change
                match &param_type.root {
                    Type::Direct(Direct::FnPointer) => {
                        todo!();
                    }
                    _ => {
                        // TODO: might need a fancy error here instead, can't remember if verifier catches this.
                        assert!(params.is_empty());
                        Ok(Expression::new(
                            mir::Entity::Parameter(*pid),
                            param_type.clone().inner,
                        ))
                    }
                }
            }

            hir::Entity::Identifier(Identifier::Function(fid), params) => {
                let f = &self.verified.functions[fid];

                let (checked, genbuffer) = self.run_call_params(params, &f.header.typing.ptypes)?;

                let mut ret = f.header.typing.returns.clone();
                genbuffer.decode(&mut ret).expect("TODO: ET: wrap error");

                Ok(Expression::new(
                    mir::Entity::Function(*fid, checked),
                    ret.inner,
                ))
            }

            hir::Entity::Identifier(Identifier::Method { trid, mid }, params) => {
                let method = &self.verified.traits[*trid].headers[mid];

                let (checked, genbuffer) = self.run_call_method_params(params, &method)?;
                todo!();
            }

            // TODO: The reason these are seperate branches is so that we can have more
            // specific errors. But; we aren't taking advantage of that yet.
            hir::Entity::Lambda(header, entity, params) => {
                let (checked, genbuffer) = self.run_call_params(&params, &header.typing.ptypes)?;

                todo!("we've completely forgotten about captures");

                let mut ret = header.typing.returns.clone();
                genbuffer.decode(&mut ret);

                let (dynfid, _got_ret) =
                    self.dynamically_generate_lambda(entity.as_ref().map(|a| &**a))?;

                todo!("i think we need to check ret against got_ret");

                Ok(Expression::new(
                    mir::Entity::DynFunction(dynfid, checked),
                    ret.inner,
                ))
            }
            hir::Entity::First(header, entity, params) => {
                let (checked, genbuffer) = self.run_call_params(&params, &header.typing.ptypes)?;

                let mut ret = header.typing.returns.clone();
                genbuffer.decode(&mut ret);

                let (dynfid, got_ret) =
                    self.dynamically_generate_firststm(entity.as_ref().map(|a| &**a))?;

                if *header.typing.returns != *got_ret {
                    panic!(
                        "ET: return type mismatch. exp {} got {}",
                        header.typing.returns, got_ret,
                    );
                }

                Ok(Expression::new(
                    mir::Entity::DynFunction(dynfid, checked),
                    ret.inner,
                ))
            }

            hir::Entity::DotCall(_, _) => unreachable!(
                "I think we're moving this to be abstracted away entirely in the verifier?"
            ),

            hir::Entity::Pass(box hir::Entity::Identifier(Identifier::Function(fid), params))
                if params.is_empty() =>
            {
                let f = &self.verified.functions[&fid];

                let mut pt = f.header.typing.ptypes.clone();
                pt.push(f.header.typing.returns.clone());

                let t = Tp::new(Type::fn_pointer(), pt);
                Ok(Expression::new(mir::Entity::FnPointer(*fid), t))
            }

            hir::Entity::Pass(box hir::Entity::Identifier(Identifier::Function(fid), params)) => {
                let f = &self.verified.functions[&fid];

                // TODO: the non-partially evaluated might rename non-decoded after this if they
                // haven't been encountered before. Is that okay? It might be entirely fine. But it
                // might also not be.
                let (checked, genbuffer) =
                    self.run_call_params(params, &f.header.typing.ptypes[..params.len()])?;

                let mut pt = f.header.typing.ptypes[params.len()..].to_vec();
                pt.iter_mut()
                    .try_for_each(|t| genbuffer.decode(t))
                    .expect("ET");
                let mut ret = f.header.typing.returns.clone();
                genbuffer.decode(&mut ret).expect("ET");
                pt.push(ret);

                unimplemented!("closures. This will be done by creating a dynamic trait-object of the Closure trait. Params were {:#?}", checked);
            }
            hir::Entity::Match {
                kind,
                of,
                patterns,
                evals,
            } => self.run_match(*kind, of, patterns, &evals),

            hir::Entity::Tuple(entries) => self.run_tuple(entries),

            _ => todo!("{:#?}", value),
        }
    }

    fn run_tuple(&mut self, entries: &[Tr<hir::Entity>]) -> Result<Expression, Error> {
        let mut tp = Vec::with_capacity(entries.len());
        let mut vs = Vec::with_capacity(entries.len());

        for entry in entries.iter() {
            let expr = self.run(entry)?;
            tp.push(Tr::tr(entry.span.clone(), expr.meta));
            vs.push(expr.entity);
        }

        let t = Tp::new(hir::Type::tuple(), tp);
        let v = mir::Entity::Structure(t.clone(), vs);

        Ok(Expression::new(v, t))
    }

    fn run_call_params(
        &mut self,
        params: &[Tr<hir::Entity>],
        ptypes: &[Tr<Tp<Type>>],
    ) -> Result<(Vec<mir::Entity>, GenBuffer), Error> {
        let (mut entities, mut genbuffer) = (Vec::with_capacity(params.len()), GenBuffer::new());

        for (p, exp) in params.iter().zip(ptypes.iter()) {
            let expr = self.run(p)?;

            self.type_check(
                &mut genbuffer,
                exp.as_ref(),
                Tr::tr(p.span.clone(), &expr.meta),
            )?;

            entities.push(expr.entity);
        }

        Ok((entities, genbuffer))
    }

    fn run_call_method_params(
        &mut self,
        params: &[Tr<hir::Entity>],
        method: &hir::Method,
    ) -> Result<(Vec<mir::Entity>, GenBuffer), Error> {
        let mut genbuffer = GenBuffer::new();

        params
            .iter()
            .zip(method.typing.ptypes.iter())
            .map(|(p, traittype)| {
                let expr = self.run(p)?;

                match &traittype.inner {
                    // static type
                    hir::TraitType::True(exp) => self.type_check(
                        &mut genbuffer,
                        Tr::tr(traittype.span.clone(), exp),
                        Tr::tr(p.span.clone(), &expr.meta),
                    ),

                    // I guess we here want to take `expr.meta` and check if it implements the
                    // trait this method belongs to?
                    //
                    // Actually; we're making decisions based of HKT or not here however; that code
                    // might already be made in the lumina_typesystem no?
                    //
                    // I kinda want to make a more low-level approach here and use
                    // lumina_typesystem as a query database that won't resolve these sort of
                    // things for me I think. Since it's easier to get extrea information here.
                    //
                    // Dunno; let's try using lumina_typesystem high-level api's
                    //
                    // Actually; because of "strict" we do need to handle HKT here regardless.

                    // non-hkt
                    hir::TraitType::Self_(type_params) if type_params.is_empty() => {
                        let trid = method.trait_;
                        let trtp = &[];
                        let impltor = Tr::tr(p.span.clone(), &expr.meta.root);
                        let impltor_tp = &expr.meta.params;

                        let _impl = self
                            .verified
                            .impls
                            .lookup(trid, trtp, impltor, impltor_tp, &mut genbuffer)
                            .map_err(|_| {
                                type_check::TypeError::NotImplemented(
                                    Tr::tr(p.span.clone(), expr.meta.clone()),
                                    trid,
                                )
                            })
                            .map_err(Error::TypeError)?;

                        dbg!(&_impl);
                        todo!();
                    }

                    // hkt
                    hir::TraitType::Self_(type_params) => todo!(),

                    _ => todo!(),
                }?;

                Ok(expr.entity)
            })
            .collect::<Result<Vec<_>, Error>>()
            .map(|entities| (entities, genbuffer))
    }

    fn dynamically_generate_lambda(
        &mut self,
        entity: Tr<&hir::Entity>,
    ) -> Result<(mir::DYNFID, Tr<Tp<Type>>), Error> {
        let v = self.run(&*entity)?;
        let dynfid = self.dynfuncs.len();
        self.dynfuncs.push(DynFunc { entity: v.entity });
        Ok((dynfid, Tr::tr(entity.span.clone(), v.meta)))
    }

    fn dynamically_generate_firststm(
        &mut self,
        entity: Tr<&hir::Entity>,
    ) -> Result<(mir::DYNFID, Tr<Tp<Type>>), Error> {
        self.dynamically_generate_lambda(entity)
    }
}

fn check_return(
    genbuffer: &GenBuffer,
    ret: &Tr<Tp<Type>>,
    expect: Tr<&Tp<Type>>,
) -> Result<Tr<Tp<Type>>, Error> {
    let mut ret = ret.clone();

    if let Err(e) = genbuffer.decode(&mut ret) {
        todo!();
    }

    if expect != ret.as_ref() {
        panic!("ET: Return type mismatch. Got {} exp {}", ret, expect);
    }

    Ok(ret)
}