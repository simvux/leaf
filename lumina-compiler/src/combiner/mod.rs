use crate::{collector, hir, mir, verifier, LinkedStore};
use lumina_env::Environment;
use lumina_typesystem::{GenBuffer, Tp, Typed};
use lumina_util::Tr;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
impl std::error::Error for Error {}

impl From<lumina_typesystem::Error<hir::UserImpl>> for Error {
    fn from(e: lumina_typesystem::Error<hir::UserImpl>) -> Error {
        todo!();
    }
}

pub type NewFID = usize;

mod r#type;
pub use r#type::*;
mod run;

/// The combiner is responsible for
///
///   * type checking HIR
///   * applying static dispatch
///   * let/lambda lifting
///
pub struct Combiner<'prev> {
    env: Rc<Environment>,

    ast: &'prev collector::Storage,
    hir: &'prev verifier::Storage,

    // Functions post static-dispatch
    built: Storage,
}

/// The instance of a Combiner over a function.
struct CombinerPass<'v, 'c, 'prev> {
    combiner: &'c mut Combiner<'prev>,

    current_template: &'prev hir::Function,
    current_variant: &'v [Tr<StaticType>],
}

pub struct Storage {
    functions: LinkedStore<(usize, Vec<Tr<StaticType>>), Function>,

    // Each function that belongs to an implementation of a trait is stored here instead.
    //
    // The first `usize` is the trait ID and the second is the method ID.
    methods: LinkedStore<(usize, usize, Vec<Tr<StaticType>>), Function>,
}

impl Storage {
    pub fn new() -> Self {
        Self {
            functions: LinkedStore::with_capacity(40),
            methods: LinkedStore::with_capacity(20),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    expr: mir::Expression,
}

impl<'prev> Combiner<'prev> {
    pub fn new(
        env: Rc<Environment>,
        ast: &'prev collector::Storage,
        hir: &'prev verifier::Storage,
    ) -> Self {
        Self {
            env,
            ast,
            hir,
            built: Storage::new(),
        }
    }

    pub fn start(&mut self, entry: usize) -> Result<(usize, StaticType)> {
        // TODO: we cannot call into run_function since we need to skip the return type check.
        let _returns = self.call_function(entry, vec![])?;

        todo!();
    }

    fn call_function(
        &mut self,
        fid: usize,
        ptypes: Vec<Tr<StaticType>>,
    ) -> Result<(NewFID, StaticType)> {
        if let Some(id) = self
            .built
            .functions
            .resolve(&(fid, ptypes.as_slice()) as &dyn FuncKey)
        {
            let returns = self
                .built
                .functions
                .get_from_idx(id)
                .unwrap()
                .expr
                .r#type
                .clone();
            return Ok((id, returns));
        }

        let hir = &self.hir.functions[&fid];
        let f = self.run(hir, &ptypes)?;

        let returns = f.expr.r#type.clone();

        let id = self.built.functions.next_id();
        if let Some(existing) = self.built.functions.insert((fid, ptypes), f) {
            panic!(
                "recompiled a variant that was already cached: {:?}",
                existing
            );
        }

        Ok((id, returns))
    }

    fn call_method(
        &mut self,
        traitid: usize,
        methodid: usize,
        self_pid: hir::SelfPosition,
        ptypes: Vec<Tr<StaticType>>,
    ) -> Result<(usize, StaticType)> {
        if let Some(id) = self
            .built
            .methods
            .resolve(&(traitid, methodid, ptypes.as_slice()) as &dyn MethodKey)
        {
            let returns = self
                .built
                .methods
                .get_from_idx(id)
                .unwrap()
                .expr
                .r#type
                .clone();
            return Ok((id, returns));
        }
        let trait_ = &self.hir.traits[traitid];

        /*
        let impltor = match self_pid {
            Some(selfpid) => &ptypes[selfpid],
            None => {
                unimplemented!(
                    "Calling methods of traits where the method doesn't reference `self`"
                )
            }
        };
        */
        let impltor = todo!();

        let mut generics = GenBuffer::new();

        // TODO: Make sure this is actually correct. Because; it does seem rather strange.
        let traittp = (0..)
            .map(|id| Tr::new(Tp::none(hir::Type::unbound(id))))
            .take(trait_.type_params_n)
            .collect::<Vec<_>>();

        let selected: &HashMap<usize, hir::Function> = self.hir.impls.clean_lookup_direct(
            traitid,
            &traittp,
            // Tr::tr(impltor.idx, &impltor.template.root),
            todo!(),
            // &impltor.template.params,
            todo!(),
            &mut generics,
        )?;
        let selected_method = &selected[&methodid];

        let f = self.run(selected_method, &ptypes)?;
        let mfid = self.built.methods.next_id();
        let returns = f.expr.r#type.clone();

        if let Some(_) = self
            .built
            .methods
            .insert((traitid, methodid, ptypes.clone()), f)
        {
            unreachable!()
        }

        Ok((mfid, returns))
    }

    fn run(&mut self, f: &'prev hir::Function, ptypes: &[Tr<StaticType>]) -> Result<Function> {
        let mut pass = CombinerPass::new(self, f, ptypes);
        let expr = pass.run(&f.body.entity)?;
        Ok(Function { expr })
    }
}

impl<'v, 'c, 'prev> CombinerPass<'v, 'c, 'prev> {
    fn new(
        combiner: &'c mut Combiner<'prev>,
        current_template: &'prev hir::Function,
        current_variant: &'v [Tr<StaticType>],
    ) -> Self {
        Self {
            combiner,
            current_template,
            current_variant,
        }
    }
}
