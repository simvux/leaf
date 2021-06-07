use super::*;
use crate::{collector, Typing as BaseTyping};
use lumina_typesystem::{GenBuffer, Generic as LTSGen, ITp, Tp, Typed};
use lumina_util::{Location, Tr};

mod entity;
pub use entity::Expr;
pub mod type_check;

#[derive(Debug)]
pub struct Builder<'ast> {
    func: &'ast collector::Function,

    // descriptor of the form of call this function allows
    calldescr: FCall,

    typing: PendingTyping,

    lambda_count: usize,
}

#[derive(Clone, Copy)]
pub enum Expect<'a> {
    Exact(&'a Tr<Tp<hir::Type>>),
    // the current value is what the function returns
    //
    // we need this to edge-case stuff like `f a => a` where both the parameter and the return
    // type, which are supposed to become the same, are set to infer.
    RootExpr,
    // I think we won't actually put this anywhere. If we do; then this is gonna get annoying.
    // Unknown,
}

/// Type information that isn't yet known to have been fully infered
pub type PendingTyping = BaseTyping<ITp<hir::IType>>;

pub type ReturnType = Tr<Tp<hir::Type>>;

/// State for the verification of a function
pub struct Pass<'calls, 'ast, 'hir> {
    verifier: Verifier<'ast, 'hir>,
    current: Option<Builder<'ast>>,
    callstack: &'calls mut CallStack<FCall, StackEntry<'ast>>,

    // we statefully keep a genbuffer.
    //
    // this genbuffer is specific to each pass of parameters of a function.
    //
    // this is so that we can properly decode the generics that are in the return types of child
    // functions we call.
    //
    // this genbuffer gets std::mem::swap'd back and forth for each call so a child call in the
    // parameter of a child call won't corrupt the upper call.
    genbuffer: GenBuffer,
}

#[derive(Debug)]
pub struct StackEntry<'ast> {
    pub builder: Builder<'ast>,
    pub calls: FKey,
}

impl<'calls, 'ast, 'hir> Pass<'calls, 'ast, 'hir> {
    pub fn new(
        callstack: &'calls mut CallStack<FCall, StackEntry<'ast>>,
        verifier: Verifier<'ast, 'hir>,
        current: Builder<'ast>,
        genbuffer: GenBuffer,
    ) -> Self {
        Self {
            callstack,
            verifier,
            current: Some(current),
            genbuffer,
        }
    }

    pub fn current(&self) -> &Builder<'ast> {
        self.current.as_ref().unwrap()
    }
    pub fn current_mut(&mut self) -> &mut Builder<'ast> {
        self.current.as_mut().unwrap()
    }

    pub fn run(mut self, body: &'ast ast::function::Body) -> hir::Function {
        let hir_function = match self.entity(Expect::RootExpr, body.entity.as_ref()) {
            Ok(root) => {
                let kind = hir::function::Kind::Defined(
                    self.current.as_ref().unwrap().func.location.clone(),
                    Box::new(root),
                );

                let current = self.current.as_ref().unwrap();

                hir::Function {
                    span: current.func.span.clone(),
                    kind,
                    header: hir::function::Header::Known(current.generate_known_typing()),
                }
            }
            Err(e) => {
                if e.should_view() {
                    let err = self.verifier.hackily_load_error(
                        &self.current.as_ref().unwrap().func.module,
                        e.position(body.entity.span.clone()),
                    );
                    self.verifier.errors.push(err);
                }

                let kind = hir::function::Kind::Poisoned;

                hir::Function {
                    span: self.current.as_ref().unwrap().func.span.clone(),
                    kind,
                    header: hir::function::Header::Failed(self.current.unwrap().typing),
                }
            }
        };

        // TODO: remaining where-binds? i guess?

        hir_function
    }

    /// Recursively verifies a child and resolves its now known return type.
    ///
    /// `child` here can be functions, methods, accessors, constructors etc.
    /// But it can *not* be a local such as a parameter.
    pub fn call_child(&mut self, fid: usize) -> Rc<hir::Function> {
        if let Some(existing) = self.verifier.hir.functions.get(&FKey::Function(fid)) {
            return existing.clone();
        }

        if let Some((depth, stackentry)) = self.callstack.fn_is_called(fid) {
            panic!("TODO: recusion of depth {}: {:?}", depth, stackentry);
        }

        let f = self.verifier.ast.functions.get_from_idx(fid).unwrap();
        match &f.kind {
            collector::FunctionKind::Normal(body) => {
                // push the current function onto the callstack
                let current = self.current.take().unwrap();
                self.callstack.push(
                    current.calldescr,
                    StackEntry {
                        builder: current,
                        calls: FKey::Function(fid),
                    },
                );

                let verified = self
                    .verifier
                    .include_function(fid, f, body, &mut self.callstack);

                // retrieve current function from the callstack
                self.current = Some(self.callstack.pop().builder);

                verified
            }
            collector::FunctionKind::Constructor { enum_, variant } => {
                let tid = self.verifier.ast.types.resolve(enum_).unwrap();
                let enum_ = self
                    .verifier
                    .hir
                    .types
                    .get(&tid)
                    .expect("function verifier called before type verifier");

                let constructor = enum_.create_constructor(enum_.location.clone(), tid, *variant);

                self.verifier
                    .hir
                    .functions
                    .entry(FKey::Function(fid))
                    .or_insert_with(|| Rc::new(constructor))
                    .clone()
            }
            collector::FunctionKind::Accessor { struct_, field } => {
                let tid = self.verifier.ast.types.resolve(struct_).unwrap();
                let struct_ = self
                    .verifier
                    .hir
                    .types
                    .get(&tid)
                    .expect("function verifier called before type verifier");

                let accessor = struct_.create_accessor(struct_.location.clone(), tid, *field);

                self.verifier
                    .hir
                    .functions
                    .entry(FKey::Function(fid))
                    .or_insert_with(|| Rc::new(accessor))
                    .clone()
            }
            collector::FunctionKind::Method { .. } => {
                // push the current function onto the callstack
                let current = self.current.take().unwrap();
                self.callstack.push(
                    current.calldescr,
                    StackEntry {
                        builder: current,
                        calls: FKey::Function(fid),
                    },
                );

                // verify child method
                unimplemented!("methods");

                // retrieve current function from the callstack
                // self.current = self.callstack.pop().builder;
            }
        }
    }
}

fn first_unused_generic(start: u8, typing: &PendingTyping) -> u8 {
    fn has(start: u8, t: &hir::IType) -> bool {
        matches!(t, hir::IType::True(hir::Type::Generic(LTSGen::Unbound(gid)))
                 | hir::IType::True(hir::Type::Generic(LTSGen::Bound(gid, _)))
                 if **gid == start
        )
    }

    fn any_has(start: u8, ttp: &ITp<hir::IType>) -> bool {
        has(start, &ttp.root) || ttp.params.iter().map(|t| &**t).any(|t| any_has(start, t))
    }

    if typing.ptypes.iter().any(|t| any_has(start, t)) {
        first_unused_generic(start + 1, typing)
    } else {
        start
    }
}

impl<'ast> Builder<'ast> {
    pub fn new(calldescr: FCall, typing: PendingTyping, func: &'ast collector::Function) -> Self {
        Self {
            func,
            lambda_count: 0,
            typing,
            calldescr,
        }
    }

    // WARNING: This function assumes that any remaining `IType::Infer` are *unused parameters*
    fn generate_known_typing(&self) -> hir::function::Typing {
        let ptypes = self
            .typing
            .ptypes
            .iter()
            .map(|t| self.generate_known_type(t.as_ref()))
            .collect();

        let ret = self.typing.returns.as_ref();
        let returns = ret.to_known().expect("return type left unchecked");

        hir::function::Typing {
            ptypes,
            returns: Tr::tr(ret.span, returns),
        }
    }

    fn generate_known_type(&self, t: Tr<&ITp<hir::IType>>) -> Tr<Tp<hir::Type>> {
        let root = match &t.root {
            // The only way we're still supposed to be able to infered types is if the parameter is
            // never ever used. So; we can assign it a generic since it can be any type.
            hir::IType::Infer => hir::Type::unbound(first_unused_generic(0, &self.typing)),
            hir::IType::True(t) => t.clone(),
        };

        let tp = t
            .params
            .iter()
            .map(|t| self.generate_known_type(t.as_ref()))
            .collect();

        Tr::tr(t.span, Tp::new(root, tp))
    }
}
