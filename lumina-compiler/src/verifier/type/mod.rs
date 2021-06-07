use super::*;

use lumina_typesystem::{Tp, Typed};
use lumina_util::{Location, Tr};

pub mod r#enum;
pub mod r#struct;

use lumina_parser::Type as AstType;

pub trait UniqueHandler {
    type Root;
    type Tree;

    fn trait_self(
        &mut self,
        verifier: Verifier<'_, '_>,
        _tp: Vec<Tr<Self::Tree>>,
    ) -> Result<Self::Tree, TypeVerificationError> {
        Err(TypeVerificationError::SelfNotAllowed)
    }

    fn generic(
        &mut self,
        verifier: Verifier<'_, '_>,
        _gid: u8,
        _tp: &lumina_parser::TypeParams,
    ) -> Result<Self::Root, TypeVerificationError> {
        Err(TypeVerificationError::GenericsNotAllowed)
    }

    // TODO: Highly considering removing the concept of the Type::TypeParameter and just edge-case
    // the generic instead.
    fn type_parameter(
        &mut self,
        verifier: Verifier<'_, '_>,
        _idx: usize,
        _gid: u8,
        _tp: &lumina_parser::TypeParams,
    ) -> Result<Self::Root, TypeVerificationError> {
        panic!("lumina_parser::Type::TypeParameter in dissallowed context");
    }

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree;
}

pub struct TypeVerifier<'ast, 'hir, H: UniqueHandler> {
    pub verifier: Verifier<'ast, 'hir>,
    pub handler: H,
}

#[derive(Debug)]
pub enum TypeVerificationError {
    Positioned(std::ops::Range<usize>, Box<Self>),
    ForbiddenInference,
    NotFound(Location),
    TraitNotFound(Location),
    GenericsNotAllowed,
    SelfNotAllowed,
}

impl TypeVerificationError {
    pub fn position(self, span: std::ops::Range<usize>) -> Self {
        match self {
            a @ Self::Positioned(..) => a,
            other => Self::Positioned(span, Box::new(other)),
        }
    }
}

impl lumina_util::ToError for TypeVerificationError {
    fn name(&self) -> &'static str {
        "placeholder name"
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        match self {
            Self::Positioned(span, _) => Some(span.clone()),
            _ => None,
        }
    }

    fn text(&self) -> String {
        format!("{:?}", self)
    }
}

// `infered` variants of methods are the similarly named non-infered variants except that they
// allow type inference and return types to reflect that (since hir::Type doesn't have infer).

impl<'ast, 'hir, R: Typed, H> TypeVerifier<'ast, 'hir, H>
where
    H: UniqueHandler<Root = R>,
{
    pub fn new(verifier: Verifier<'ast, 'hir>, handler: H) -> Self {
        Self { verifier, handler }
    }

    // attach type parameters to a root type
    fn attach(
        &mut self,
        root: H::Root,
        tp: &lumina_parser::TypeParams,
    ) -> Result<H::Tree, TypeVerificationError> {
        self.verify_types(tp).map(|tps| H::combinator(root, tps))
    }

    // TODO: We're gonna have to repeat this code a *third* time for TraitType.
    //
    // Can we improve the visitor pattern?
    // fn verify_type(&mut self, t: &AstType) -> Result<C, TypeVerificationError>
    //   UniqueHandler: fn combine(root: O, params: Vec<O>) -> C
    //
    // I'm gonna try this.

    /// Convert an AST type into an HIR type.
    ///
    /// This verifies that all types exist, and stores them using ID's rather than identifiers.
    pub fn verify_type(&mut self, t: &AstType) -> Result<H::Tree, TypeVerificationError> {
        match t {
            AstType::Int => Ok(H::combinator(H::Root::int(), vec![])),
            AstType::Float => Ok(H::combinator(H::Root::float(), vec![])),
            AstType::Nothing => Ok(H::combinator(H::Root::unit(), vec![])),
            AstType::Bool => Ok(H::combinator(H::Root::bool(), vec![])),
            AstType::Defined(tloc, tp) => {
                let tid = self
                    .verifier
                    .ast
                    .types
                    .resolve(tloc)
                    .ok_or_else(|| TypeVerificationError::NotFound(tloc.clone()))?;

                let tp = self.verify_types(tp)?;
                Ok(H::combinator(H::Root::defined(tid), tp))
            }
            AstType::TraitSelf(tp) => self
                .verify_types(tp)
                .and_then(|tp| self.handler.trait_self(self.verifier.copy(), tp)),
            AstType::Generic(gid, tp) => self
                .handler
                // TODO: we're disgarding the fact that hkt can be applied to types with type
                // parameters. we need a merging solution right?
                .generic(self.verifier.copy(), *gid, tp)
                .and_then(|t| {
                    let hkt_tps = self.verify_types(tp)?;
                    Ok(H::combinator(t, hkt_tps))
                }),
            AstType::List(inner) => self
                .verify_type(inner)
                .and_then(|of| self.list_type(of))
                .map_err(|e| e.position(inner.span.clone())),
            AstType::Tuple(entries) => self
                .verify_types(entries)
                .map(|elems| H::combinator(Typed::tuple(), elems)),
            AstType::Function(takes, gives) => self.verify_function_type(takes, gives),
            AstType::Closure(takes, gives) => self.verify_closure_type(takes, gives),
            AstType::Pointer(of) => self
                .verify_type(of)
                .map(|r| H::combinator(Typed::pointer(), vec![Tr::tr(of.span.clone(), r)])),
            AstType::Infallible => todo!(),
            AstType::TypeParameter(idx, gid, tp) => self
                .handler
                .type_parameter(self.verifier.copy(), *idx, *gid, tp)
                .and_then(|root| self.attach(root, tp)),
            AstType::Infer => Err(TypeVerificationError::ForbiddenInference),
        }
    }

    pub fn verify_types(
        &mut self,
        ts: &lumina_parser::TypeParams,
    ) -> Result<Vec<Tr<H::Tree>>, TypeVerificationError> {
        ts.iter()
            .map(|t| {
                self.verify_type(t)
                    .map_err(|e| e.position(t.span.clone()))
                    .map(|r| Tr::tr(t.span.clone(), r))
            })
            .collect()
    }

    pub fn verify_function_type(
        &mut self,
        takes: &lumina_parser::TypeParams,
        gives: &Tr<AstType>,
    ) -> Result<H::Tree, TypeVerificationError> {
        takes
            .iter()
            .map(|t| self.verify_type(t).map(|r| Tr::tr(t.span.clone(), r)))
            .collect::<Result<Vec<_>, _>>()
            .and_then(|mut tps| {
                let returns = self.verify_type(gives)?;
                tps.push(Tr::tr(gives.span.clone(), returns));
                Ok(H::combinator(Typed::fn_pointer(), tps))
            })
    }

    pub fn list_type(&self, _of: H::Tree) -> Result<H::Tree, TypeVerificationError> {
        todo!("we need a list tid in verifier.settings");
    }

    pub fn verify_closure_type(
        &self,
        _takes: &lumina_parser::TypeParams,
        _gives: &Tr<AstType>,
    ) -> Result<H::Tree, TypeVerificationError> {
        unimplemented!("dynamic dispatch, use `fn(a ->  b)` syntax instead for function pointers");
    }
}
