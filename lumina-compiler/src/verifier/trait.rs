use super::*;

/// State for the verification of a trait decleration
pub struct Pass<'ast, 'hir> {
    verifier: Verifier<'ast, 'hir>,
    trait_: &'ast collector::DefinedTrait,
}

impl<'ast, 'hir> Pass<'ast, 'hir> {
    pub fn new(verifier: Verifier<'ast, 'hir>, trait_: &'ast collector::DefinedTrait) -> Self {
        Self { verifier, trait_ }
    }

    pub fn run(mut self) -> hir::UserTrait {
        let headers = self
            .trait_
            .body
            .functions
            .iter()
            .map(|(name, header)| (header.1, self.verify_header(name, header.0.as_ref())))
            .collect();

        hir::UserTrait {
            span: self.trait_.span.clone(),
            location: self.trait_.location.clone(),

            type_params_n: self.trait_.type_params.len(),

            headers,
        }
    }

    fn verify_header(&mut self, name: &str, header: Tr<&ast::function::Header>) -> hir::Method {
        // TODO: We cannot simply implement UniqueHandler since it's `trait_self` method expects
        // `Tp<hir::Type>`.
        todo!();
    }
}

struct TraitTypeVerifier;

pub struct TTp<T> {
    pub root: T,
    pub params: Vec<Tr<TTp<hir::TraitType>>>,
}

impl From<hir::Type> for hir::TraitType {
    fn from(t: hir::Type) -> hir::TraitType {
        hir::TraitType::True(t)
    }
}

impl lumina_typesystem::Typed for hir::TraitType {
    fn unbound(gid: u8) -> hir::TraitType {
        hir::Type::unbound(gid).into()
    }
    fn bound(gid: u8, bounds: Vec<Tp<usize>>) -> hir::TraitType {
        hir::Type::bound(gid, bounds).into()
    }
    fn prim(p: lumina_util::Primitive) -> hir::TraitType {
        hir::Type::prim(p).into()
    }
    fn defined(tid: usize) -> hir::TraitType {
        hir::Type::defined(tid).into()
    }
    fn tuple() -> hir::TraitType {
        hir::Type::tuple().into()
    }
    fn pointer() -> hir::TraitType {
        hir::Type::pointer().into()
    }
    fn fn_pointer() -> hir::TraitType {
        hir::Type::fn_pointer().into()
    }
}

impl r#type::UniqueHandler for TraitTypeVerifier {
    type Root = hir::TraitType;
    type Tree = TTp<hir::TraitType>;

    fn trait_self(
        &mut self,
        verifier: Verifier<'_, '_>,
        tp: Vec<Tr<Self::Tree>>,
    ) -> Result<Self::Tree, r#type::TypeVerificationError> {
        todo!();
    }

    fn generic(
        &mut self,
        verifier: Verifier<'_, '_>,
        gid: u8,
        _tp: &lumina_parser::TypeParams,
    ) -> Result<Self::Root, r#type::TypeVerificationError> {
        todo!();
    }

    fn type_parameter(
        &mut self,
        verifier: Verifier<'_, '_>,
        idx: usize,
        gid: u8,
        _tp: &lumina_parser::TypeParams,
    ) -> Result<Self::Root, r#type::TypeVerificationError> {
        todo!();
    }

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree {
        TTp { root, params }
    }
}

impl<'ast, 'hir> r#type::TypeVerifier<'ast, 'hir, TraitTypeVerifier> {}
