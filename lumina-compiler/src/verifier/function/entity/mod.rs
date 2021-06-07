use super::*;
use hir::function::Value;
use hir::Entity;

pub type Expr = (hir::Entity, ReturnType);
type Result<T> = std::result::Result<T, Error>;

mod local;

// TODO: I'm not sure we *ever* need to return types.
//
// we have self.genbuffer if needed and we can check the types in-line with `expect`.
//
// with the current setup. the `expect` *is used to type-check* when encountering local parameters.
// however; it isn't used to check types directly here which is of course wrong.
//
// alright in order to not get duplicate checks; let's make this a set rule:
// *we try to do the actual type check in as deep of a scope as possible*
// inlines for example are dead-ends. So; let's type-check there.

impl<'calls, 'ast, 'hir> Pass<'calls, 'ast, 'hir> {
    pub fn entity(&mut self, expect: Expect<'_>, entity: Tr<&ast::Entity>) -> Result<hir::Entity> {
        match *entity {
            ast::Entity::Inlined(ast::Inlinable::Int(n)) => {
                let t = Tr::tr(entity.span.clone(), Tp::none(hir::Type::int()));
                self.type_check_value(t, expect)?;
                Ok(Entity::Inlined(Value::Int(*n)))
            }
            ast::Entity::Inlined(ast::Inlinable::String(_)) => todo!("read_only table stuff here"),

            ast::Entity::Identifier { takes, params } => self.identifier(takes, params, expect),

            _ => unimplemented!("expression: {:#?}", entity),
        }
        .map_err(|err| err.position(entity.span.clone()))
    }

    fn expand(&self, loc: &Location) -> Location {
        let origin = &self.current.as_ref().unwrap().func.module;
        self.verifier.expand(origin, loc)
    }

    fn identifier(
        &mut self,
        location: &Location,
        params: &[Tr<ast::Entity>],
        expect: Expect<'_>,
    ) -> Result<hir::Entity> {
        // check for local identifiers such as parameters
        if location.is_local {
            if let Some(v) = self.local(location.last(), params, expect)? {
                return Ok(v);
            }
        }

        // expand identifier to absolute, either by imports are by current module origin
        let flocation = self.expand(location);

        self.function_call(&flocation, params, expect)
    }

    fn function_call(
        &mut self,
        location: &Location,
        params: &[Tr<ast::Entity>],
        expect: Expect<'_>,
    ) -> Result<hir::Entity> {
        let fid = self
            .verifier
            .ast
            .functions
            .resolve(location)
            .ok_or_else(|| Error::FunctionNotFound(location.clone()))?;

        self.function_call_by_fid(fid, params, expect)
    }

    fn function_call_by_fid(
        &mut self,
        fid: usize,
        params: &[Tr<ast::Entity>],
        expect: Expect<'_>,
    ) -> Result<hir::Entity> {
        let child = self.call_child(fid);

        let (params_for_child, genbuffer_of_call, mut ret) = match &child.header {
            hir::function::Header::Known(typing) => self
                .params(&typing.ptypes, params)
                .map(|(p, g)| (p, g, typing.returns.clone())),
            hir::function::Header::Failed(ptyping) => match poisoned_typing_to_known(ptyping) {
                Some(known_typing) => {
                    let (entity, genbuffer) = self.params(&known_typing.ptypes, params)?;
                    Ok((entity, genbuffer, known_typing.returns))
                }
                None => Err(Error::Skipped),
            },
        }?;

        genbuffer_of_call
            .decode(&mut ret)
            .expect("return type shouldn't be able to be generic if the same generic is not present as parameter");

        // TODO: Check return type
        debug!("WARNING", "missing type-check of return type");

        let identifier = hir::function::Identifier::Function(fid);
        let entity = Entity::Identifier(identifier, params_for_child);
        Ok(entity)
    }

    fn params(
        &mut self,
        child_ptypes: &[Tr<Tp<hir::Type>>],
        params: &[Tr<ast::Entity>],
    ) -> Result<(Vec<Tr<Entity>>, GenBuffer)> {
        // in case of nested function calls, we need to make sure we don't corrupt the state of parent
        //
        // backup parent genbuffer
        let old_genbuffer = std::mem::replace(&mut self.genbuffer, GenBuffer::new());

        if params.len() != child_ptypes.len() {
            panic!("ET: parameter amount mismatch");
        }

        let hir_params = params
            .iter()
            .zip(child_ptypes.iter())
            .map(|(p, exp)| {
                let entity = self.entity(Expect::Exact(exp), p.as_ref())?;
                Ok(Tr::tr(p.span.clone(), entity))
            })
            .collect::<Result<Vec<_>>>()?;

        // return to parent genbuffer
        let genbuffer = std::mem::replace(&mut self.genbuffer, old_genbuffer);

        Ok((hir_params, genbuffer))
    }
}

fn poisoned_typing_to_known(
    ptyping: &hir::function::PoisonedTyping,
) -> Option<hir::function::Typing> {
    let ptypes = ptyping
        .ptypes
        .iter()
        .map(|trp| trp.to_known().map(|t| Tr::tr(trp.span.clone(), t)).ok())
        .collect::<Option<Vec<_>>>()?;

    let returns = ptyping.returns.to_known().ok()?;

    Some(hir::function::Typing::new(
        ptypes,
        Tr::tr(ptyping.returns.span.clone(), returns),
    ))
}
