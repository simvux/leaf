use super::*;
use hir::function::Identifier as Ident;
use lumina_util::Tr;

impl<'v, 'c, 'prev> CombinerPass<'v, 'c, 'prev> {
    /// General conversion of HIR to MIR
    pub fn run(&mut self, entity: &hir::Entity) -> Result<mir::Expression> {
        match entity {
            hir::Entity::Identifier(Ident::Where(wid), params) => self.run_where(*wid, params),
            hir::Entity::Identifier(Ident::Function(fid), params) => {
                self.run_function(*fid, params)
            }
            hir::Entity::Identifier(Ident::Param(pid), params) => self.run_param(*pid, params),
            hir::Entity::Identifier(Ident::Parent(p), params) => self.run_parent_ident(p, params),
            hir::Entity::Identifier(Ident::Pattern(), _params) => todo!(),
            hir::Entity::Inlined(_v) => todo!(),
            _ => todo!("{:#?}", entity),
        }
    }

    fn run_where(&mut self, _wid: usize, _params: &[Tr<hir::Entity>]) -> Result<mir::Expression> {
        todo!();
    }

    fn run_param(&mut self, pid: usize, _params: &[Tr<hir::Entity>]) -> Result<mir::Expression> {
        // we cannot use params.len() to detect whether it's a HOF or not. Because; functions don't
        // need to take parameters. We're gonna just need to get the type of `pid`.

        let _param = &self.current_template.header.typing.ptypes[pid];
        /*
        match &param.root {
            IType::
        }
        */
        todo!();
    }

    // When we're calling/using an identifier that needs to be captured from the parents scope
    //
    // Such as; getting the parents parameters as a lambda
    fn run_parent_ident(
        &mut self,
        _parent: &Ident,
        _params: &[Tr<hir::Entity>],
    ) -> Result<mir::Expression> {
        // I suppose this is where we need to track capturing and handle all that stuff.
        todo!();
    }

    fn run_function(&mut self, fid: usize, params: &[Tr<hir::Entity>]) -> Result<mir::Expression> {
        // we need to get the TemplateType of the child function in order to infer `params`
        // correctly.
        let child = &self.combiner.hir.functions[&fid];

        // TODO: Figure out if we need to clone here to prevent circular stuff.
        let child_infers = &child.header.typing.ptypes;

        let (params, statics) = self.run_params(params, &child_infers)?;

        // Recursive descent into child function/method
        //
        // (types aren't known unless we apply static dispatch to the child)
        let (nfid, returns) = match &child.kind {
            hir::function::Kind::Defined(_location) => self.combiner.call_function(fid, statics),
            hir::function::Kind::DefinedMethod {
                traitid,
                methodid,
                self_position,
            } => self
                .combiner
                .call_method(*traitid, *methodid, self_position.clone(), statics),
            hir::function::Kind::Accessor(_location) => todo!(),
            hir::function::Kind::Constructor(_location) => todo!(),

            // TODO: Wait a second; aren't these all stored inline and can't apear here?
            hir::function::Kind::Where => todo!(),
            hir::function::Kind::Let => todo!(),
            hir::function::Kind::Lambda => todo!(),
            hir::function::Kind::First => todo!(),
            hir::function::Kind::PartialEval => todo!(),
        }?;

        Ok(mir::Expression {
            entity: mir::Entity::Function(nfid, params),
            r#type: returns,
        })
    }

    fn run_params(
        &mut self,
        params: &[Tr<hir::Entity>],
        infer_to: &[Tr<Tp<TemplateType>>],
    ) -> Result<(Vec<mir::Entity>, Vec<Tr<StaticType>>)> {
        let mut compiled = Vec::with_capacity(params.len());
        let mut compiled_types = Vec::with_capacity(params.len());

        // Compile the parameters
        for (idx, expr) in params
            .iter()
            .zip(infer_to.iter())
            .map(|(param, _infer_to)| (param.idx, self.run(param)))
        {
            let expr = expr?;
            compiled.push(expr.entity);
            compiled_types.push(Tr::tr(idx, expr.r#type));
        }

        Ok((compiled, compiled_types))
    }
}
