use super::*;

impl<'calls, 'ast, 'hir> function::Pass<'calls, 'ast, 'hir> {
    pub fn local(
        &mut self,
        name: &str,
        params: &[Tr<ast::Entity>],
        inf: Expect<'_>,
    ) -> Result<Option<hir::Entity>> {
        let f = &mut self.current.as_mut().unwrap();

        if let Some(pid) = f.func.header.pnames.iter().position(|n| **n == name) {
            let hir_params = self.type_check_parameter_with_inference(pid, params, inf)?;

            return Ok(Some(hir::Entity::Identifier(
                hir::function::Identifier::Param(pid),
                hir_params,
            )));
        }

        debug!(
            "warning",
            "locals other than params are ignored because they aren't implemented"
        );

        Ok(None)
    }
}
