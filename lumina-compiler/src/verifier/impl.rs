use super::*;

impl<'ast, 'hir> Verifier<'ast, 'hir> {
    pub fn index_impl(&mut self, implid: usize, impl_: &'ast collector::ImplTemplate) {
        let mut tv = r#type::TypeVerifier::new(
            self.copy(),
            ImplTypeInit {
                impl_,
                stage: ImplStage::Header,
            },
        );
        let trait_tp = tv.verify_types(&impl_.trait_tp).expect("ET");

        let ast_trait_loc = &impl_.trait_;
        let ast_trait_loc_abs = self.expand(&impl_.origin, &ast_trait_loc.inner);

        let trid = self
            .ast
            .traits
            .resolve(&ast_trait_loc_abs)
            .ok_or(r#type::TypeVerificationError::TraitNotFound(
                ast_trait_loc_abs,
            ))
            .map_err(|e| e.position(ast_trait_loc.span.clone()))
            .expect("ET");

        let trait_ = self.ast.traits.get_from_idx(trid).unwrap();

        for (mname, method) in impl_.impl_.iter() {
            let mut tv = r#type::TypeVerifier::new(
                self.copy(),
                ImplTypeInit {
                    impl_,
                    stage: ImplStage::Method(&method.header),
                },
            );

            let (method_template, mid) = trait_
                .body
                .functions
                .get(mname)
                .expect("ET: Method not part of trait");

            // TODO: is there a reason we don't turn ast::Function into collector::Function for the
            // methods?
            let (ptypes, returns, header_error) =
                tv.typing_from_ast(&method.header.ptypes, method.header.returns.as_ref());

            let hir_method_typing = hir::function::Typing::new(ptypes, returns);

            if !header_error.is_empty() {
                // on the ordinary function we early-return with an poisoned Function instead.
                // i guess we perhaps should insert the result as poisoned or something?
                //
                // could just be an enum like
                //
                // Implementation {
                //  Poisoned,
                //  Real(usize),
                // }
                // TraitImpls<Implementation>
                //
                // TODO: ^ preferably we'd only poison this specific method rather than the entire implementation.
                //
                // TODO: We've already got this! Because; hir::function::Header which I assume we
                // can use has a Poisoned variant!

                for err in header_error.into_iter() {
                    let luminerr = self.hackily_load_error(&impl_.origin, err);
                    println!(
                        "WE'RE ABORTING BECAUSE OF TODO, HERE's THE ERR: \n{}",
                        luminerr
                    );
                    self.errors.push(luminerr);
                }

                // let header = hir::function::Header::Failed(hir_method_typing);
                todo!("early-return a poisoned variant");
            }

            // TODO: tbh we should make our own typing_from_ast here.
            //
            // that way we can instead generate Tp<hir::Type> rather than ITp<hir::Type>
            // and also verify compatibility on-the-fly.
            let known_hir_method_typing = todo!("verify compatibility");
            let header = hir::function::Header::Known(known_hir_method_typing);

            // we can't just direct-compare hir_method_typing with method_template because the
            // implementor missmatches. We need a special function that walks em. (kinda anoying).
            //
            // Actually; I think the api already has this sort of `walk_together_and_visit` thing.
            //
            // TODO:
            // method_template is in AST form. I think we need a self.run_all_traits or some shit
            // to generate the method headers in HIR. But; i have no idea how that should look
            // like. As in; where are we storing the results?
            // maybe `mod:some_module:Functor:map` -> `hir::function::Kind::Method`?
            // That would make sense but then how will we reach it from *here*? perhaps that enum
            // should hold indice to another field in hir::Storage like `methods: HashMap<(TRID, MID), hir::function::Header>`
            //
            // ACTUALLY; ideally we should in the collector make it so that we link up
            // `mod:the-callable-identifier:Functor:map` to the method (or are we already doing that?)
            // oh we are already doing that. we're currently linking up
            // `mod:Functor:map` -> `{ trait_: Rc<Location>, mid: usize }`
            // perhaps we just need to make `HashMap<(TRID, MID)>` then? the problem is that we want
            // to index those headers (so we may use them here) *before* we actually check the
            // bodies.
            // actually; we could totally just store a `traits: HashMap<TRID, { methods: Vec<ast::function::Header>, .. }>`
            //  ^ although if we do that then we've got the ugly problem that we're allocating and have to redo it.
            //  Although; since the next step is getting them as FKey's regardless I think it's fine
            todo!("verify the types of method and then verify that they're compatible with method_template+impltor");
        }
    }
}

struct ImplTypeInit<'ast> {
    impl_: &'ast collector::ImplTemplate,
    stage: ImplStage<'ast>,
}

#[derive(Clone, Copy)]
enum ImplStage<'ast> {
    Header,
    Method(&'ast ast::function::Header),
}

impl<'ast> r#type::UniqueHandler for ImplTypeInit<'ast> {
    type Root = hir::Type;
    type Tree = Tp<hir::Type>;

    fn generic(
        &mut self,
        mut verifier: Verifier<'_, '_>,
        gid: u8,
        _: &lumina_parser::TypeParams,
    ) -> Result<hir::Type, r#type::TypeVerificationError> {
        if let ImplStage::Method(header) = self.stage {
            // we prioritise more local generic bounds such as those included on the method itself.
            //
            // TODO: I think we actually need to merge so we've got *both* the bounds of the
            // trait and the bounds of the method if the same `gid` exists in both places.
            if let Some(bound) = header.bounds.get(&gid) {
                /*
                let bounds = unique_handler_verify_bound(
                    verifier.copy(),
                    &self.impl_.origin,
                    bound,
                    || ImplTypeInit {
                        impl_: &self.impl_,
                        stage: self.stage,
                    },
                )?;

                return Ok(hir::Type::bound(gid, bounds));
                */
                todo!();
            }
        }

        /*
        unique_handler_verify_bounds(
            gid,
            verifier.copy(),
            &self.impl_.origin,
            &self.impl_.bounds,
            || ImplTypeInit {
                impl_: &self.impl_,
                stage: self.stage,
            },
        )
        */
        todo!();
    }

    fn trait_self(
        &mut self,
        mut verifier: Verifier<'_, '_>,
        tp: Vec<Tr<Tp<hir::Type>>>,
    ) -> Result<Tp<hir::Type>, r#type::TypeVerificationError> {
        match &self.stage {
            ImplStage::Header => Err(r#type::TypeVerificationError::SelfNotAllowed),
            ImplStage::Method(_header) => {
                let mut rec = r#type::TypeVerifier::new(
                    verifier.copy(),
                    ImplTypeInit {
                        impl_: self.impl_,
                        stage: self.stage,
                    },
                );

                let impltor = rec
                    .verify_type(&self.impl_.impltor)
                    .map_err(|e| e.position(self.impl_.impltor.span.clone()))?;

                match (impltor.params.len(), tp.len()) {
                    (e, 0) if e > 0 => {} // using `self` when impltor is `option a` is allowed
                    (e, g) if e > 0 && g == e => {
                        // when explicitely setting `self`s parameters they need to exactly match
                        // the impltor in the header.
                        if impltor.params != tp {
                            panic!("wrong parameters for `self`");
                        }
                    }
                    (_, _) => {
                        panic!("ET: Wrong amount of parameters");
                    }
                }

                Ok(impltor)
            }
        }
    }

    fn combinator(root: Self::Root, params: Vec<Tr<Self::Tree>>) -> Self::Tree {
        Tp::new(root, params)
    }
}

impl<'ast, 'hir> r#type::TypeVerifier<'ast, 'hir, ImplTypeInit<'ast>> {
    fn method_typing_from_ast(
        &mut self,
        // TODO: here we want to pass the hir template.
    ) -> (hir::function::Header, Vec<r#type::TypeVerificationError>) {
        // let mut errors = Vec::new();

        todo!();
    }
}
