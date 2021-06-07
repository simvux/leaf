use super::*;

use lumina_util::Typing;
use std::borrow::Borrow;

#[derive(Debug, PartialEq)]
struct Implementation {
    trait_: Tr<Location>,
    trait_type_params: TypeParameters,
    implementor: Tr<Type>,
    bounds: Bounds<Type>,
    methods: HashMap<String, Tr<ast::Function>>,
}

impl<'src, 'a, 'h, H: Handler<E>, E> Builder<'src, 'a, 'h, H, E> {
    pub fn implementation(
        mut self,
        decl_span: lumina_tokenizer::Span,
    ) -> Result<(), HandlerError<E>> {
        let base_indentation = self.indentation_spaces;

        let impl_ = self.parse_impl(base_indentation)?;

        self.module
            .handler
            .on_impl(
                decl_span,
                impl_.trait_,
                impl_.trait_type_params,
                impl_.bounds,
                impl_.implementor,
                impl_.methods,
            )
            .map_err(HandlerError::Handler)?;

        self.headers(Vec::new())
    }
}

impl<'src, 'a, 'h, H, E> Builder<'src, 'a, 'h, H, E> {
    fn parse_impl(&mut self, base_indentation: usize) -> Result<Implementation, Error> {
        let trait_ = self.trait_name()?.try_map(Location::from_string)?;

        let trait_type_params = self.parse_optional_type_parameters(TypeSettings::default())?;

        // Skip the `for`
        let for_ = self.progress()?;
        match &for_.inner {
            Token::For => {}
            _ => return Err(self.err_expected("`for` followed by a type", for_)),
        }

        let implementor = self.parse_type(TypeSettings::default())?;

        let mut bounds = Bounds::new();
        self.trait_bounds(&mut bounds)?;

        let mut implementing_functions = HashMap::new();

        loop {
            self.view()?.undo();
            if self.indentation_spaces <= base_indentation {
                break Ok(Implementation {
                    trait_,
                    trait_type_params,
                    implementor,
                    bounds,
                    methods: implementing_functions,
                });
            }
            let view = self.view()?;

            match view.borrow() {
                Token::Attributes => {
                    let attr = self.attributes()?;
                    let after = self.progress()?;
                    match &*after {
                        Token::HeaderFn => {
                            let fn_span = after.span.clone();
                            let func = self.impl_function(attr)?;
                            implementing_functions
                                .insert(func.name.clone(), Tr::new(func).set(fn_span));
                        }
                        _ => return Err(self.err_expected("a method for this trait", after)),
                    }
                }
                Token::HeaderFn => {
                    let fn_span = view.span.clone();
                    let func = self.impl_function(vec![])?;
                    implementing_functions.insert(func.name.clone(), Tr::new(func).set(fn_span));
                }
                Token::EOF => {
                    break Ok(Implementation {
                        trait_,
                        trait_type_params,
                        implementor,
                        bounds,
                        methods: implementing_functions,
                    })
                }
                _ => {
                    let v = view.consume();
                    return Err(self.err_unexpected(v));
                }
            }
        }
    }

    fn impl_function(&mut self, attributes: Vec<Attr>) -> Result<Function, Error> {
        let fname = self.func_name()?;

        let pnames = self.pnames()?;
        let (Typing { ptypes, returns }, pflags) = self.infered_ptypes(&pnames)?;

        let mut fbounds = Bounds::new();
        self.trait_bounds(&mut fbounds)?;

        let body = self.body()?;

        Ok(Function {
            header: ast::function::Header {
                pnames,
                returns,
                ptypes,
                bounds: fbounds,
                pflags,
                attributes,
            },
            name: fname.inner,
            body,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declerations() {
        let mut module = super::function::tests::test_parser();
        let src = "Into int for option int\n  fn into opt (self -> int)\n    0\n  fn other opt (option int -> int) 0";

        let impl_ = Error::test_failure(Builder::from_str(&mut module, src).parse_impl(0), src);

        let mut methods = HashMap::new();
        methods.insert(
            String::from("into"),
            Tr::new(ast::function::Function {
                name: String::from("into"),
                header: ast::function::Header {
                    pnames: vec![Tr::new("opt".to_string())],
                    ptypes: vec![Tr::new(Type::TraitSelf(vec![]))],
                    returns: Tr::new(Type::Int),
                    pflags: lumina_util::PFlags::new(),
                    bounds: Bounds::new(),
                    attributes: vec![],
                },
                body: ast::function::Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: vec![],
                },
            }),
        );
        methods.insert(
            String::from("other"),
            Tr::new(ast::function::Function {
                name: String::from("other"),
                header: ast::function::Header {
                    pnames: vec![Tr::new("opt".to_string())],
                    ptypes: vec![Tr::new(Type::Defined(
                        Location::from_str("option").unwrap(),
                        vec![Tr::new(Type::Int)],
                    ))],
                    returns: Tr::new(Type::Int),
                    pflags: lumina_util::PFlags::new(),
                    bounds: Bounds::new(),
                    attributes: vec![],
                },
                body: ast::function::Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: vec![],
                },
            }),
        );

        let expected = Implementation {
            trait_: Tr::new(Location::from_str("Into").unwrap()),
            trait_type_params: vec![Tr::new(Type::Int)],
            implementor: Tr::new(Type::Defined(
                Location::from_str("option").unwrap(),
                vec![Tr::new(Type::Int)],
            )),
            bounds: Bounds::new(),
            methods,
        };

        assert_eq!(impl_, expected);
    }
}
