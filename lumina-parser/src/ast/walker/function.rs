use super::*;
use crate::ast::function::Header;
use lumina_util::{PFlags, Tr, Typing};
use std::borrow::Borrow;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    pub fn function(
        mut self,
        attributes: Vec<Attr>,
        decl_span: lumina_tokenizer::Span,
    ) -> Result<(), HandlerError<E>> {
        let func = self.func(attributes)?;
        self.module
            .handler
            .on_func(Tr::tr(decl_span, func))
            .map_err(HandlerError::Handler)?;
        self.headers(Vec::new())
    }
}

fn generate_infered_types<N>(params: &[Tr<N>]) -> Vec<Tr<Type>> {
    params
        .iter()
        .map(|tr| Tr::tr(tr.span.clone(), Type::Infer))
        .collect()
}

impl<'src, 'a, 'h, H, E> Builder<'src, 'a, 'h, H, E> {
    pub fn func_header(&mut self, attributes: Vec<Attr>) -> Result<Header, Error> {
        let pnames = self.pnames()?;
        let (Typing { ptypes, returns }, pflags) = self.infered_ptypes(&pnames)?;

        let mut bounds = Bounds::new();
        self.trait_bounds(&mut bounds)?;

        Ok(Header {
            ptypes,
            pnames,
            returns,
            pflags,
            bounds,
            attributes,
        })
    }

    /// We only really use the pnames for the spans, thus the `&[Tr<N>]`
    pub fn infered_ptypes<N>(&mut self, pnames: &[Tr<N>]) -> Result<(Typing<Type>, PFlags), Error> {
        self.skip_spaces()?;

        let (t, span) = self.view_spooky().map(View::consume).map(Tr::sep)?;

        match t {
            Token::FatArrow | Token::NewLines => Ok((
                Typing::new(generate_infered_types(pnames), Tr::tr(span, Type::Infer)),
                PFlags::new(),
            )),
            Token::OpenParen => self.parse_function_type(),
            _ => todo!("{:#?}", t),
        }
    }

    // Parses a function from tokens
    pub fn func(&mut self, attributes: Vec<Attr>) -> Result<Function, Error> {
        let name = self.func_name()?;
        let header = self.func_header(attributes)?;

        let body = self.body()?;

        Ok(Function {
            header,
            name: name.inner,
            body,
        })
    }

    // add
    pub fn func_name(&mut self) -> Result<Tr<String>, Error> {
        let t = self.progress()?;
        match &t.inner {
            Token::Identifier => {
                let name = self.take_span(t.span.clone());
                let location = Location::from_str(name)?;
                if !location.is_local {
                    Err(Error::IdentifierAsPath(location))
                } else {
                    Ok(Tr::tr(t.span.clone(), location.restore()))
                }
            }
            _ => Err(self.err_expected("an identifier for this function", t)),
        }
    }
    // x y
    pub fn pnames(&mut self) -> Result<Vec<Tr<String>>, Error> {
        let mut pnames = Vec::new();
        loop {
            let t = self.view_spooky()?;
            let span = t.span.clone();

            match t.borrow() {
                Token::Identifier => {
                    pnames.push(Tr::tr(span.clone(), self.take_span(span).to_string()))
                }
                Token::FatArrow => {
                    t.undo();
                    return Ok(pnames);
                }
                Token::NewLines => {
                    t.undo();
                    return Ok(pnames);
                }
                Token::Spaces => {
                    continue;
                }
                Token::OpenParen => {
                    t.undo();
                    break Ok(pnames);
                }
                _ => {
                    let t = t.consume();
                    break Err(self.err_expected("an identifier for this parameter", t));
                }
            }
        }
    }
    // x + y
    pub fn body(&mut self) -> Result<ast::function::Body, Error> {
        // Get the body
        let entity = self.expr()?;

        // Pick up on `where` keyword
        let peek = self.view()?;
        let wheres = if let Token::Where = peek.borrow() {
            // Parse all where bindings
            let span = peek.span.clone();
            self.where_statements().map_err(|e| e.idx(span))?
        } else {
            peek.undo();
            Vec::new()
        };

        Ok(ast::function::Body { wheres, entity })
    }

    // let x = ...
    // fn add x y (int, int -> int) ...
    // fn add x y
    //   ...
    fn where_statements(&mut self) -> Result<Vec<(Tr<String>, WhereBinding)>, Error> {
        let base_indentation = self.indentation_spaces;

        if base_indentation == 0 {
            return Err(Error::UnindentedWhereBinding);
        }

        let mut assignments = Vec::new();
        loop {
            // Update self.indentation_spaces in a terrible hacky way
            self.view()?.undo();

            if self.indentation_spaces < base_indentation {
                break;
            }
            let view = self.view()?;

            let (kind, sym) = match view.borrow() {
                Token::HeaderFn => self.where_fn_binding(),
                Token::Let => self.where_let_binding(),
                _ => {
                    let t = view.consume();
                    return Err(Error::WhereBindingMissingHeader(t.describe().to_string()));
                }
            }?;

            let entity = self.expr()?;

            assignments.push((sym, WhereBinding::new(kind, entity)))
        }

        Ok(assignments)
    }

    fn where_fn_binding(&mut self) -> Result<(WhereKind, Tr<String>), Error> {
        let name = self.func_name()?;
        let header = self.func_header(vec![])?;
        Ok((WhereKind::Fn(header), name))
    }

    fn where_let_binding(&mut self) -> Result<(WhereKind, Tr<String>), Error> {
        let name = self.func_name()?;

        let t = self.progress()?;
        if t.inner != Token::Equals {
            return Err(self.err_unexpected(t));
        }

        Ok((WhereKind::Let, name))
    }

    pub fn trait_bounds(&mut self, bounds: &mut Bounds<Type>) -> Result<(), Error> {
        let view = self.view()?;
        match view.borrow() {
            Token::When => {
                let (gen, loc, tp) = self.trait_bound()?;
                bounds.bind(gen, loc, tp);
                self.trait_bounds(bounds)
            }
            _ => {
                view.undo();
                Ok(())
            }
        }
    }

    fn trait_bound(&mut self) -> Result<(u8, Location, Vec<Tr<Type>>), Error> {
        let generic = self.progress()?;
        match &generic.inner {
            Token::Identifier => {
                let genname = self.take_span(generic.span.clone());
                let trait_ = self.progress()?;
                match &trait_.inner {
                    Token::Identifier => {
                        let traitname = self.take_span(trait_.span.clone());
                        let anots = self.parse_optional_type_parameters(TypeSettings::default())?;
                        let trait_ = Location::from_str(traitname)?;

                        if genname.len() != 1 {
                            return Err(
                                Error::InvalidGeneric(genname.to_string()).idx(generic.span)
                            );
                        }

                        let gen = genname.as_bytes()[0] - b'a';

                        Ok((gen, trait_, anots))
                    }
                    _ => Err(self.err_unexpected(trait_)),
                }
            }
            _ => Err(self.err_expected("a generic", generic)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::ast;
    use crate::ast::function::Body;

    static mut GARBAGE_CAN_HANDLER: () = ();

    fn trstr(s: &str) -> Tr<String> {
        Tr::new(s.to_string())
    }

    pub fn test_parser() -> ast::FileParser<'static, (), ()> {
        ast::FileParser::new(unsafe { &mut GARBAGE_CAN_HANDLER })
    }

    fn build_func(code: &str) -> Result<Function, Error> {
        let mut module = test_parser();
        let func = ast::Builder::from_str(&mut module, code).func(vec![])?;
        Ok(func)
    }

    #[test]
    fn body() {
        let mut module = test_parser();
        let body = ast::Builder::from_str(&mut module, "0").body().unwrap();
        assert_eq!(Entity::Inlined(Inlinable::Int(0)), body.entity.inner);
    }

    #[test]
    fn single_line_annotated() {
        let src = "add x y (int, int -> int) 0";
        let func = Error::test_failure(build_func(src), src);

        assert_eq!(
            func,
            Function {
                name: "add".into(),
                header: Header {
                    pnames: vec![trstr("x"), trstr("y")],
                    ptypes: vec![Tr::new(Type::Int), Tr::new(Type::Int)],
                    bounds: ast::Bounds::new(),
                    pflags: PFlags::new(),
                    returns: Tr::new(Type::Int),
                    attributes: vec![],
                },
                body: Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: Vec::new(),
                }
            }
        );
    }

    #[test]
    fn function_annotation() {
        let src = "add x y (int, int -> int)\n  0";
        let func = Error::test_failure(build_func(src), src);

        assert_eq!(
            func,
            Function {
                name: "add".into(),
                header: Header {
                    pnames: vec![trstr("x"), trstr("y")],
                    ptypes: vec![Tr::new(Type::Int), Tr::new(Type::Int)],
                    bounds: ast::Bounds::new(),
                    pflags: PFlags::new(),
                    returns: Tr::new(Type::Int),
                    attributes: vec![],
                },
                body: Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: Vec::new(),
                }
            }
        );
    }

    #[test]
    fn without_annotation() {
        let src = "add x y\n  0";
        let func = Error::test_failure(build_func(src), src);

        assert_eq!(
            func,
            Function {
                name: "add".into(),
                header: Header {
                    pnames: vec![trstr("x"), trstr("y")],
                    ptypes: vec![Tr::new(Type::Infer), Tr::new(Type::Infer)],
                    pflags: PFlags::new(),
                    bounds: ast::Bounds::new(),
                    returns: Tr::new(Type::Infer),
                    attributes: vec![],
                },
                body: Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: Vec::new(),
                }
            }
        );
    }

    #[test]
    fn trait_bounds() {
        let src = "inc x (n -> n)\n    when n Add int\n  0";
        let func = Error::test_failure(build_func(src), src);

        let mut bounds = ast::Bounds::new();
        bounds.bind(
            b'n' - b'a',
            Location::from_str("Add").unwrap(),
            vec![Tr::new(Type::Int)],
        );

        assert_eq!(
            func,
            Function {
                name: "inc".into(),
                header: Header {
                    pnames: vec![trstr("x")],
                    ptypes: vec![Tr::new(Type::Generic(b'n' - b'a', vec![]))],
                    pflags: PFlags::new(),
                    bounds,
                    returns: Tr::new(Type::Generic(b'n' - b'a', vec![])),
                    attributes: vec![],
                },
                body: Body {
                    entity: Tr::new(Entity::Inlined(Inlinable::Int(0))),
                    wheres: Vec::new(),
                }
            }
        );
    }
}
