use super::*;
use lumina_util::{FlagList, PFlags, ParameterFlag, Typing};
use std::borrow::Borrow;

#[derive(Clone, Copy)]
pub struct TypeSettings<'t> {
    pub allow_type_params: bool,
    pub type_scope: Option<&'t [Tr<u8>]>,
}

impl<'t> Default for TypeSettings<'t> {
    fn default() -> Self {
        Self {
            allow_type_params: true,
            type_scope: None,
        }
    }
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    pub fn parse_type(&mut self, settings: TypeSettings<'_>) -> Result<Tr<Type>, Error> {
        let t = self.progress()?;
        match &t.inner {
            Token::TypeSelfStrict => {
                let type_params = if settings.allow_type_params {
                    self.parse_optional_type_parameters(settings)?
                } else {
                    Vec::new()
                };

                Ok(t.map(|_| Type::TraitSelf(type_params)))
            }
            Token::Identifier => {
                let name = self.take_span(t.span.clone());

                let type_params = if settings.allow_type_params {
                    self.parse_optional_type_parameters(settings)?
                } else {
                    Vec::new()
                };

                let r#type = match Type::simple(&name) {
                    Some(r#type) => {
                        if !type_params.is_empty() {
                            return Err(Error::TypeParameterOnPrimitive(r#type, type_params)
                                .idx(t.span.clone()));
                        }
                        r#type
                    }
                    None => {
                        if let Some(genid) = Type::try_generic(&name) {
                            match settings
                                .type_scope
                                .as_ref()
                                .and_then(|scope| scope.iter().position(|g| **g == genid))
                            {
                                Some(tpid) => Type::TypeParameter(tpid, genid, type_params),
                                None => Type::Generic(genid, type_params),
                            }
                        } else {
                            let location = Location::from_str(name)?;
                            Type::Defined(location, type_params)
                        }
                    }
                };

                Ok(t.map(|_| r#type))
            }
            Token::Operator => self.parse_pointer_type(t.span.clone()),
            Token::HeaderFn => self
                .parse_function_pointer_type()
                .map(|v| Tr::tr(t.span.clone(), v)),
            Token::OpenList => self.parse_list_type().map(|v| Tr::tr(t.span.clone(), v)),
            Token::OpenParen => self.parse_paren_type().map(|v| Tr::tr(t.span.clone(), v)),
            _ => Err(self.err_expected("a type", t)),
        }
    }

    fn parse_pointer_type(&mut self, span: Span) -> Result<Tr<Type>, Error> {
        let op = self.take_span(span.clone());
        if !op.bytes().all(|op| op == b'*') {
            return Err(Error::Unexpected(op.to_string()).idx(span));
        }

        let inner = self.parse_type(TypeSettings::default())?;
        let inner_span = inner.span.clone();

        let ptr = span.fold(inner, |t, ptr_indice| {
            Tr::tr(ptr_indice..inner_span.end, Type::Pointer(Box::new(t)))
        });

        Ok(ptr)
    }

    fn parse_function_pointer_type(&mut self) -> Result<Type, Error> {
        let open = self.progress()?;
        match &open.inner {
            Token::OpenParen => {}
            _ => {
                return Err(self.err_expected(
                    "a function annotation representing the function pointer",
                    open,
                ))
            }
        }

        let (typing, _pflags) = self.parse_function_type()?;
        Ok(Type::Function(typing.ptypes, Box::new(typing.returns)))
    }

    pub fn parse_optional_type_parameters(
        &mut self,
        settings: TypeSettings,
    ) -> Result<Vec<Tr<Type>>, Error> {
        let mut buf = Vec::new();

        loop {
            let view = self.view_spooky()?;
            match view.borrow() {
                Token::Spaces => continue,
                Token::NewLines => {
                    view.undo();
                    break Ok(buf);
                }
                Token::Identifier | Token::OpenList | Token::OpenParen => {
                    view.undo();
                    let type_param = self.parse_type(TypeSettings {
                        allow_type_params: false,
                        ..settings
                    })?;
                    buf.push(type_param)
                }
                _ => {
                    view.undo();
                    break Ok(buf);
                }
            }
        }
    }

    fn parse_list_type(&mut self) -> Result<Type, Error> {
        let inner = self.parse_type(TypeSettings::default())?;
        let t = self.progress()?;
        match &t.inner {
            Token::CloseList => Ok(Type::List(Box::new(inner))),
            Token::Comma => self.parse_tuple_type(inner),
            _ => Err(self.err_expected("a comma or an end to the list", t)),
        }
    }

    fn parse_tuple_type(&mut self, first: Tr<Type>) -> Result<Type, Error> {
        let mut types = Vec::with_capacity(2);
        types.push(first);
        loop {
            let inner = self.parse_type(TypeSettings::default())?;
            types.push(inner);

            let t = self.progress()?;
            match &t.inner {
                Token::Comma => continue,
                Token::CloseList => break Ok(Type::Tuple(types)),
                _ => break Err(self.err_expected("a comma or an end to the tuple", t)),
            }
        }
    }

    fn parse_remaining_function_type(
        &mut self,
        mut params: Vec<Tr<Type>>,
        mut pflags: PFlags,
    ) -> Result<(Typing<Type>, PFlags), Error> {
        loop {
            let p = self.parse_type(TypeSettings::default())?;
            params.push(p);

            let view = self.view()?;
            match view.borrow() {
                Token::Comma => {}
                Token::DoubleColon => {
                    let this = pflags.entry(params.len() - 1);
                    self.parse_pflags(this)?;
                    let after = self.progress()?;
                    match &*after {
                        Token::Arrow => {
                            let returns = self.function_returns(pflags.entry_return())?;
                            return Ok((Typing::new(params, returns), pflags));
                        }
                        Token::Comma => {}
                        _ => return Err(self.err_unexpected(after)),
                    }
                }
                Token::Arrow => {
                    let returns = self.function_returns(pflags.entry_return())?;
                    return Ok((Typing::new(params, returns), pflags));
                }
                Token::CloseParen => {
                    return Err(Error::MissingReturnType
                        .idx(params[0].span.start..view.span.clone().end - 1))
                }
                _ => {
                    let t = view.consume();
                    return Err(self.err_unexpected(t));
                }
            }
        }
    }

    fn function_returns(&mut self, pflag: &mut FlagList) -> Result<Tr<Type>, Error> {
        let returns = self.parse_type(TypeSettings::default())?;
        let after = self.progress()?;
        match &after.inner {
            Token::CloseParen => Ok(returns),
            Token::DoubleColon => {
                self.parse_pflags(pflag)?;
                let after = self.progress()?;
                match after.inner {
                    Token::CloseParen => Ok(returns),
                    _ => Err(self.err_expected(format!("`{}`", ")".purple()), after)),
                }
            }
            _ => Err(self.err_expected(format!("`{}`", ")".purple()), after)),
        }
    }

    // A "paren type" can either be a function `(a -> b)` or just be an ordinary type that we have
    // to parentehsize in order to have the type-params go to the correct thing `result (result a) e`.
    pub fn parse_paren_type(&mut self) -> Result<Type, Error> {
        // edge-case for functions with 0 parameters
        // `( -> int)`
        let view = self.view()?;
        match view.borrow() {
            Token::Arrow => {
                return self
                    .function_returns(PFlags::new().entry_return())
                    .map(|returns| Type::Closure(vec![], Box::new(returns)))
            }
            _ => view.undo(),
        }

        let t = self.parse_type(TypeSettings::default())?;
        let after = self.progress()?;
        match &after.inner {
            Token::Comma => {
                let (typing, _pflags) =
                    self.parse_remaining_function_type(vec![t], PFlags::new())?;

                // TODO: We might want to throw an error on pflags here. They're not allowed in
                // this context but instead of erroring they're just ignored.

                Ok(Type::Closure(typing.ptypes, Box::new(typing.returns)))
            }
            Token::CloseParen => Ok(t.inner),
            Token::Arrow => self
                .function_returns(PFlags::new().entry_return())
                .map(|returns| Type::Closure(vec![t], Box::new(returns))),
            _ => Err(self.err_unexpected(after)),
        }
    }

    // `a -> b)`
    pub fn parse_function_type(&mut self) -> Result<(Typing<Type>, PFlags), Error> {
        let first = self.parse_type(TypeSettings::default())?;
        let mut pflags = PFlags::new();

        // This code is really weird and confusing, and it's all because of the edge case regarding
        // the syntax sugar `(type)`
        loop {
            let after = self.progress()?;
            match &*after {
                Token::Comma => return self.parse_remaining_function_type(vec![first], pflags),
                Token::Arrow => {
                    let returns = self.function_returns(pflags.entry_return())?;
                    return Ok((Typing::new(vec![first], returns), pflags));
                }
                Token::DoubleColon => {
                    let dest = pflags.entry(0);
                    self.parse_pflags(dest)?;
                }

                // edge-case for `(type)`
                Token::CloseParen => {
                    pflags.swap_param_with_return(0);
                    return Ok((Typing::new(vec![], first), pflags));
                }

                _ => panic!(),
            }
        }
    }

    pub fn parse_pflags(&mut self, dst: &mut FlagList) -> Result<(), Error> {
        // We support both just a plain flag but also a list of flags
        let first = self.progress()?;
        match &*first {
            Token::OpenList => self.parse_pflag_list(dst)?,
            Token::Mut => dst.push(ParameterFlag::Mut),
            Token::Debug => dst.push(ParameterFlag::Debug),
            _ => return Err(Error::UnknownPFlag),
        }
        Ok(())
    }

    pub fn parse_pflag_list(&mut self, dst: &mut FlagList) -> Result<(), Error> {
        let t = self.progress()?;

        match &*t {
            Token::CloseList => Ok(()),
            Token::Mut => {
                dst.push(ParameterFlag::Mut);
                let t = self.progress()?;
                match &*t {
                    Token::CloseList => Ok(()),
                    Token::Comma => self.parse_pflag_list(dst),
                    _ => Err(self.err_expected("a comma or an end to the list", t)),
                }
            }
            Token::Debug => {
                dst.push(ParameterFlag::Debug);
                let t = self.progress()?;
                match &*t {
                    Token::CloseList => Ok(()),
                    Token::Comma => self.parse_pflag_list(dst),
                    _ => Err(self.err_expected("a comma or an end to the list", t)),
                }
            }
            _ => Err(self.err_expected("a pflag or an end to the list", t)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lumina_util::Location;

    #[test]
    fn primitive() {
        let mut module = super::function::tests::test_parser();
        let src = "int";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        assert_eq!(*t, Type::Int);
    }

    #[test]
    fn generic() {
        let mut module = super::function::tests::test_parser();
        let src = "a";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        assert_eq!(*t, Type::Generic(0, vec![]));
    }

    #[test]
    fn higher_kinded_generic() {
        let mut module = super::function::tests::test_parser();
        let src = "f a";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        assert_eq!(
            *t,
            Type::Generic(b'f' - b'a', vec![Tr::new(Type::Generic(0, vec![]))])
        );
    }

    #[test]
    fn list() {
        let mut module = super::function::tests::test_parser();
        let src = "[int]";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        assert_eq!(*t, Type::List(Box::new(Tr::new(Type::Int))));
    }

    #[test]
    fn user_defined() {
        let mut module = super::function::tests::test_parser();
        let src = "other";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        assert_eq!(
            *t,
            Type::Defined(Location::from_str("other").unwrap(), vec![])
        );
    }

    #[test]
    fn high_order_function() {
        let mut module = super::function::tests::test_parser();
        let src = "(int, int -> int)";

        let mut builder = ast::Builder::from_str(&mut module, src);
        let t = Error::test_failure(builder.parse_type(TypeSettings::default()), src);

        assert_eq!(
            *t,
            Type::Closure(
                vec![Tr::new(Type::Int), Tr::new(Type::Int)],
                Box::new(Tr::new(Type::Int))
            )
        );

        let mut builder = ast::Builder::from_str(&mut module, "( -> int)");
        let t = Error::test_failure(builder.parse_type(TypeSettings::default()), src);

        assert_eq!(*t, Type::Closure(vec![], Box::new(Tr::new(Type::Int))));
        assert_eq!(builder.progress().unwrap().inner, Token::EOF);
    }

    #[test]
    fn type_parameters() {
        let mut module = super::function::tests::test_parser();
        let src = "other int (other float float)";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );
        let loc = Location::from_str("other").unwrap();

        let expected = Type::Defined(
            loc.clone(),
            vec![
                Tr::new(Type::Int),
                Tr::new(Type::Defined(
                    loc,
                    vec![Tr::new(Type::Float), Tr::new(Type::Float)],
                )),
            ],
        );

        assert_eq!(*t, expected);
    }

    #[test]
    fn raw_function() {
        let mut module = super::function::tests::test_parser();
        let src = "fn((a -> b), int -> fn(int -> int))";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );

        let p0 = Tr::new(Type::Closure(
            vec![Tr::new(Type::Generic(0, vec![]))],
            Box::new(Tr::new(Type::Generic(1, vec![]))),
        ));
        let p1 = Tr::new(Type::Int);
        let ret = Type::Function(vec![Tr::new(Type::Int)], Box::new(Tr::new(Type::Int)));

        assert_eq!(
            t.inner,
            Type::Function(vec![p0, p1], Box::new(Tr::new(ret)))
        );
    }

    fn ptr(t: Type) -> Type {
        Type::Pointer(Box::new(Tr::new(t)))
    }

    #[test]
    fn raw_pointers() {
        let mut module = super::function::tests::test_parser();
        let src = "*(*int, *(*int -> **[*int]) -> *(*int -> **int))";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );

        let pint = Type::Pointer(Box::new(Tr::new(Type::Int)));

        let p0 = pint.clone();

        let p1 = ptr(Type::Closure(
            vec![Tr::new(pint.clone())],
            Box::new(Tr::new(ptr(ptr(Type::List(Box::new(Tr::new(
                pint.clone(),
            ))))))),
        ));

        let ret = ptr(Type::Closure(
            vec![Tr::new(pint.clone())],
            Box::new(Tr::new(ptr(pint))),
        ));

        let expected = ptr(Type::Closure(
            vec![Tr::new(p0), Tr::new(p1)],
            Box::new(Tr::new(ret)),
        ));

        assert_eq!(t.inner, expected);
    }

    #[test]
    fn function_with_type_params() {
        let mut module = super::function::tests::test_parser();
        let src = "(f a b, f a b -> f a b)";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );

        let f = Tr::new(Type::Generic(
            b'f' - b'a',
            vec![
                Tr::new(Type::Generic(0, vec![])),
                Tr::new(Type::Generic(1, vec![])),
            ],
        ));

        assert_eq!(
            t.inner,
            Type::Closure(vec![f.clone(), f.clone()], Box::new(f))
        );
    }

    #[test]
    fn complex() {
        let mut module = super::function::tests::test_parser();
        let src = "[(other int [other int float], int -> other (other [int] [float]) float)]";

        let t = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_type(TypeSettings::default()),
            src,
        );

        let loc = Location::from_str("other").unwrap();
        let expected = Type::List(Box::new(Tr::new(Type::Closure(
            vec![
                Tr::new(Type::Defined(
                    loc.clone(),
                    vec![
                        Tr::new(Type::Int),
                        Tr::new(Type::List(Box::new(Tr::new(Type::Defined(
                            loc.clone(),
                            vec![Tr::new(Type::Int), Tr::new(Type::Float)],
                        ))))),
                    ],
                )),
                Tr::new(Type::Int),
            ],
            Box::new(Tr::new(Type::Defined(
                loc.clone(),
                vec![
                    Tr::new(Type::Defined(
                        loc,
                        vec![
                            Tr::new(Type::List(Box::new(Tr::new(Type::Int)))),
                            Tr::new(Type::List(Box::new(Tr::new(Type::Float)))),
                        ],
                    )),
                    Tr::new(Type::Float),
                ],
            ))),
        ))));

        println!("GOT: {:#?}\n\nEXPECTED: {:#?}", &t.inner, &expected);

        assert_eq!(*t, expected);
    }

    #[test]
    fn pflags() {
        let mut module = super::function::tests::test_parser();
        let src = "int :: mut, [float] :: [mut, debug] -> (int -> int) :: mut)";

        let (typing, pflags) = Error::test_failure(
            ast::Builder::from_str(&mut module, src).parse_function_type(),
            src,
        );

        assert_eq!(
            typing,
            Typing {
                ptypes: vec![
                    Tr::new(Type::Int),
                    Tr::new(Type::List(Box::new(Tr::new(Type::Float))))
                ],
                returns: Tr::new(Type::Closure(
                    vec![Tr::new(Type::Int)],
                    Box::new(Tr::new(Type::Int))
                )),
            }
        );

        let mut expected = PFlags::new();
        expected.insert(0, ParameterFlag::Mut);
        expected.insert(1, ParameterFlag::Mut);
        expected.insert(1, ParameterFlag::Debug);
        expected.insert_return(ParameterFlag::Mut);

        assert_eq!(pflags, expected);
    }
}
