use super::*;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    pub fn r#enum(mut self, attributes: Vec<Attr>) -> Result<(), HandlerError<E>> {
        let t = self.parse_enum(attributes)?;
        self.module
            .handler
            .on_type(t)
            .map_err(HandlerError::Handler)?;
        self.headers(Vec::new())
    }
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    fn parse_enum(&mut self, attributes: Vec<Attr>) -> Result<DefType, Error> {
        let t = self.progress()?;

        let name = match &t.inner {
            Token::Identifier => self.take_span(t.span),
            _ => return Err(self.err_expected("an identifier for this enum", t)),
        };
        let location = Location::from_str(name)?;
        if !location.is_local {
            return Err(Error::IdentifierAsPath(location));
        }
        let name = location.restore();

        let type_parameters = self.declared_generics()?;

        let mut variants: Vec<(Tr<String>, Vec<Tr<Type>>)> = Vec::new();
        loop {
            let peek = self.view()?;

            match peek.borrow() {
                Token::Identifier => {
                    let span = peek.span.clone();
                    let constructor = self.take_span(span.clone());
                    let types = self.parse_optional_type_parameters(TypeSettings {
                        allow_type_params: false,
                        type_scope: Some(type_parameters.as_slice()),
                    })?;
                    variants.push((Tr::tr(span, constructor.to_string()), types));
                }
                _ => {
                    let span = peek.span.clone();
                    peek.undo();
                    break Ok(DefType {
                        kind: TypeKind::Enum(Enum { variants }),
                        attributes,
                        type_parameters,
                        name: name.to_string(),
                        span: Ign::new(span),
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decleration() {
        let mut module = super::function::tests::test_parser();
        let src = "result a b\n  ok a\n  err b";

        let mut builder = Builder::from_str(&mut module, src);
        let attributes = vec![Attr::Linux];
        let dt = Error::test_failure(builder.parse_enum(attributes.clone()), src);

        let expected = DefType {
            kind: ast::TypeKind::Enum(ast::Enum {
                variants: vec![
                    (
                        Tr::new(String::from("ok")),
                        vec![Tr::new(Type::TypeParameter(0, 0, vec![]))],
                    ),
                    (
                        Tr::new(String::from("err")),
                        vec![Tr::new(Type::TypeParameter(1, 1, vec![]))],
                    ),
                ],
            }),
            name: String::from("result"),
            span: Ign::new(0..0),
            type_parameters: vec![Tr::new(0), Tr::new(1)],
            attributes,
        };

        assert_eq!(dt, expected);
    }
}
