use super::*;
use std::borrow::Borrow;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    pub fn r#struct(mut self, attributes: Vec<Attr>) -> Result<(), HandlerError<E>> {
        let t = self.parse_struct(attributes)?;
        self.module
            .handler
            .on_type(t)
            .map_err(HandlerError::Handler)?;
        self.headers(Vec::new())
    }
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    fn parse_struct(&mut self, attributes: Vec<Attr>) -> Result<DefType, Error> {
        let t = self.progress()?;

        let name = match &t.inner {
            Token::Identifier => self.take_span(t.span),
            _ => return Err(self.err_expected("an identifier for this struct", t)),
        };
        let location = Location::from_str(name)?;
        if !location.is_local {
            return Err(Error::IdentifierAsPath(location));
        }
        let name = location.restore();

        let type_parameters = self.declared_generics()?;

        let mut fields = Vec::new();
        loop {
            let peek = self.view()?;
            let span = peek.span.clone();

            match peek.borrow() {
                Token::Identifier => {
                    let name = self.take_span(span.clone());

                    let t = self.parse_type(TypeSettings {
                        allow_type_params: true,
                        type_scope: Some(type_parameters.as_slice()),
                    })?;

                    fields.push((Tr::tr(span, name.to_string()), t));
                }
                _ => {
                    peek.undo();
                    break Ok(DefType {
                        kind: TypeKind::Struct(Struct { fields }),
                        attributes,
                        type_parameters,
                        name: name.to_string(),
                        span: Ign::new(span),
                    });
                }
            }
        }
    }

    pub fn declared_generics(&mut self) -> Result<Vec<Tr<u8>>, Error> {
        let mut buf = Vec::new();

        loop {
            let token = self.view_spooky()?.consume();

            match token.inner {
                Token::NewLines => {
                    self.check_indentation_after_newline()?;
                    return Ok(buf);
                }
                Token::Spaces => continue,
                Token::EOF => break Ok(buf),
                Token::Identifier => {
                    let name = self.take_span(token.span.clone());
                    if name.len() != 1 {
                        return Err(Error::InvalidGeneric(name.to_string()).idx(token.span));
                    } else {
                        buf.push(Tr::tr(token.span.clone(), name.as_bytes()[0] - b'a'));
                    }
                }
                _ => break Err(self.err_unexpected(token)),
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
        let src = "both a b\n  left a\n  right b";

        let mut builder = Builder::from_str(&mut module, src);
        let attributes = vec![Attr::Linux];
        let dt = Error::test_failure(builder.parse_struct(attributes.clone()), src);

        let expected = DefType {
            kind: ast::TypeKind::Struct(ast::Struct {
                fields: vec![
                    (
                        Tr::new(String::from("left")),
                        Tr::new(Type::TypeParameter(0, 0, vec![])),
                    ),
                    (
                        Tr::new(String::from("right")),
                        Tr::new(Type::TypeParameter(1, 1, vec![])),
                    ),
                ],
            }),
            name: String::from("both"),
            span: Ign::new(0..0),
            type_parameters: vec![Tr::new(0), Tr::new(1)],
            attributes,
        };

        assert_eq!(dt, expected);
    }
}
