use super::*;
use crate::r#type::Trait;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    pub fn r#trait(mut self) -> Result<(), HandlerError<E>> {
        let base_indentation = self.indentation_spaces;

        let dt = self.parse_trait(base_indentation)?;

        self.module
            .handler
            .on_type(dt)
            .map_err(HandlerError::Handler)?;

        self.headers(Vec::new())
    }
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    pub fn parse_trait(&mut self, base_indentation: usize) -> Result<DefType, Error> {
        let (traitname, idx) = self.trait_name()?.sep();

        let mut requirements = HashMap::with_capacity(1);
        let type_parameters = self.declared_generics()?;

        let mut methodid = 0;

        loop {
            // HACK: We need to make sure that the indentation_spaces are up to date.
            // But, if we actually keep the value here then the borrow checker
            // won't allow us to grab the self.indentation_spaces.
            self.view()?.undo();
            if self.indentation_spaces <= base_indentation {
                break;
            }

            let view = self.view()?;
            match view.borrow() {
                Token::HeaderFn => {
                    let idx = view.span;
                    let fname = self.func_name()?;
                    let header = self.func_header(Vec::new())?;
                    requirements.insert(fname.inner, (Tr::new(header).set(idx), methodid));
                    methodid += 1;
                }
                Token::Attributes => {
                    let idx = view.span;
                    let attributes = self.attributes()?;
                    let fname = self.func_name()?;
                    let header = self.func_header(attributes)?;
                    requirements.insert(fname.inner, (Tr::new(header).set(idx), methodid));
                    methodid += 1;
                }
                Token::EOF => break,
                _ => {
                    let t = view.consume();
                    return Err(self.err_expected("method declerations for the trait", t));
                }
            }
        }

        Ok(DefType {
            kind: TypeKind::Trait(Trait::new(requirements)),
            attributes: Vec::new(),
            type_parameters,
            name: traitname,
            span: Ign::new(idx),
        })
    }

    pub fn trait_name(&mut self) -> Result<Tr<String>, Error> {
        let t = self.progress()?;
        match &t.inner {
            Token::Identifier => {
                Ok(Tr::new(self.take_span(t.span.clone()).to_string()).set(t.span))
            }
            _ => Err(self.err_expected("an identifier for this trait", t)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declerations() {
        let mut module = super::function::tests::test_parser();
        let src = "mytrait t\n  fn one a (int -> int)\n  fn two a (int -> int)";

        let dt = Error::test_failure(Builder::from_str(&mut module, src).parse_trait(0), src);

        let pnames = vec![Tr::new(String::from("a"))];
        let ptypes = vec![Tr::new(Type::Int)];
        let returns = Tr::new(Type::Int);

        let mut requirements = HashMap::new();

        requirements.insert(
            "one".to_string(),
            (
                Tr::new(ast::function::Header {
                    pnames: pnames.clone(),
                    ptypes: ptypes.clone(),
                    returns: returns.clone(),
                    attributes: vec![],
                    pflags: lumina_util::PFlags::new(),
                    bounds: ast::Bounds::new(),
                }),
                0,
            ),
        );
        requirements.insert(
            "two".to_string(),
            (
                Tr::new(ast::function::Header {
                    pnames,
                    ptypes,
                    returns,
                    attributes: vec![],
                    pflags: lumina_util::PFlags::new(),
                    bounds: ast::Bounds::new(),
                }),
                1,
            ),
        );

        let expected = DefType {
            kind: TypeKind::Trait(Trait::new(requirements)),
            attributes: vec![],
            type_parameters: vec![Tr::new(b't' - b'a')],
            name: String::from("mytrait"),
            span: Ign::new(0..0),
        };

        assert_eq!(dt, expected);
    }
}
