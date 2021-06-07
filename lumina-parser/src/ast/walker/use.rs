use super::*;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    pub fn r#use(mut self) -> Result<(), HandlerError<E>> {
        let t = self.progress()?;
        let import_path = match &t.inner {
            Token::Identifier => {
                Tr::new(Location::from_str(self.take_span(t.span.clone()))?).set(t.span.clone())
            }
            _ => return Err(self.err_expected("an import path", t).into()),
        };

        let assign_to = {
            let t = self.view()?;
            let idx = t.span.clone();
            let v = match t.borrow() {
                Token::Identifier => self.take_span(idx.clone()).to_string(),
                _ => {
                    t.undo();
                    import_path.inner.last().to_string()
                }
            };
            Tr::new(v).set(idx)
        };

        let mut exposing = Vec::new();
        let t = self.view()?;
        match t.borrow() {
            Token::OpenList => loop {
                let t = self.progress()?;
                match &t.inner {
                    Token::Identifier => exposing
                        .push(Tr::new(self.take_span(t.span.clone()).to_string()).set(t.span)),
                    Token::CloseList => break,
                    _ => {
                        return Err(self
                            .err_expected("more identifiers to expose or an end to the list", t)
                            .into())
                    }
                }
            },
            _ => t.undo(),
        }

        self.module
            .handler
            .on_use(assign_to, import_path, exposing)?;

        self.headers(Vec::new())
    }
}
