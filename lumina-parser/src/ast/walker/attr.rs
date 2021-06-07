use super::*;

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    pub fn attributes(&mut self) -> Result<Vec<Attr>, Error> {
        let first = self.progress()?;

        if first.inner != Token::OpenList {
            return Err(self.err_expected("a list of attributes", first));
        }

        let mut attributes = Vec::new();

        loop {
            let v = self.progress()?;
            match &v.inner {
                Token::CloseList => break Ok(attributes),
                Token::Identifier => {
                    let ident = self.take_span(v.span.clone());
                    let attr = Attr::try_from(&ident).map_err(|e| e.idx(v.span.clone()))?;
                    attributes.push(attr);
                }
                _ => break Err(self.err_expected("an attribute or end of list", v)),
            }
        }
    }
}
