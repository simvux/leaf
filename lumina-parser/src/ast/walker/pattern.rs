use super::*;

#[derive(Clone, Copy)]
pub struct Settings {
    pub followups: bool,
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    pub fn pattern(&mut self, settings: Settings) -> Result<Pattern, Error> {
        let t = self.progress()?;

        match &t.inner {
            Token::Identifier => {
                let wildcard = self.take_span(t.span.clone());
                self.pattern_followup(
                    Pattern::Wildcard(Tr::tr(t.span, wildcard.to_owned())),
                    settings,
                )
            }
            Token::Int(n) => self.pattern_followup(Pattern::Value(Inlinable::Int(*n)), settings),
            Token::OpenParen => self.pattern_parenthesis(),
            Token::OpenCurly => self.pattern_record_or_tuple(settings),
            Token::OpenList => self.pattern_list(),
            _ => Err(self.err_expected("a pattern", t)),
        }
    }

    pub fn pattern_parenthesis(&mut self) -> Result<Pattern, Error> {
        let p = self.pattern(Settings { followups: true })?;
        let after = self.progress()?;
        match &*after {
            Token::CloseParen => Ok(p),
            _ => Err(self.err_expected("a closing parenthesis", after)),
        }
    }

    /// Try to create a pattern but undo tokens if next is not a valid pattern
    pub fn try_pattern(&mut self, settings: Settings) -> Result<Option<Pattern>, Error> {
        let view = self.view()?;
        let span = view.span.clone();

        let t: &Token = view.borrow();

        match t.clone() {
            Token::Identifier => {
                let wildcard = self.take_span(span.clone());
                self.pattern_followup(
                    Pattern::Wildcard(Tr::tr(span, wildcard.to_owned())),
                    settings,
                )
                .map(Some)
            }
            Token::Int(n) => self
                .pattern_followup(Pattern::Value(Inlinable::Int(n)), settings)
                .map(Some),
            Token::OpenCurly => self.pattern_record_or_tuple(settings).map(Some),
            Token::OpenParen => self.pattern_parenthesis().map(Some),
            Token::OpenList => self.pattern_list().map(Some),
            _ => {
                view.undo();
                Ok(None)
            }
        }
    }

    fn pattern_followup(&mut self, left: Pattern, settings: Settings) -> Result<Pattern, Error> {
        if !settings.followups {
            return Ok(left);
        }
        let view = self.view()?;

        match view.borrow() {
            Token::If => self.pattern_if(left),
            Token::Identifier => {
                let span = view.span.clone();
                let v = view.consume();

                match self.take_span(span.clone()) {
                    "or" => self.pattern_or(left, settings),

                    // Hack for enum destructs
                    other => match left {
                        Pattern::Wildcard(ident) => self.pattern_enum(
                            ident,
                            vec![Pattern::Wildcard(Tr::tr(span, other.to_string()))],
                            settings,
                        ),
                        _ => Err(self.err_unexpected(v)),
                    },
                }
            }
            Token::Operator => {
                let span = view.span.clone();
                let t = {
                    let t: &Token = view.borrow();
                    t.clone()
                };

                match self.take_span(span.clone()) {
                    ".." => self.pattern_range(left, settings),
                    _ => Err(self.err_unexpected(Tr::tr(span, t))),
                }
            }
            _ => match left {
                // if the left was an identifier, it's possible that it's an enum destructor, so;
                // we cannot return here.
                Pattern::Wildcard(type_) => {
                    view.undo();
                    self.pattern_enum(type_, vec![], settings)
                }
                other => {
                    view.undo();
                    Ok(other)
                }
            },
        }
    }

    fn pattern_enum(
        &mut self,
        type_: Tr<String>,
        mut params: Vec<Pattern>,
        settings: Settings,
    ) -> Result<Pattern, Error> {
        // let mut params = Vec::new();

        // HACK: This module is written with the intention off ignoring whatever comes after the
        // pattern. But; for enum destruct we pick up patterns as parameters until we no longer
        // can.
        //
        // So; we just go until it encounters something unpatternable

        loop {
            match self.try_pattern(Settings { followups: false })? {
                Some(p) => params.push(p),
                None if params.is_empty() => return Ok(Pattern::Wildcard(type_)),
                None => {
                    let fold = Pattern::Enum(type_, params);
                    return self.pattern_followup(fold, settings);
                }
            };
        }
    }

    fn pattern_if(&mut self, left: Pattern) -> Result<Pattern, Error> {
        self.expr()
            .map(|condition| Pattern::If(Box::new(left), Box::new(condition)))
    }

    fn pattern_range(&mut self, left: Pattern, settings: Settings) -> Result<Pattern, Error> {
        self.pattern(settings)
            .map(|right| Pattern::Range(Box::new([left, right])))
    }

    fn pattern_or(&mut self, left: Pattern, settings: Settings) -> Result<Pattern, Error> {
        self.pattern(settings)
            .map(|right| Pattern::Or([Box::new(left), Box::new(right)]))
    }

    fn pattern_record_or_tuple(&mut self, settings: Settings) -> Result<Pattern, Error> {
        // HACK:
        //
        // Since our parser only allows look-ahead of one token we can't actually know whether this
        // is a tuple or a record.
        //
        // ```lumina
        // { add one two, 4 }
        // { point int . 4 }
        // ```
        //
        // So; we don't allow type-parameters on the type and rely on there being a dot on the
        // second token or not.
        //
        // But then we also have to hackily inject the now already consumed token into the
        // pattern_tuple parser
        let t = self.view()?;
        let identspan = t.span.clone();
        match t.borrow() {
            Token::Identifier => {
                let after = self.view()?;
                match after.borrow() {
                    Token::Dot => {
                        let ident = self.take_span(identspan).to_string();
                        self.pattern_record(ident)
                    }
                    Token::Comma => {
                        let wildcard = self.take_span(identspan.clone());
                        let fst = Pattern::Wildcard(Tr::tr(identspan, wildcard.to_string()));
                        self.pattern_tuple(vec![fst], settings)
                    }
                    _ => {
                        after.undo();
                        // Assume tuple but also start with the token `t`
                        let wildcard = self.take_span(identspan.clone());
                        let fst = self.pattern_followup(
                            Pattern::Wildcard(Tr::tr(identspan, wildcard.to_string())),
                            settings,
                        )?;
                        self.pattern_tuple(vec![fst], settings)
                    }
                }
            }
            _ => {
                t.undo();
                self.pattern_tuple(vec![], settings)
            }
        }
    }

    fn pattern_tuple(
        &mut self,
        mut assignments: Vec<Pattern>,
        settings: Settings,
    ) -> Result<Pattern, Error> {
        // edge-case for empty tuple
        let view = self.view()?;
        match view.borrow() {
            Token::CloseCurly => return Ok(Pattern::Tuple(assignments)),
            _ => view.undo(),
        }

        loop {
            let p = self.pattern(Settings { followups: true })?;
            assignments.push(p);

            let t = self.progress()?;

            match &t.inner {
                Token::Comma => continue,
                Token::CloseCurly => {
                    break self.pattern_followup(Pattern::Tuple(assignments), settings)
                }
                _ => break Err(self.err_unexpected(t)),
            }
        }
    }

    // NOTE: Assumes `Token::Dot` has already been consumed
    fn pattern_record(&mut self, ident: String) -> Result<Pattern, Error> {
        let mut fields = Vec::new();

        // edge-case for empty record
        let view = self.view()?;
        match view.borrow() {
            Token::CloseCurly => return Ok(Pattern::Record(ident, fields)),
            _ => view.undo(),
        }

        loop {
            let fieldname = {
                let t = self.progress()?;
                match &*t {
                    Token::Identifier => Tr::tr(t.span.clone(), self.take_span(t.span).to_string()),
                    _ => return Err(self.err_expected("a field for the struct", t)),
                }
            };

            // edge-case for when we omit an expression in favor of using a value with the same name
            let t = self.view()?;
            match t.borrow() {
                Token::Comma => {
                    fields.push((fieldname.clone(), Pattern::Wildcard(fieldname)));
                    continue;
                }
                Token::CloseCurly => {
                    fields.push((fieldname.clone(), Pattern::Wildcard(fieldname)));
                    break Ok(Pattern::Record(ident, fields));
                }
                _ => t.undo(),
            }

            let p = self.pattern(Settings { followups: true })?;
            fields.push((fieldname, p));

            let t = self.progress()?;

            match &t.inner {
                Token::Comma => continue,
                Token::CloseCurly => break Ok(Pattern::Record(ident, fields)),
                _ => break Err(self.err_unexpected(t)),
            }
        }
    }

    fn pattern_list(&mut self) -> Result<Pattern, Error> {
        let mut list = Vec::new();

        // edge-case for empty lists
        let view = self.view()?;
        match view.borrow() {
            Token::CloseList => return Ok(Pattern::List(list)),
            _ => view.undo(),
        }

        loop {
            let p = self.pattern(Settings { followups: true })?;
            list.push(p);

            let t = self.progress()?;

            match &t.inner {
                Token::Comma => continue,
                Token::DoubleColon => break self.pattern_list_remaining(list),
                Token::CloseList => break Ok(Pattern::List(list)),
                _ => break Err(self.err_expected("a comma or an end to the list", t)),
            }
        }
    }

    fn pattern_list_remaining(&mut self, list: Vec<Pattern>) -> Result<Pattern, Error> {
        let t = self.progress()?;

        match &t.inner {
            Token::Identifier => {
                let ident = Tr::tr(t.span.clone(), self.take_span(t.span).to_string());

                let after = self.progress()?;
                match &*after {
                    Token::CloseList => Ok(Pattern::ListRemaining(list, ident)),
                    _ => Err(self.err_expected(format!("`{}`", "]".purple()), after)),
                }
            }
            _ => Err(self.err_unexpected(t)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn i(n: i64) -> Pattern {
        Pattern::Value(Inlinable::Int(n))
    }

    #[test]
    fn list() {
        let mut module = super::function::tests::test_parser();
        let src = "[] [1, 2] [1, 2 :: xs] [x :: xs]";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(v, Pattern::List(vec![]));

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(v, Pattern::List(vec![i(1), i(2)]));

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(
            v,
            Pattern::ListRemaining(vec![i(1), i(2)], Tr::new(String::from("xs")))
        );

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(
            v,
            Pattern::ListRemaining(
                vec![Pattern::Wildcard(Tr::new(String::from("x")))],
                Tr::new(String::from("xs"))
            )
        );
    }

    #[test]
    fn or() {
        let mut module = super::function::tests::test_parser();
        let src = "4 or 2";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(v, Pattern::Or([Box::new(i(4)), Box::new(i(2))]));
    }

    #[test]
    fn record_and_tuple() {
        let mut module = super::function::tests::test_parser();
        let src = "{ triplet . x, y {4, [2]}, z }";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(
            v,
            Pattern::Record(
                String::from("triplet"),
                vec![
                    (Tr::new("x".into()), Pattern::Wildcard(Tr::new("x".into()))),
                    (
                        Tr::new("y".into()),
                        Pattern::Tuple(vec![i(4), Pattern::List(vec![i(2)])])
                    ),
                    (Tr::new("z".into()), Pattern::Wildcard(Tr::new("z".into())))
                ]
            )
        );
    }

    #[test]
    fn enum_destruct() {
        let mut module = super::function::tests::test_parser();
        let src = "just 0";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(v, Pattern::Enum(Tr::new(String::from("just")), vec![i(0)]));
    }

    #[test]
    fn nested_enums() {
        let mut module = super::function::tests::test_parser();
        let src = "just (just 0)";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);
        assert_eq!(
            v,
            Pattern::Enum(
                Tr::new(String::from("just")),
                vec![Pattern::Enum(Tr::new(String::from("just")), vec![i(0)])]
            )
        );
    }

    #[test]
    fn complex() {
        let mut module = super::function::tests::test_parser();
        let src = "just { point . x [1 or 2, 2 :: xs], y {4, 2} } {4, 2} if true";

        let mut builder = Builder::from_str(&mut module, src);

        let v = Error::test_failure(builder.pattern(Settings { followups: true }), src);

        let tuple = Pattern::Tuple(vec![i(4), i(2)]);
        let list = Pattern::ListRemaining(
            vec![Pattern::Or([Box::new(i(1)), Box::new(i(2))]), i(2)],
            Tr::new(String::from("xs")),
        );
        let record = Pattern::Record(
            String::from("point"),
            vec![
                (Tr::new("x".into()), list),
                (Tr::new("y".into()), tuple.clone()),
            ],
        );
        let destruct = Pattern::Enum(Tr::new(String::from("just")), vec![record, tuple]);
        let guard = Pattern::If(
            Box::new(destruct),
            Box::new(Tr::new(ast::Entity::Inlined(Inlinable::Bool(true)))),
        );

        assert_eq!(v, guard);
    }
}
