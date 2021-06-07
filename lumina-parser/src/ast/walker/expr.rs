use super::*;
use lumina_util::Tr;
use std::borrow::Borrow;

impl<'src, 'a, 'h, H, E> Builder<'src, 'a, 'h, H, E> {
    pub fn expr(&mut self) -> Result<Tr<Entity>, Error> {
        let t = self.view()?;
        let i = t.span.clone();

        let v = match t.borrow() {
            Token::Todo => Entity::Unimplemented,
            Token::Int(n) => Entity::Inlined(Inlinable::Int(*n)),
            Token::Float(n) => Entity::Inlined(Inlinable::Float(*n)),
            Token::BoolTrue => Entity::Inlined(Inlinable::Bool(true)),
            Token::BoolFalse => Entity::Inlined(Inlinable::Bool(true)),
            Token::Underscore => Entity::Inlined(Inlinable::Nothing),
            Token::Pass => self.pass()?,
            Token::OpenParen => self.open_paren()?,
            Token::PipeLeft => self.expr()?.inner,
            Token::Let => self.letbind()?,
            Token::If => self.ifexpr()?,
            Token::Match => self.matchexpr()?,
            Token::First => self.first_stm()?,
            Token::OpenCurly => self.record()?,
            Token::Lambda => self.lambda()?,
            Token::StringLiteral => {
                todo!("get literal from span and construct");
            }
            Token::OpenList => self.list()?,
            Token::Identifier => {
                let name = self.take_span(i.clone());
                self.identifier(Tr::tr(i.clone(), name.to_string()))?
            }
            Token::Operator => {
                let name = self.take_span(i);
                return Err(Error::OperatorMissingLeftSide(name.to_string()));
            }
            _ => {
                let t = t.consume();
                return Err(self.err_unexpected(t));
            }
        };

        self.maybe_exprfollowup(Tr::new(v).set(i))
    }

    fn parameters(&mut self, mut buf: Vec<Tr<Entity>>) -> Result<Vec<Tr<Entity>>, Error> {
        if let Some(entity) = self.maybe_single_value()? {
            hackily_offload_lambda_params(entity, &mut buf);
            return self.parameters(buf);
        }
        Ok(buf)
    }

    fn pass(&mut self) -> Result<Entity, Error> {
        match self.maybe_single_value()? {
            Some(to_pass) => Ok(Entity::Pass(Box::new(to_pass.inner))),
            None => Err(Error::PassMissingRightSide),
        }
    }

    // Gets a singular value
    fn maybe_single_value(&mut self) -> Result<Option<Tr<Entity>>, Error> {
        let t = self.view()?;
        let span = t.span.clone();

        let v = match t.borrow() {
            Token::Todo => Entity::Unimplemented,
            Token::Int(n) => Entity::Inlined(Inlinable::Int(*n)),
            Token::Float(n) => Entity::Inlined(Inlinable::Float(*n)),
            Token::StringLiteral => {
                let s = self.take_span(span.clone());
                Entity::Inlined(Inlinable::String(s.to_string()))
            }
            Token::Underscore => Entity::Inlined(Inlinable::Nothing),
            Token::OpenList => self.list()?,
            Token::OpenParen => self.open_paren()?,
            Token::PipeLeft => self.expr()?.inner,
            Token::OpenCurly => self.record()?,
            Token::Pass => self.pass()?,
            Token::Identifier => {
                // let loc = Location::from_string(name)?;
                let name = self.take_span(span.clone());

                match self.maybe_dot_param(Tr::new(name.to_string()).set(span.clone()))? {
                    Either::Right(no_change) => ast::Entity::Identifier {
                        takes: Location::from_string(no_change.inner)?,
                        params: vec![],
                    },
                    Either::Left(wrapped) => wrapped.inner,
                }
            }
            Token::EOF
            | Token::As
            | Token::Operator
            | Token::Else
            | Token::HeaderFn
            | Token::HeaderUse
            | Token::HeaderEnum
            | Token::HeaderTrait
            | Token::HeaderStruct
            | Token::HeaderImpl
            | Token::Dot
            | Token::Comma
            | Token::Where
            | Token::Arrow
            | Token::CloseList
            | Token::In
            | Token::CloseCurly
            | Token::Then
            | Token::Attributes
            | Token::Next
            | Token::With
            | Token::CloseParen => {
                t.undo();
                return Ok(None);
            }
            _ => {
                let t = t.consume();
                return Err(self.err_unexpected(t));
            }
        };
        Ok(Some(Tr::new(v).set(span)))
    }

    fn lambda(&mut self) -> Result<Entity, Error> {
        let mut param_names = Vec::new();
        loop {
            let t = self.progress()?;
            match &*t {
                Token::Identifier => {
                    let name = self.take_span(t.span.clone());
                    param_names.push(Tr::tr(t.span, name.to_string()))
                }
                Token::Arrow => break,
                _ => {
                    return Err(self.err_expected(
                        format!(
                            "a parameter identifier for the lambda or `{}`",
                            "->".green()
                        ),
                        t,
                    ))
                }
            }
        }
        let v = self.expr()?;
        let params = self.parameters(vec![])?;
        Ok(Entity::Lambda {
            param_names,
            params,
            body: Box::new(v),
        })
    }

    fn open_paren(&mut self) -> Result<Entity, Error> {
        let inner = self.expr()?;
        let after = self.progress()?;
        match &after.inner {
            Token::CloseParen => match inner.inner {
                // Bit of a hacky solution. But basically,
                // `(\n -> n) 0` here `0` needs to be a part of the Entity::Lambda{params}
                // And the only way to accomplish that is to deconstruct here.
                //
                // The `params.len()` check is to fix recursive lambdas, since we don't want to
                // apply this hack multiple times for the most inner'st lambda.
                Entity::Lambda {
                    body,
                    param_names,
                    params,
                } if params.is_empty() => {
                    let params = self.parameters(params)?;
                    Ok(Entity::Lambda {
                        body,
                        param_names,
                        params,
                    })
                }
                other => Ok(other),
            },
            _ => Err(self.err_expected(")".purple().to_string(), after)),
        }
    }

    fn identifier(&mut self, name: Tr<String>) -> Result<Entity, Error> {
        let takes = Location::from_string(name.inner)?;

        // edge-case for `f.x`
        let takes = if takes.is_local {
            match self.maybe_dot_param(Tr::tr(name.span, takes.restore()))? {
                Either::Right(Tr { inner, .. }) => Location::from_string(inner).unwrap(),
                Either::Left(wrapped) => {
                    // We return directly here because `f.x` cannot be given more parameters
                    // without it being more `.` applications. And those are already handled
                    // recursively.
                    return Ok(wrapped.inner);
                }
            }
        } else {
            takes
        };

        let params = self.parameters(vec![])?;
        let entity = Entity::Identifier { takes, params };

        Ok(entity)
    }

    fn maybe_dot_param(
        &mut self,
        left: Tr<String>,
    ) -> Result<Either<Tr<Entity>, Tr<String>>, Error> {
        let view = self.view_spooky()?;
        match view.borrow() {
            Token::Dot => {
                let span = view.span.clone();
                let right = match self.maybe_single_value()? {
                    None => return Err(Error::PassMissingRightSide.idx(span)),
                    Some(right) => right,
                };

                let (left, idx) = left.sep();

                let complete = Entity::DotCall {
                    left,
                    right: Box::new(right),
                };

                Ok(Either::Left(Tr::tr(idx, complete)))
            }
            _ => {
                view.undo();
                Ok(Either::Right(left))
            }
        }
    }

    // (returns-func 20) 30
    //                   ^
    // hello + 20
    //       ^
    fn maybe_exprfollowup(&mut self, left: Tr<Entity>) -> Result<Tr<Entity>, Error> {
        let lidx = left.span.clone();
        let inner = self.maybe_operator(left)?;

        let params = self.parameters(vec![])?;
        if params.is_empty() {
            Ok(inner)
        } else {
            Ok(inner.map(|takes| Entity::ExpressionCall {
                takes: Tr::tr(lidx, Box::new(takes)),
                params,
            }))
        }
    }

    // hello + 20
    //       ^
    //
    // hello as int
    //       ^
    fn maybe_operator(&mut self, left: Tr<Entity>) -> Result<Tr<Entity>, Error> {
        let peek = self.view()?;
        match peek.borrow() {
            Token::As => {
                let span = peek.span.clone();
                let target = self.parse_type(TypeSettings::default())?;
                self.maybe_operator(Tr::tr(span, Entity::Cast(left.map(Box::new), target)))
            }
            Token::Operator => {
                let pspan = peek.span.clone();

                // HACK: edge-case for `|` as a match branch seperator
                peek.undo();
                if self.take_span(pspan.clone()) == "|" {
                    return Ok(left);
                } else {
                    self.progress().unwrap();
                }

                let operation = self.handle_operator(left, pspan)?;
                self.maybe_operator(operation)
            }
            _ => {
                peek.undo();
                Ok(left)
            }
        }
    }

    fn handle_operator(&mut self, left: Tr<Entity>, op: Span) -> Result<Tr<Entity>, Error> {
        let name = self.take_span(op.clone());
        let takes = Location::from_str(name)?;

        let right = self.expr()?;
        let operation = Tr::new(Entity::Identifier {
            takes,
            params: vec![left, right],
        })
        .set(op);
        self.maybe_operator(operation)
    }

    fn record(&mut self) -> Result<Entity, Error> {
        let to_mod = self.expr()?;

        let t = self.progress()?;
        match &t.inner {
            // oh wait, this isn't even a record. It's a tuple!
            Token::Comma => {
                return self.tuple(vec![to_mod]);
            }
            Token::CloseCurly => return Ok(Entity::Tuple(vec![to_mod])),

            // alright it's definitely a record
            Token::Dot => {}

            _ => return Err(self.err_unexpected(t)),
        }

        let mut assignments = Vec::new();
        loop {
            let t = self.progress()?;
            match &t.inner {
                Token::Identifier => {
                    let name = self.take_span(t.span.clone());
                    let v = self.expr()?;
                    assignments.push((name.to_string(), v));

                    let after = self.progress()?;
                    match &after.inner {
                        Token::Comma => {
                            // We allow trailing commas, to make new-lines look fancy in records
                            let t = self.view()?;
                            if Token::CloseCurly == *t.borrow() {
                                break Ok(Entity::Record(to_mod.map(Box::new), assignments));
                            }
                            t.undo();
                            continue;
                        }
                        Token::CloseCurly => {
                            break Ok(Entity::Record(to_mod.map(Box::new), assignments))
                        }
                        _ => {
                            break Err(self.err_expected("a comma or an end to the record", after));
                        }
                    }
                }
                _ => break Err(self.err_expected("a field name", t)),
            }
        }
    }

    fn tuple(&mut self, mut entries: Vec<Tr<Entity>>) -> Result<Entity, Error> {
        loop {
            let expr = self.expr()?;
            entries.push(expr);

            let t = self.progress()?;
            match &*t {
                Token::Comma => continue,
                Token::CloseCurly => break Ok(Entity::Tuple(entries)),
                _ => return Err(self.err_expected("a comma or an end to the tuple", t)),
            }
        }
    }

    fn list(&mut self) -> Result<Entity, Error> {
        let mut entries = Vec::new();

        let peek = self.view()?;
        match peek.borrow() {
            Token::CloseList => return Ok(Entity::List(entries)),
            _ => peek.undo(),
        }

        loop {
            let entry = self.expr()?;
            let entry = self.maybe_operator(entry)?;
            entries.push(entry);

            let after = self.progress()?;
            match &after.inner {
                Token::CloseList => return Ok(Entity::List(entries)),
                Token::Comma => continue,
                _ => return Err(self.err_expected("a comma or an end to the list", after)),
            }
        }
    }

    fn matchexpr(&mut self) -> Result<Entity, Error> {
        let to_match = self.expr()?.map(Box::new);

        let mut patterntable = PatternTable::new();
        let mut evals = Vec::new();

        // skip the initial `|`
        let after = self.progress()?;
        match &*after {
            Token::Operator if self.take_span(after.span.clone()) == "|" => {}
            _ => {
                return Err(self.err_expected(
                    format!(
                        "an `{}` followed by the first branch of this match expression",
                        "|".green()
                    ),
                    after,
                ))
            }
        }

        loop {
            let pat = self.pattern(PatternSettings { followups: true })?;
            patterntable.push(pat);

            let after = self.progress()?;
            if *after != Token::Arrow {
                return Err(self.err_expected(
                    format!(
                        "`{}` to seperate the branch from it's expression",
                        "->".green()
                    ),
                    after,
                ));
            }

            let expr = self.expr()?;
            evals.push(expr);

            let after = self.view()?;
            match after.borrow() {
                Token::Operator => {
                    let t = after.consume();
                    // any operator that isn't `|` will be handled by the `self.expr` in it's
                    // `self.maybe_operator`
                    assert_eq!(self.take_span(t.span), "|");
                }
                // TODO: We might want to handle tokens that aren't valid to come after an match
                // expression here to throw errors that are more relevant to the user. Because now
                // we're just letting the lower scope decide whether the continuation is valid or
                // not.
                _ => {
                    after.undo();
                    break Ok(Entity::Match(to_match, patterntable, evals));
                }
            }
        }
        /*
        self.dot_function_calls_enabled = true;

        let expr = self.expr()?;

        let after = self.progress()?;
        if after.inner != Token::With {
            return Err(Error::Unexpected(after.inner).idx(after.idx));
        }

        let mut patterntable = PatternTable::new();
        let mut evals = Vec::new();

        loop {
            // This is to update indentation spaces. There's probably better ways to do it.
            self.view()?.undo();

            let ispaces = self.indentation_spaces;

            {
                let pattern = self.pattern()?;
                patterntable.push(pattern);
            }

            {
                let after = self.progress()?;
                if *after != Token::Arrow {
                    return Err(Error::Unexpected(after.inner).idx(after.idx));
                }
            }

            let eval = self.expr()?;
            evals.push(eval);

            let after_eval = self.view()?;
            match after_eval.borrow() {
                Token::Comma => {
                    self.view()?.undo();
                    let indentation_spaces = self.indentation_spaces;

                    let after_comma = self.view()?;
                    match after_comma.borrow() {
                        a if pattern::patternable(a) && ispaces == indentation_spaces => {
                            after_comma.undo();
                            continue;
                        }
                        _ => {
                            after_comma.undo();
                            break;
                        }
                    }
                }
                other => {
                    let t = other.clone();
                    after_eval.undo();
                    let indentation_spaces = self.indentation_spaces;

                    match pattern::patternable(&t) {
                        // This seems to be a pattern, but there's no comma.
                        true if indentation_spaces == ispaces => {
                            return Err(Error::MatchExpectedComma(t));
                        }
                        // Since there isn't a comma, but this match arm is still complete. We're gonna
                        // assume that this match expression is complete
                        _ => break,
                    }
                }
            }
        }

        Ok(Entity::Match(expr.map(Box::new), patterntable, evals))
        */
    }

    fn ifexpr(&mut self) -> Result<Entity, Error> {
        let mut expr = r#if::Expr::new();
        loop {
            let cond = self.expr()?;

            let after = self.progress()?;
            match &after.inner {
                Token::Then => {}
                _ => return Err(self.err_unexpected(after)),
            }

            let eval = self.expr()?;

            expr.push_condition(cond);
            expr.push_evaluation(eval);

            let after = self.progress()?;
            match &after.inner {
                Token::Else => {
                    let peek = self.view()?;
                    match peek.borrow() {
                        Token::If => continue,
                        _ => {
                            peek.undo();
                            break;
                        }
                    }
                }
                _ => return Err(self.err_unexpected(after)),
            }
        }

        let or_else = self.expr()?;
        expr.push_else(or_else);

        Ok(Entity::If(expr))
    }

    fn letbind(&mut self) -> Result<Entity, Error> {
        let left = self.pattern(PatternSettings { followups: true })?;
        let after = self.progress()?;
        match &after.inner {
            Token::Equals => {}
            _ => return Err(self.err_expected("an `=` to acompany the let binding", after)),
        }
        let right = self.expr()?;

        // We currently create an PatternTable with only one pattern here instead simply to escape
        // some lifetime issues in the verifier. Yes; it's very weird.
        let mut table = ast::PatternTable::new();
        table.push(left);
        let left = table;

        let after = self.progress()?;
        if *after != Token::In {
            return Err(
                self.err_expected(format!("`{}` to seal the let-binding", "in".green()), after)
            );
        }

        let and_then = self.expr()?;

        Ok(Entity::LetBind {
            left,
            right: Box::new(right),
            and_then: vec![and_then],
        })
    }

    fn first_stm(&mut self) -> Result<Entity, Error> {
        let mut stm = first::Stm::new();
        loop {
            let entity = self.expr()?;
            stm.push(entity);

            let after = self.progress()?;
            match &after.inner {
                Token::Next => continue,
                Token::Then => {
                    let entity = self.expr()?;
                    stm.push(entity);
                    return Ok(Entity::First(stm));
                }
                _ => return Err(self.err_unexpected(after)),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Either<A, B> {
    Left(A),
    Right(B),
}

// We sort out some weirdness with lambdas here
// f #(\n -> n) 4
//
// the `4` will be parsed as a parameter to the lambda.
// However, since we're passing this lambda as parameter, we don't really want that.
fn hackily_offload_lambda_params(entity: Tr<Entity>, called_params: &mut Vec<Tr<Entity>>) {
    match entity.inner {
        Entity::Pass(box Entity::Lambda {
            param_names,
            mut params,
            body,
        }) => {
            let entity = Tr::new(Entity::Pass(Box::new(Entity::Lambda {
                param_names,
                params: vec![],
                body,
            })))
            .set(entity.span);
            called_params.push(entity);
            called_params.append(&mut params);
        }
        _ => called_params.push(entity),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Entity as V;

    fn i(v: i64) -> Tr<Entity> {
        Tr::new(V::int(v))
    }
    fn f(v: f64) -> Tr<Entity> {
        Tr::new(V::float(v))
    }

    #[test]
    fn list() {
        let mut module = super::function::tests::test_parser();
        let src = "4, 2.2]";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.list(), src);

        assert_eq!(v, V::list(&[V::int(4), V::float(2.2)]));
    }

    #[test]
    fn fcall() {
        let mut module = super::function::tests::test_parser();
        let src = "std:math:add 4 2.2";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        match v.inner {
            Entity::Identifier { takes, params } => {
                assert_eq!(takes, Location::from_str("std:math:add").unwrap());
                assert_eq!(params, vec![i(4), f(2.2)]);
            }
            _ => unreachable!("{:?}", v),
        }

        let src = "add 4 2.2";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        match v.inner {
            Entity::Identifier { takes, params } => {
                assert_eq!(takes, Location::from_str("add").unwrap());
                assert_eq!(params, vec![i(4), f(2.2)]);
            }
            _ => unreachable!("{:?}", v),
        }
    }

    #[test]
    fn dot_call() {
        let mut module = super::function::tests::test_parser();
        let src = "add x.point unwrap.try_y.point";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        assert_eq!(
            v.inner,
            V::call_name(
                "add",
                &[
                    V::call_dotted("x", V::call_name("point", &[]),),
                    V::call_dotted(
                        "unwrap",
                        V::call_dotted("try_y", V::call_name("point", &[]),),
                    ),
                ],
            ),
        );
    }

    #[test]
    fn operators() {
        let mut module = super::function::tests::test_parser();
        let src = "1 + 2 + 3 + 4";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        let takes = Location::from_str("+").unwrap();
        let o2 = Entity::Identifier {
            takes: takes.clone(),
            params: vec![i(3), i(4)],
        };
        let o1 = Entity::Identifier {
            takes: takes.clone(),
            params: vec![i(2), Tr::new(o2)],
        };
        let o0 = Entity::Identifier {
            takes,
            params: vec![i(1), Tr::new(o1)],
        };

        assert_eq!(v.inner, o0);
    }

    #[test]
    fn first_stm() {
        let mut module = super::function::tests::test_parser();
        let src = "first 0 next 1 then 2";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        assert_eq!(v.inner, V::first(&[V::int(0), V::int(1)], V::int(2)));
    }

    #[test]
    fn if_expr() {
        let mut module = super::function::tests::test_parser();
        let src = "if std:math:add 2 3 + 2 then 0 else if 0 then 1 else 1";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        assert_eq!(
            v.inner,
            V::r#if(
                &[
                    (
                        V::call_name(
                            "+",
                            &[
                                V::call_name("std:math:add", &[V::int(2), V::int(3)]),
                                V::int(2)
                            ]
                        ),
                        V::int(0)
                    ),
                    (V::int(0), V::int(1))
                ],
                V::int(1)
            ),
        );
    }

    #[test]
    fn record_modify() {
        let mut module = super::function::tests::test_parser();
        let src = "{ person . name 0, age 172 }";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        assert_eq!(
            v.inner,
            V::record(
                V::call_name("person", &[]),
                &[("name", V::int(0)), ("age", V::int(172))]
            )
        );
    }

    #[test]
    fn letbind() {
        let mut module = super::function::tests::test_parser();
        let src = "let x = 2 + 2 in\n  let y = 2 + 2 in\n    0";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        let operation = V::call_name("+", &[V::int(2), V::int(2)]);

        assert_eq!(
            v.inner,
            V::let_bind(
                Pattern::Wildcard(Tr::new("x".into())),
                operation.clone(),
                V::let_bind(Pattern::Wildcard(Tr::new("y".into())), operation, V::int(0))
            )
        );
    }

    #[test]
    fn tuple() {
        let mut module = super::function::tests::test_parser();
        let src = "{4, (2 + 2)}";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        assert_eq!(
            v.inner,
            V::tuple(&[V::int(4), V::call_name("+", &[V::int(2), V::int(2)])]),
        );
    }

    #[test]
    fn casting() {
        let mut module = super::function::tests::test_parser();
        let src = "4.0 as int + v as *option int";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        let left = Entity::Cast(f(4.0).map(Box::new), Tr::new(Type::Int));
        let right = Entity::Cast(
            Tr::new(Box::new(Entity::Identifier {
                takes: Location::from_str("v").unwrap(),
                params: vec![],
            })),
            Tr::new(Type::Pointer(Box::new(Tr::new(Type::Defined(
                Location::from_str("option").unwrap(),
                vec![Tr::new(Type::Int)],
            ))))),
        );

        assert_eq!(
            v.inner,
            Entity::Identifier {
                takes: Location::from_str("+").unwrap(),
                params: vec![Tr::new(left), Tr::new(right)]
            }
        );
    }

    // We currently have a little dirty hack to allow `*` as an operator since `**` needs to be
    // parsed as two tokens in the context of types but one token in the context of user defined
    // operators.
    //
    // This, in combination with the tokenizer and type.rs tests, verifies that hack is working.
    #[test]
    fn asterisk_as_operator() {
        let mut module = super::function::tests::test_parser();
        let src = "1 ** 2 *% 3 %* 4";

        let v = Error::test_failure(Builder::from_str(&mut module, src).expr(), src);

        let o2 = V::call_name("%*", &[V::int(3), V::int(4)]);
        let o1 = V::call_name("*%", &[V::int(2), o2]);
        let o0 = V::call_name("**", &[V::int(1), o1]);

        assert_eq!(v.inner, o0);
    }

    #[test]
    fn match_expression() {
        let mut module = super::function::tests::test_parser();
        let src = "match 0 \n  | {5, 2} -> 0\n  | other -> 1";

        let v = Error::test_failure(Builder::from_str(&mut module, src).expr(), src);

        let b0 = ast::Pattern::Tuple(vec![
            ast::Pattern::Value(Inlinable::Int(5)),
            ast::Pattern::Value(Inlinable::Int(2)),
        ]);
        let b1 = ast::Pattern::Wildcard(Tr::new("other".into()));
        let branchexprs = vec![i(0), i(1)];

        assert_eq!(
            v.inner,
            ast::Entity::Match(
                i(0).map(Box::new),
                PatternTable::from_patterns(vec![b0, b1]),
                branchexprs
            )
        );
    }

    #[test]
    fn complex() {
        let mut module = super::function::tests::test_parser();
        let src =
            "(first 0 then [add 4 2 + 1+2]) <> [add (4) 2] <> if 1 then [add 1 << 1 + 2] else match add 4 2 | 0 -> 0 | 1 -> 1";

        let mut builder = Builder::from_str(&mut module, src);
        let v = Error::test_failure(builder.expr(), src);

        let add = V::call_name("add", &[V::int(4), V::int(2)]);

        let left = V::first(
            &[V::int(0)],
            Entity::list(&[V::call_name(
                "+",
                &[add.clone(), V::call_name("+", &[V::int(1), V::int(2)])],
            )]),
        );

        let middle = V::list(&[add.clone()]);

        let right = V::list(&[V::call_name(
            "add",
            &[V::int(1), V::call_name("+", &[V::int(1), V::int(2)])],
        )]);

        let match_expression = V::r#match(
            add,
            &[
                Pattern::Value(Inlinable::Int(0)),
                Pattern::Value(Inlinable::Int(1)),
            ],
            &[V::int(0), V::int(1)],
        );

        let right = V::r#if(&[(V::int(1), right)], match_expression);

        let expected = V::call_name("<>", &[left, V::call_name("<>", &[middle, right])]);

        if v.inner != expected {
            println!("not equal:\nWANTED:\n{}\nGOT:\n{}", &expected, &v.inner);
            panic!();
        }
    }
}
