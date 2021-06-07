//! Construct AST directly through rust

use super::*;
use ast::Entity;

impl Entity {
    pub fn call_location(takes: Location, params: &[Entity]) -> Entity {
        Entity::Identifier {
            takes,
            params: tr(params),
        }
    }

    pub fn call_name<S: AsRef<str>>(takes: S, params: &[Entity]) -> Entity {
        Entity::call_location(Location::from_str(takes.as_ref()).unwrap(), params)
    }

    pub fn call_lambda<N: Into<String> + Clone>(
        pnames: &[N],
        params: &[Entity],
        body: Entity,
    ) -> Entity {
        Entity::Lambda {
            param_names: pnames
                .iter()
                .cloned()
                .map(Into::into)
                .map(Tr::new)
                .collect(),
            params: tr(params),
            body: Box::new(Tr::new(body)),
        }
    }

    pub fn call_expr(expr: Entity, params: &[Entity]) -> Entity {
        Entity::ExpressionCall {
            takes: Tr::new(Box::new(expr)),
            params: params.iter().cloned().map(Tr::new).collect(),
        }
    }

    pub fn call_dotted<S: Into<String>>(left: S, right: Entity) -> Entity {
        Entity::DotCall {
            left: left.into(),
            right: Box::new(Tr::new(right)),
        }
    }

    pub fn cast(expr: Entity, as_: ast::Type) -> Entity {
        Entity::Cast(Tr::new(Box::new(expr)), Tr::new(as_))
    }

    pub fn int(v: i64) -> Entity {
        Entity::Inlined(Inlinable::Int(v))
    }

    pub fn float(v: f64) -> Entity {
        Entity::Inlined(Inlinable::Float(v))
    }

    pub fn string<S: Into<String>>(s: S) -> Entity {
        Entity::Inlined(Inlinable::String(s.into()))
    }

    pub fn record<S: Into<String> + Clone>(root: Entity, entries: &[(S, Entity)]) -> Entity {
        Entity::Record(
            Tr::new(Box::new(root)),
            entries
                .iter()
                .cloned()
                .map(|(s, v)| (s.into(), Tr::new(v)))
                .collect(),
        )
    }

    pub fn tuple(entries: &[Entity]) -> Entity {
        Entity::Tuple(tr(entries))
    }

    pub fn list(entries: &[Entity]) -> Entity {
        Entity::List(tr(entries))
    }

    pub fn pass(inner: Entity) -> Entity {
        Entity::Pass(Box::new(inner))
    }

    pub fn r#match(expr: Entity, patterns: &[ast::Pattern], evals: &[Entity]) -> Entity {
        Entity::Match(
            Tr::new(Box::new(expr)),
            PatternTable::from_patterns(patterns.to_vec()),
            evals.iter().cloned().map(Tr::new).collect(),
        )
    }

    pub fn r#if(branches: &[(Entity, Entity)], else_: Entity) -> Entity {
        let mut all = Vec::new();

        for (cond, eval) in branches {
            all.push(Tr::new(cond.clone()));
            all.push(Tr::new(eval.clone()));
        }

        all.push(Tr::new(else_));

        Entity::If(ast::r#if::Expr::from_raw(all))
    }

    pub fn first(void: &[Entity], then: Entity) -> Entity {
        let mut stm = ast::first::Stm::new();

        for v in void.iter().cloned().map(Tr::new) {
            stm.push(v);
        }

        stm.push(Tr::new(then));

        Entity::First(stm)
    }

    pub fn let_bind(pattern: Pattern, right: Entity, and_then: Entity) -> Entity {
        Entity::LetBind {
            left: PatternTable::from_patterns(vec![pattern]),
            right: Box::new(Tr::new(right)),
            and_then: vec![Tr::new(and_then)],
        }
    }

    pub fn unimplemented() -> Entity {
        Entity::Unimplemented
    }
}

fn tr<T: Clone>(ts: &[T]) -> Vec<Tr<T>> {
    ts.iter().cloned().map(Tr::new).collect()
}
