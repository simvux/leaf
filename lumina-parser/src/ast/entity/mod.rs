use colored::*;
use itertools::Itertools;
use lumina_util::{Builtin, Location, Tr};
use std::fmt;
use std::rc::Rc;

pub mod first;
pub mod r#if;
pub mod r#patterns;

/// An AST Entity.
/// Which means, an AST expression (like an if-expression) or an AST statement (like an first-statement)
#[derive(Clone, PartialEq, Debug)]
pub enum Entity {
    Identifier {
        takes: Location,
        params: Vec<Tr<Entity>>,
    },
    Lambda {
        param_names: Vec<Tr<String>>,
        params: Vec<Tr<Entity>>,
        body: Box<Tr<Entity>>,
    },
    ExpressionCall {
        takes: Tr<Box<Entity>>,
        params: Vec<Tr<Entity>>,
    },
    DotCall {
        left: String,
        right: Box<Tr<Entity>>,
    },
    Builtin(Builtin<Rc<Location>, Entity>),

    Cast(Tr<Box<Self>>, Tr<super::Type>),

    Inlined(Inlinable),
    Record(Tr<Box<Entity>>, Vec<(String, Tr<Entity>)>),
    Tuple(Vec<Tr<Entity>>),
    List(Vec<Tr<Entity>>),

    Pass(Box<Entity>),

    Match(Tr<Box<Entity>>, patterns::PatternTable, Vec<Tr<Entity>>),
    If(r#if::Expr<Tr<Entity>>),
    First(first::Stm),
    LetBind {
        left: r#patterns::PatternTable,
        right: Box<Tr<Entity>>,
        // This is an `Vec` rather than `Box` as a hack to pass the `f` lifetime when using this as
        // `bodies` list for the match expression it lowers to.
        and_then: Vec<Tr<Entity>>,
    },
    Unimplemented,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Inlinable {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Nothing,
}

impl fmt::Display for Inlinable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Char(c) => write!(f, "{}", c),
            Self::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Self::Nothing => write!(f, "nothing"),
        }
    }
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Entity::*;

        match self {
            Identifier { takes, params } if params.is_empty() => takes.fmt(f),
            Identifier { takes, params } => {
                write!(f, "({}", takes)?;
                for p in params.iter() {
                    f.write_str(" ")?;
                    p.inner.fmt(f)?;
                }
                write!(f, ")")
            }
            ExpressionCall { takes, params } => {
                f.write_str("(")?;
                takes.fmt(f)?;
                f.write_str(")")?;
                for p in params.iter() {
                    f.write_str(" ")?;
                    p.inner.fmt(f)?;
                }
                Ok(())
            }
            Cast(entity, type_) => write!(f, "{} as {}", entity, type_),
            Builtin(b) => b.fmt(f),
            Tuple(entries) => write!(f, "{{ {} }}", entries.iter().format(", ")),
            DotCall { left, right } => write!(f, "{}.{}", left, right),
            Record(apply_on, fields) => {
                write!(f, "{} ", "{".purple())?;
                apply_on.inner.fmt(f)?;
                write!(f, " {} ", ".".purple())?;

                if let Some((name, v)) = fields.get(0) {
                    write!(f, "{} ", name)?;
                    v.inner.fmt(f)?;
                }

                for (name, v) in fields.iter().skip(1) {
                    write!(f, ", {} ", name)?;
                    v.inner.fmt(f)?;
                }
                write!(f, " {}", "}".purple())
            }
            Lambda {
                param_names,
                params,
                body,
            } => {
                write!(
                    f,
                    "({}{} -> ",
                    "\\".purple(),
                    param_names.iter().format(" ")
                )?;
                body.inner.fmt(f)?;
                f.write_str(")")?;
                for p in params.iter() {
                    f.write_str(" ")?;
                    p.inner.fmt(f)?;
                }
                Ok(())
            }
            Inlined(v) => v.fmt(f),
            List(inner) => {
                write!(f, "{}", "[".purple())?;
                if let Some(v) = inner.get(0) {
                    v.inner.fmt(f)?;
                }
                for v in inner.iter().skip(1) {
                    f.write_str(", ")?;
                    v.inner.fmt(f)?;
                }
                write!(f, "{}", "]".purple())?;
                Ok(())
            }
            Pass(inner) => {
                write!(f, "{}", "#".purple())?;
                inner.fmt(f)
            }
            LetBind {
                left,
                right,
                and_then,
            } => {
                write!(f, "{} ", "let".green())?;
                left.patterns[0].fmt(f)?;
                write!(f, " {} ", "=".green())?;
                right.inner.fmt(f)?;
                write!(f, " {} ", "in".green())?;
                and_then[0].inner.fmt(f)
            }
            Match(expr, patt, evals) => write!(
                f,
                "match {}\n  {}",
                expr,
                patt.patterns
                    .iter()
                    .zip(evals.iter())
                    .map(|(pat, eval)| format!("{} -> {},", pat, eval))
                    .format("\n  "),
            ),
            If(expr) => expr.fmt(f),
            First(stm) => stm.fmt(f),
            Unimplemented => write!(f, "{}", "todo".green()),
        }
    }
}
