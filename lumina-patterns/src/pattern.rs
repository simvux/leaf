use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<IDENT, EXPR, TYPE, V> {
    Wildcard(IDENT),
    Value(V),
    Range(Box<[Self; 2]>),

    If(Box<Self>, Box<EXPR>),
    Record(TYPE, Vec<(IDENT, Self)>),
    Tuple(Vec<Self>),
    Enum(IDENT, Vec<Self>),

    List(Vec<Self>),
    ListRemaining(Vec<Self>, IDENT),

    Or([Box<Self>; 2]),
}

impl<I, E, T, V> Pattern<I, E, T, V> {
    pub fn visit_wildcards<O, F: FnMut(&I) -> O>(&self, visit: &mut F) -> Vec<O> {
        match self {
            Self::Wildcard(ident) => {
                vec![visit(ident)]
            }
            Self::Range(entries) => entries
                .iter()
                .flat_map(|v| v.visit_wildcards(visit)) // TODO: Do these collect in the right order?
                .collect(),
            Self::If(pat, _cond) => pat.visit_wildcards(visit),
            Self::List(entries) => entries
                .iter()
                .flat_map(|v| v.visit_wildcards(visit))
                .collect(),
            Self::ListRemaining(x, _xs) => {
                x.iter().flat_map(|v| v.visit_wildcards(visit)).collect()
            }
            Self::Record(_t, entries) => entries
                .iter()
                .flat_map(|(_, v)| v.visit_wildcards(visit))
                .collect(),
            Self::Tuple(entries) => entries
                .iter()
                .flat_map(|v| v.visit_wildcards(visit))
                .collect(),
            Self::Enum(_t, tp) => tp.iter().flat_map(|v| v.visit_wildcards(visit)).collect(),

            Self::Or(entries) => entries
                .iter()
                .flat_map(|v| v.visit_wildcards(visit))
                .collect(),

            _ => vec![],
        }
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct PatternTable<IDENT, EXPR, TYPE, V> {
    pub patterns: Vec<Pattern<IDENT, EXPR, TYPE, V>>,
}

impl<I, E, T, V> PatternTable<I, E, T, V> {
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
        }
    }

    pub fn push(&mut self, p: Pattern<I, E, T, V>) {
        self.patterns.push(p);
    }

    pub fn from_patterns(patterns: Vec<Pattern<I, E, T, V>>) -> Self {
        Self { patterns }
    }
}

impl<I: fmt::Display, E: fmt::Display, T: fmt::Display, V: fmt::Display> fmt::Display
    for Pattern<I, E, T, V>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Wildcard(ident) => ident.fmt(f),
            Self::Value(v) => v.fmt(f),
            Self::Range(entries) => {
                entries[0].fmt(f)?;
                f.write_str("..")?;
                entries[1].fmt(f)?;
                Ok(())
            }
            Self::If(pat, cond) => write!(f, "{} if {}", pat, cond),
            Self::List(entries) => write!(f, "[{}]", entries.iter().format(", ")),
            Self::ListRemaining(x, xs) => write!(f, "[{} :: {}]", x.iter().format(", "), xs),
            Self::Record(t, entries) => write!(
                f,
                " {{ {} . {} }}",
                t,
                entries
                    .iter()
                    .map(|(field, pat)| format!("{} {}", field, pat))
                    .format(", ")
            ),
            Self::Tuple(entries) => write!(f, "{{ {} }}", entries.iter().format(", ")),
            Self::Enum(t, tp) => write!(f, "{} {}", t, tp.iter().format(" ")),

            Self::Or([left, right]) => write!(f, "{} or {}", left, right),
        }
    }
}
