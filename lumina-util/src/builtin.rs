use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Builtin<I, E> {
    Accessor(usize, Box<E>),
    Constructor(usize, I, Vec<E>),
}

impl<IDENT: fmt::Display, E: fmt::Display> fmt::Display for Builtin<IDENT, E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Accessor(id, of) => write!(f, "ACCESS {} {}", id, of),
            Builtin::Constructor(id, of, params) => write!(
                f,
                "CONSTRUCT {} {} [{}]",
                id,
                of,
                params.iter().format(", ")
            ),
        }
    }
}
