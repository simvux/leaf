use super::Tr;
use colored::Colorize;
use itertools::Itertools;
use std::fmt;

/// Typing is a simple generic struct that pairs parameters with a return type while
/// holding the types position in source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Typing<T> {
    pub ptypes: Vec<Tr<T>>,
    pub returns: Tr<T>,
}

impl<T> Typing<T> {
    pub fn new(ptypes: Vec<Tr<T>>, returns: Tr<T>) -> Self {
        Self { ptypes, returns }
    }
}

impl<T: fmt::Display> fmt::Display for Typing<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.ptypes.is_empty() {
            return write!(f, "{}{}{}", "(".purple(), &self.returns, ")".purple());
        }

        write!(
            f,
            "{}{} -> {}{}",
            "(".purple(),
            self.ptypes.iter().format(" "),
            &self.returns,
            ")".purple(),
        )
    }
}
