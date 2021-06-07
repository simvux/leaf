use super::Entity;
use colored::*;
use lumina_util::Tr;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Stm {
    inner: Vec<Tr<Entity>>,
}

impl From<Vec<Tr<Entity>>> for Stm {
    fn from(v: Vec<Tr<Entity>>) -> Stm {
        Stm { inner: v }
    }
}

impl Stm {
    pub fn new() -> Stm {
        Stm {
            inner: Vec::with_capacity(2),
        }
    }
    pub fn push(&mut self, v: Tr<Entity>) {
        self.inner.push(v)
    }
    pub fn iter_void(&self) -> impl Iterator<Item = &Tr<Entity>> {
        self.inner[0..self.inner.len() - 1].iter()
    }
    pub fn used(&self) -> &Tr<Entity> {
        self.inner.last().unwrap()
    }
    pub fn into_inner(self) -> Vec<Tr<Entity>> {
        self.inner
    }
    pub fn inner(&self) -> &[Tr<Entity>] {
        &self.inner
    }
}

impl fmt::Display for Stm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut is_first = true;
        for v in self.iter_void() {
            if is_first {
                write!(f, "{} ", "first".green())?;
                v.inner.fmt(f)?;
            } else {
                write!(f, " {} ", "and".green())?;
                v.inner.fmt(f)?;
            }
            is_first = false;
        }
        write!(f, " {}", "then ".green())?;
        self.used().inner.fmt(f)
    }
}
