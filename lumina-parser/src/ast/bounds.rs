use super::*;

// NOTE: This file will be moved to leaf-util at some point.

/// All bounds for all generics
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Bounds<T> {
    inner: HashMap<u8, Vec<GenBound<T>>>,
}

/// A bound for a generic
pub type GenBound<T> = (Location, Vec<Tr<T>>);

impl<T> Bounds<T> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn bind(&mut self, gen: u8, trait_: Location, tp: Vec<Tr<T>>) {
        self.inner
            .entry(gen)
            .or_insert_with(Vec::new)
            .push((trait_, tp))
    }

    /*
    pub fn map<B>(
        self,
        mut f: impl FnMut((Location, Vec<Tr<T>>)) -> (Location, Vec<Tr<B>>),
    ) -> Bounds<B> {
        Bounds {
            inner: self.inner.into_iter().map(|(g, v)| (g, f(v))).collect(),
        }
    }
    */

    pub fn get(&self, gen: &u8) -> Option<&[GenBound<T>]> {
        self.inner.get(gen).map(|a| a.as_slice())
    }
}
