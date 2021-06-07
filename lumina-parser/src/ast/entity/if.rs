use colored::*;
use std::fmt;

/// If expressions, to prevent heap allocations, is just one singular Vec.
///
/// if   6 == 6 then 0
/// elif 7 == 7 then 1
/// elif 8 == 8 then 2
/// else 9
///
/// 0: 6 == 6
/// 1: 0
/// 2: 7 == 7
/// 3: 1
/// 4: 8 == 8
/// 5: 2
/// 6: 9
///
/// Causes panic if branches are pushed after `push_else`.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr<T> {
    inner: Vec<T>,
}

impl<T> From<(Vec<T>, T)> for Expr<T> {
    fn from((mut tree, or_else): (Vec<T>, T)) -> Expr<T> {
        let mut expr = Expr::new();
        expr.inner.append(&mut tree);
        expr.push_else(or_else);
        expr
    }
}

impl<T> Expr<T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(3),
        }
    }
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn push_raw(&mut self, v: T) {
        self.inner.push(v)
    }

    pub fn push_condition(&mut self, v: T) {
        if self.inner.len() & 1 == 0 {
            self.inner.push(v);
        } else {
            panic!("Corrupt if expression",);
        }
    }
    pub fn push_evaluation(&mut self, v: T) {
        if self.inner.len() & 1 == 1 {
            self.inner.push(v);
        } else {
            panic!("Corrupt if expression",);
        }
    }
    pub fn push_else(&mut self, v: T) {
        self.inner.push(v);
    }

    pub fn iter_conditions(&self) -> impl Iterator<Item = &T> {
        self.inner
            .iter()
            .enumerate()
            .filter(move |(i, _v)| (*i == 0 || i & 1 == 0) && (i + 1 != self.inner.len()))
            .map(|(_i, v)| v)
    }
    pub fn iter_evaluations(&self) -> impl Iterator<Item = &T> {
        self.inner
            .iter()
            .enumerate()
            .filter(move |(i, _v)| (i & 1 == 1) && (i + 1 != self.inner.len()))
            .map(|(_i, v)| v)
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.inner.iter_mut()
    }
    pub fn branches(&mut self) -> impl Iterator<Item = (&T, &T)> {
        self.iter_conditions().zip(self.iter_evaluations())
    }
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.inner.into_iter()
    }

    pub fn r#else(&self) -> &T {
        self.inner.last().unwrap()
    }

    pub fn map<F>(self, f: F) -> Self
    where
        F: Fn(T) -> T,
    {
        Self {
            inner: self.inner.into_iter().map(f).collect(),
        }
    }

    pub fn from_raw(inner: Vec<T>) -> Self {
        Self { inner }
    }
}

impl<T> std::iter::FromIterator<T> for Expr<T> {
    fn from_iter<I: std::iter::IntoIterator<Item = T>>(iter: I) -> Self {
        let inner = iter.into_iter().collect::<Vec<_>>();
        Self { inner }
    }
}

impl<T: fmt::Display> fmt::Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for (c, e) in self.iter_conditions().zip(self.iter_evaluations()) {
            write!(f, "{} ", if first { "if" } else { "elif" }.green())?;
            c.fmt(f)?;
            write!(f, " {}", "then".green())?;
            e.fmt(f)?;
            first = false;
        }
        write!(f, "{}", "else ".green())?;
        self.r#else().fmt(f)
    }
}
