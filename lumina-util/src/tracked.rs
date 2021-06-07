use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

// Simple wrapper type, used for including the source-code index
#[derive(Clone, Default)]
pub struct Tr<T> {
    pub span: std::ops::Range<usize>,
    pub inner: T,
}

impl<T> Deref for Tr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T> DerefMut for Tr<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T: PartialEq> PartialEq for Tr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
impl<T: Eq> Eq for Tr<T> {}
impl<T: Hash> Hash for Tr<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.inner.hash(hasher)
    }
}

impl<T: fmt::Debug> fmt::Debug for Tr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}..{}.{:#?}",
            self.span.start, self.span.end, &self.inner
        )
    }
}
impl<T: fmt::Display> fmt::Display for Tr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T> Tr<T> {
    pub fn new(inner: T) -> Self {
        Self { inner, span: 0..0 }
    }
    pub fn tr(span: std::ops::Range<usize>, inner: T) -> Self {
        Self { span, inner }
    }
    pub fn set(mut self, span: std::ops::Range<usize>) -> Self {
        self.span = span;
        self
    }
    pub fn sep(self) -> (T, std::ops::Range<usize>) {
        (self.inner, self.span)
    }
    pub fn untrack(self) -> T {
        self.inner
    }
    pub fn span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }
    pub fn as_ref(&self) -> Tr<&T> {
        let span = self.span.clone();
        Tr {
            span,
            inner: &self.inner,
        }
    }
    pub fn as_mut(&mut self) -> Tr<&mut T> {
        let span = self.span.clone();
        Tr {
            span,
            inner: &mut self.inner,
        }
    }
    pub fn swap<A: Into<B>, B>(self, other: A) -> Tr<B> {
        let span = self.span;
        Tr {
            span,
            inner: other.into(),
        }
    }
    pub fn map<B>(self, f: impl FnOnce(T) -> B) -> Tr<B> {
        let i = self.span;
        let v = f(self.inner);
        Tr::new(v).set(i)
    }
    pub fn try_map<B, E>(self, f: impl FnOnce(T) -> Result<B, E>) -> Result<Tr<B>, E> {
        let i = self.span;
        let v = f(self.inner)?;
        Ok(Tr::new(v).set(i))
    }
    pub fn set_span(mut self, span: std::ops::Range<usize>) -> Self {
        self.span = span;
        self
    }
}

impl<T: Clone> Tr<&mut T> {
    pub fn cloned_mut(&self) -> Tr<T> {
        let span = self.span.clone();
        Tr::tr(span, self.inner.clone())
    }
}
impl<T: Clone> Tr<&T> {
    pub fn cloned(&self) -> Tr<T> {
        let span = self.span.clone();
        Tr::tr(span, self.inner.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tracked() {
        let (n, i) = Tr::new(1).set(0..1).map(|old: i32| old + 1).sep();
        assert_eq!((n, i), (2, 0..1));
    }
}
