// A simple wrapper type that causes the interior value to be ignored by `PartialEq` and `Hash`
// derives. Useful for keeping metadata in hashmap keys.
#[derive(Debug, Clone, Default, Copy)]
pub struct Ign<T> {
    pub inner: T,
}

impl<T> Ign<T> {
    pub const fn new(inner: T) -> Self {
        Self { inner }
    }
}

impl<T> PartialEq for Ign<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}
impl<T> Eq for Ign<T> {}
impl<T> std::hash::Hash for Ign<T> {
    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
}

impl<T> std::ops::Deref for Ign<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.inner
    }
}
