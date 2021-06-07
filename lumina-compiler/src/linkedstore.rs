use lumina_util::Location;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

pub type LocationLinkedStore<T> = LinkedStore<Rc<Location>, T>;

/// LinkedStore allows accessing a value of type V using either a key or an index.
///
/// Both forms of lookup are O(1)
#[derive(Debug, Clone)]
pub struct LinkedStore<K, V> {
    links: HashMap<K, usize>,
    bodies: Vec<V>,
}

impl<K, V> Default for LinkedStore<K, V> {
    fn default() -> Self {
        Self::with_capacity(10)
    }
}

impl<K, V> LinkedStore<K, V> {
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            links: HashMap::with_capacity(cap),
            bodies: Vec::with_capacity(cap),
        }
    }

    pub fn len(&self) -> usize {
        self.bodies.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        self.bodies.iter()
    }
}

impl<K: Eq + Hash, V> LinkedStore<K, V> {
    /// Insert this K/V pair.
    ///
    /// Returns linked V if K is already occupied
    pub fn insert(&mut self, k: K, mut v: V) -> Option<V> {
        let id = self.next_id();

        match self.links.insert(k, id) {
            Some(existing) => {
                std::mem::swap(&mut self.bodies[existing], &mut v);
                Some(v)
            }
            None => {
                self.bodies.push(v);
                None
            }
        }
    }

    /// Returns the ID that's out of bound by 1
    pub fn next_id(&self) -> usize {
        self.bodies.len()
    }

    pub fn get_from_key<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let idx = self.links.get(k.borrow()).copied()?;
        self.get_from_idx(idx)
    }

    pub fn get_from_idx(&self, idx: usize) -> Option<&V> {
        self.bodies.get(idx)
    }

    pub fn get_mut_from_key<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let idx = self.links.get(k).copied()?;
        self.get_mut_from_idx(idx)
    }
    pub fn get_mut_from_idx(&mut self, idx: usize) -> Option<&mut V> {
        self.bodies.get_mut(idx)
    }

    pub fn resolve<Q: ?Sized>(&self, k: &Q) -> Option<usize>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.links.get(k.borrow()).copied()
    }

    pub fn as_index(&self) -> &HashMap<K, usize> {
        &self.links
    }

    pub fn as_slice(&self) -> &[V] {
        &self.bodies
    }
}
