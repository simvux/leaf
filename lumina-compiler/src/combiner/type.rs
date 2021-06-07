use crate::Typing as BaseTyping;
pub use lumina_typesystem::{ITp, Tp};
use lumina_util::Tr;

pub type TemplateType = lumina_typesystem::Type;
pub type DirectType = lumina_typesystem::Direct;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Types {
    pub template: Tp<TemplateType>,
    pub dispatched: StaticType,
}

pub type Typing = BaseTyping<StaticType>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StaticType {
    pub ownership: Ownership,
    pub template: Tp<DirectType>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ownership {
    Copied,
    Borrowed { clone: usize, free: usize },
    Owned { clone: usize, free: usize },
}

impl StaticType {
    fn new(template: Tp<DirectType>, ownership: Ownership) -> StaticType {
        StaticType {
            template,
            ownership,
        }
    }

    pub fn copied(template: Tp<DirectType>) -> StaticType {
        StaticType::new(template, Ownership::Copied)
    }
}

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

pub trait FuncKey {
    fn to_key(&self) -> (usize, &[Tr<StaticType>]);
}

impl Hash for dyn FuncKey + '_ {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_key().hash(state)
    }
}
impl PartialEq for dyn FuncKey + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.to_key() == other.to_key()
    }
}
impl Eq for dyn FuncKey + '_ {}
impl FuncKey for (usize, Vec<Tr<StaticType>>) {
    fn to_key(&self) -> (usize, &[Tr<StaticType>]) {
        (self.0, &self.1)
    }
}
impl<'a> Borrow<dyn FuncKey + 'a> for (usize, Vec<Tr<StaticType>>) {
    fn borrow(&self) -> &(dyn FuncKey + 'a) {
        self
    }
}
impl FuncKey for (usize, &[Tr<StaticType>]) {
    fn to_key(&self) -> (usize, &[Tr<StaticType>]) {
        (self.0, &self.1)
    }
}
impl<'a> Borrow<dyn FuncKey + 'a> for (usize, &'a [Tr<StaticType>]) {
    fn borrow(&self) -> &(dyn FuncKey + 'a) {
        self
    }
}

pub trait MethodKey {
    fn to_key(&self) -> (usize, usize, &[Tr<StaticType>]);
}

impl Hash for dyn MethodKey + '_ {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_key().hash(state)
    }
}
impl PartialEq for dyn MethodKey + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.to_key() == other.to_key()
    }
}
impl Eq for dyn MethodKey + '_ {}
impl MethodKey for (usize, usize, Vec<Tr<StaticType>>) {
    fn to_key(&self) -> (usize, usize, &[Tr<StaticType>]) {
        (self.0, self.1, &self.2)
    }
}
impl<'a> Borrow<dyn MethodKey + 'a> for (usize, usize, Vec<Tr<StaticType>>) {
    fn borrow(&self) -> &(dyn MethodKey + 'a) {
        self
    }
}
impl MethodKey for (usize, usize, &[Tr<StaticType>]) {
    fn to_key(&self) -> (usize, usize, &[Tr<StaticType>]) {
        (self.0, self.1, &self.2)
    }
}
impl<'a> Borrow<dyn MethodKey + 'a> for (usize, usize, &'a [Tr<StaticType>]) {
    fn borrow(&self) -> &(dyn MethodKey + 'a) {
        self
    }
}
