use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Primitive {
    Int,
    Float,
    Bool,
    Nothing,
}
use Primitive::*;

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Int => "int",
            Float => "float",
            Bool => "bool",
            Nothing => "nothing",
        }.fmt(f)
    }
}
