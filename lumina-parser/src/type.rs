use super::Attr;
use crate::{ast, Error};
use colored::*;
use itertools::Itertools;
use lumina_util::{Ign, Location, Tr};
use std::collections::HashMap;
use std::fmt;

pub type TypeParameters = Vec<Tr<Type>>;
pub type TypeParams = [Tr<Type>];

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Type {
    Nothing,
    Int,
    Float,
    Infer,
    Bool,
    TraitSelf(TypeParameters),
    Generic(u8, TypeParameters),

    List(Box<Tr<Type>>),
    Tuple(Vec<Tr<Type>>),
    Closure(Vec<Tr<Type>>, Box<Tr<Type>>),
    Function(Vec<Tr<Type>>, Box<Tr<Type>>),
    Defined(Location, TypeParameters),
    Pointer(Box<Tr<Type>>),
    Infallible,

    /// Sometimes we need to index into the given type parameters
    ///
    /// ```lumina
    /// enum result a e
    ///   ok  a
    ///   err e
    /// ```
    ///
    /// `a` is converted into `TypeParameter(0)`
    /// `e` is converted into `TypeParameter(1)`
    ///
    /// We then use this information to index into the given type-annotation to construct the *real*
    /// degenericified type.
    TypeParameter(usize, u8, TypeParameters),
}

impl Type {
    /// Tries to parse a generic. Which in our syntax is any lowercase one-letter identifier
    pub fn try_generic<S: AsRef<str>>(s: S) -> Option<u8> {
        let s = s.as_ref();
        if s.len() == 1 {
            if let c @ 'a'..='z' = s.chars().next().unwrap() {
                return Some(c as u8 - b'a');
            }
        }
        None
    }

    /// Tries to parse a simple primitive/builtin type that takes no type parameters or anything
    /// fancy like that
    pub fn simple<S: AsRef<str>>(s: S) -> Option<Type> {
        let s = s.as_ref();
        match s {
            "nothing" | "_" => Some(Type::Nothing),
            "int" => Some(Type::Int),
            "float" => Some(Type::Float),
            "bool" => Some(Type::Bool),
            "*" => Some(Type::Infer),
            _ => None,
        }
    }

    pub fn unwrap_defined(self) -> (Location, TypeParameters) {
        match self {
            Type::Defined(v, params) => (v, params),
            other => panic!("called `unwrap_defined` in an non-defined value: {}", other),
        }
    }

    /// Convert a declared types generics to the index those generics are declared in under the
    /// type decleration
    ///
    /// ```lumina
    /// enum result a e
    ///   err e
    ///   ok  a
    /// ```
    /// ... becomes
    /// ```lumina
    /// enum result
    ///   err p1
    ///   ok  p0
    /// ```
    pub fn resolve_type_parameter(self, type_parameters: &[u8]) -> Result<Type, Error> {
        match self {
            Type::Generic(genid, params) => {
                let params = params
                    .into_iter()
                    .map(|t| {
                        let idx = t.span;
                        match t.inner.resolve_type_parameter(type_parameters) {
                            Ok(t) => Ok(Tr::tr(idx, t)),
                            Err(e) => Err(e.idx(idx)),
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                match type_parameters.iter().position(|&i| i == genid) {
                    Some(type_param_idx) => Ok(Type::TypeParameter(type_param_idx, genid, params)),
                    None => Err(Error::GenericNotDeclared(genid)),
                }
            }
            other => Ok(other),
        }
    }

    pub fn map_bottom_types_except_traitself<F>(self, mut f: F) -> Type
    where
        F: FnMut(Self) -> Self,
    {
        let f = &mut f;

        match self {
            Type::Generic(genid, tp) => {
                Type::Generic(genid, tp.into_iter().map(|t| t.map(|t| f(t))).collect())
            }
            Type::List(inner) => Type::List(Box::new(inner.map(f))),
            Type::Tuple(inner) => Type::Tuple(inner.into_iter().map(|t| t.map(|t| f(t))).collect()),
            Type::Closure(takes, gives) => Type::Closure(
                takes.into_iter().map(|t| t.map(|t| f(t))).collect(),
                Box::new(gives.map(f)),
            ),
            Type::Defined(loc, tp) => {
                Type::Defined(loc, tp.into_iter().map(|t| t.map(|t| f(t))).collect())
            }
            Type::TypeParameter(idx, genid, tp) => Type::TypeParameter(
                idx,
                genid,
                tp.into_iter().map(|t| t.map(|t| f(t))).collect(),
            ),
            _ => f(self),
        }
    }

    pub fn type_params(&self) -> &[Tr<Type>] {
        match self {
            Type::Generic(_, tp) => &tp,
            Type::Defined(_, tp) => &tp,
            Type::TypeParameter(_, _, tp) => &tp,
            _ => &[],
        }
    }
}

impl Default for Type {
    fn default() -> Type {
        Type::Infer
    }
}

use super::ast::Inlinable;
impl From<&Inlinable> for Type {
    fn from(v: &Inlinable) -> Type {
        match v {
            Inlinable::Float(_) => Type::Float,
            Inlinable::Int(_) => Type::Int,
            Inlinable::Char(_) => todo!(),
            Inlinable::String(_) => panic!("cannot convert string literal to type"),
            Inlinable::Bool(_) => Type::Bool,
            Inlinable::Nothing => Type::Nothing,
        }
    }
}

/// A user type defined type or trait.
#[derive(Debug, PartialEq, Clone)]
pub struct DefType {
    pub kind: TypeKind,
    pub attributes: Vec<Attr>,

    pub type_parameters: Vec<Tr<u8>>, // for u8: 0 == 'a', 1 == 'b' etc.
    pub name: String,

    pub span: Ign<lumina_tokenizer::Span>,
}

/// Type kind-specific data.
#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Struct(Struct),
    Enum(Enum),
    Trait(Trait),
}

/// A trait decleration. Contains only header and type information.
#[derive(Debug, PartialEq, Clone)]
pub struct Trait {
    pub functions: HashMap<String, (Tr<ast::function::Header>, usize)>, // usize == method ID
}

impl Trait {
    pub fn new(functions: HashMap<String, (Tr<ast::function::Header>, usize)>) -> Trait {
        Trait { functions }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Struct {
    pub fields: Vec<(Tr<String>, Tr<Type>)>,
}

impl Struct {
    pub fn field(&self, k: &str) -> Option<(usize, &Tr<Type>)> {
        self.fields
            .iter()
            .enumerate()
            .find_map(|(i, (name, t))| if k == **name { Some((i, t)) } else { None })
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Enum {
    pub variants: Vec<(Tr<String>, TypeParameters)>,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fields
            .iter()
            .map(|(n, t)| format!("{} {}", n, t))
            .format(", ")
            .fmt(f)
    }
}

impl fmt::Display for Trait {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.functions.values().map(|(a, _)| a).format("\n").fmt(f)
    }
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.variants
            .iter()
            .map(|(n, ts)| format!("  {} {}", n, ts.iter().format(" ")))
            .format(", ")
            .fmt(f)
    }
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::Struct(a) => a.fmt(f),
            TypeKind::Enum(a) => a.fmt(f),
            TypeKind::Trait(meta) => meta.fmt(f),
        }
    }
}

impl fmt::Display for DefType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "attributes [{}]\n{} {} {}\n  {}",
            self.attributes.iter().format(", "),
            match self.kind {
                TypeKind::Struct(_) => "struct",
                TypeKind::Enum(_) => "enum",
                TypeKind::Trait(_) => "trait",
            }
            .green(),
            self.name,
            self.type_parameters
                .iter()
                .map(|b| (**b + b'a') as char)
                .format(" "),
            self.kind,
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn optional_params(params: &TypeParams) -> String {
            if params.is_empty() {
                String::new()
            } else {
                let base = params.iter().format(" ");
                format!(" {}", base)
            }
        }

        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Nothing => write!(f, "_"),
            Type::Defined(dt, params) => write!(f, "{}{}", &dt, optional_params(params)),
            Type::Pointer(inner) => write!(f, "*{}", inner),
            Type::Generic(i, params) => {
                write!(f, "{}{}", (i + b'a') as char, optional_params(params))
            }
            Type::TraitSelf(params) => write!(f, "self{}", optional_params(params)),
            Type::TypeParameter(_idx, genid, params) => {
                write!(f, "'{}{}", (genid + b'a') as char, optional_params(params))
            }
            Type::Bool => f.write_str("bool"),
            Type::Infer => write!(f, "*"),
            Type::List(inner) => {
                write!(f, "{}", "[".purple())?;
                inner.fmt(f)?;
                write!(f, "{}", "]".purple())
            }
            Type::Tuple(inners) => {
                write!(f, "{}", "[".purple())?;
                if let Some(fst) = inners.get(0) {
                    fst.fmt(f)?;
                }
                for inner in inners.iter().skip(1) {
                    f.write_str(", ")?;
                    inner.fmt(f)?;
                }
                write!(f, "{}", "]".purple())
            }
            Type::Closure(ptypes, returns) => write!(
                f,
                "{}{} {} {}{}",
                "(".purple(),
                ptypes.iter().format(", "),
                "->".purple(),
                returns,
                ")".purple()
            ),
            Type::Function(ptypes, returns) => write!(
                f,
                "fn{}{} {} {}{}",
                "(".purple(),
                ptypes.iter().format(", "),
                "->".purple(),
                returns,
                ")".purple()
            ),
            Type::Infallible => write!(f, "!"),
        }
    }
}
