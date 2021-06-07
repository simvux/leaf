use itertools::Itertools;
use lumina_util::{Ign, Primitive, Tr};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::ops::{Deref, DerefMut};

mod error;
pub use error::{Error, LookupError};
mod impls;
pub use impls::{Scorer, TraitImpls};
mod genbuffer;
pub use genbuffer::GenBuffer;

type Bound = Tp<TRID>;
type TRID = usize;
type TID = usize;
type GenID = Ign<u8>;

/// IType is like a Type but also manages type inference
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IType {
    True(Type),
    Infer,
}

pub const INFERENCE_ERROR: &str = "inference error";

/// Type can be either a generic type or an direct type, where direct type includes both primitives
/// and user defined types.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Generic(Generic),
    Direct(Direct),
}

impl Type {
    /// Our ordinary PartialEq implementation treats the generics `a` and `b` as equal to simplify
    /// hashing. This function is more strict with that rule, requiring the genid's the match up.
    pub fn true_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Generic(Generic::Unbound(s)), Self::Generic(Generic::Unbound(o))) if s == o => {
                true
            }
            (Self::Generic(Generic::Bound(s, sb)), Self::Generic(Generic::Bound(o, ob)))
                if s == o && sb == ob =>
            {
                true
            }
            (Self::Direct(s), Self::Direct(o)) if s == o => true,
            _ => false,
        }
    }
}

impl From<Generic> for Type {
    fn from(g: Generic) -> Type {
        Type::Generic(g)
    }
}
impl From<Direct> for Type {
    fn from(dt: Direct) -> Type {
        Type::Direct(dt)
    }
}
impl From<GenID> for Type {
    fn from(gid: GenID) -> Type {
        Type::Generic(Unbound(gid))
    }
}
impl From<(GenID, Vec<Bound>)> for Type {
    fn from((gid, bounds): (GenID, Vec<Bound>)) -> Type {
        Type::Generic(Bound(gid, bounds))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Generic {
    Unbound(GenID),
    Bound(GenID, Vec<Bound>),
}
use Generic::*;

impl Generic {
    pub fn gid(&self) -> GenID {
        match self {
            Self::Unbound(gid) | Self::Bound(gid, _) => gid.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Direct {
    Primitive(Primitive),
    Tuple,
    Pointer,
    FnPointer,
    Defined(TID),
}
use Direct::*;

impl ITp<IType> {
    pub fn unwrap(&self) -> Tp<Type> {
        Tp::new(
            self.root.clone().unwrap(),
            self.params
                .iter()
                .map(|trt| trt.as_ref().map(|t| t.unwrap()))
                .collect(),
        )
    }
}

impl IType {
    pub fn infer() -> IType {
        IType::Infer
    }

    pub fn unwrap(self) -> Type {
        self.ok_or_else(|| panic!("unwrap called on an IType::Infer value"))
    }

    pub fn as_known(&self) -> &Type {
        match self {
            Self::True(t) => t,
            Self::Infer => panic!("`as_known` on an IType::Infer value"),
        }
    }

    pub fn ok_or_else(self, mut f: impl FnMut() -> Type) -> Type {
        match self {
            IType::True(t) => t,
            IType::Infer => f(),
        }
    }
}

pub trait Typed {
    fn unbound(gid: u8) -> Self;
    fn bound(gid: u8, bounds: Vec<Bound>) -> Self;
    fn prim(p: Primitive) -> Self;
    fn defined(tid: usize) -> Self;
    fn tuple() -> Self;
    fn pointer() -> Self;
    fn fn_pointer() -> Self;

    fn int() -> Self
    where
        Self: Sized,
    {
        Self::prim(Primitive::Int)
    }
    fn float() -> Self
    where
        Self: Sized,
    {
        Self::prim(Primitive::Float)
    }
    fn bool() -> Self
    where
        Self: Sized,
    {
        Self::prim(Primitive::Bool)
    }
    fn unit() -> Self
    where
        Self: Sized,
    {
        Self::prim(Primitive::Int)
    }
}

impl Typed for IType {
    fn unbound(gid: u8) -> IType {
        Type::unbound(gid).into()
    }
    fn bound(gid: u8, bounds: Vec<Bound>) -> IType {
        Type::bound(gid, bounds).into()
    }
    fn prim(p: Primitive) -> IType {
        Type::prim(p).into()
    }
    fn defined(tid: usize) -> IType {
        Type::defined(tid).into()
    }
    fn tuple() -> IType {
        Type::tuple().into()
    }
    fn pointer() -> IType {
        Type::pointer().into()
    }
    fn fn_pointer() -> IType {
        Type::fn_pointer().into()
    }
}

impl From<Type> for IType {
    fn from(t: Type) -> IType {
        IType::True(t)
    }
}

impl<'a> TryFrom<&'a IType> for &'a Type {
    type Error = ();

    fn try_from(t: &'a IType) -> Result<Self, Self::Error> {
        match t {
            IType::True(t) => Ok(t),
            IType::Infer => panic!(INFERENCE_ERROR),
        }
    }
}

impl Typed for Type {
    fn int() -> Type {
        Type::prim(Primitive::Int)
    }
    fn unbound(gid: u8) -> Type {
        Type::Generic(Unbound(Ign::new(gid)))
    }
    fn bound(gid: u8, bounds: Vec<Bound>) -> Type {
        Type::Generic(Bound(Ign::new(gid), bounds))
    }
    fn prim(p: Primitive) -> Type {
        Type::Direct(Primitive(p))
    }
    fn defined(tid: usize) -> Type {
        Type::Direct(Defined(tid))
    }
    fn tuple() -> Type {
        Type::Direct(Tuple)
    }
    fn pointer() -> Type {
        Type::Direct(Pointer)
    }
    fn fn_pointer() -> Type {
        Type::Direct(Direct::FnPointer)
    }
}

pub type TypeParameters = Vec<Tr<Tp<Type>>>;
pub type TypeParams = [Tr<Tp<Type>>];

pub type ITypeParameters = Vec<Tr<ITp<IType>>>;
pub type ITypeParams = [Tr<ITp<IType>>];

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Tp<T> {
    pub root: T,
    pub params: TypeParameters,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ITp<T> {
    pub root: T,
    pub params: ITypeParameters,
}

impl<T> Tp<T> {
    pub fn none(root: T) -> Self {
        Self::new(root, TypeParameters::new())
    }

    pub fn new(root: T, params: TypeParameters) -> Self {
        Self { root, params }
    }
}

impl Tp<Type> {
    pub fn into_unknown(self) -> ITp<IType> {
        ITp::new(
            IType::True(self.root),
            self.params
                .into_iter()
                .map(|t| Tr::tr(t.span.clone(), t.inner.into_unknown()))
                .collect(),
        )
    }
}

impl<T> ITp<T> {
    pub fn none(root: T) -> Self {
        Self::new(root, ITypeParameters::new())
    }

    pub fn new(root: T, params: ITypeParameters) -> Self {
        Self { root, params }
    }
}

impl ITp<IType> {
    pub fn is_known(&self) -> bool {
        self.root != IType::infer() && self.params.iter().all(|t| t.is_known())
    }
    pub fn to_known(&self) -> Result<Tp<Type>, Option<std::ops::Range<usize>>> {
        let root = match &self.root {
            IType::True(t) => t.clone(),
            IType::Infer => return Err(None),
        };

        let tp = self
            .params
            .iter()
            .map(|p| match p.to_known() {
                Err(None) => Err(Some(p.span.clone())),
                Err(e) => Err(e),
                Ok(t) => Ok(Tr::tr(p.span.clone(), t)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Tp::new(root, tp))
    }
}

/// Iterates the types to generate a unique unused GID that's as close to `a` as possible
pub fn generate_unused_gid(types: &[Tr<ITp<IType>>]) -> u8 {
    fn gen(mut lowest: u8, types: &[Tr<ITp<IType>>]) -> u8 {
        for t in types {
            match &t.root {
                IType::True(Type::Generic(gen)) if *gen.gid() == lowest => lowest += 1,
                _ => {}
            }

            lowest = gen(lowest, &t.params);
        }
        lowest
    }

    gen(0, types)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Generic(gt) => gt.fmt(f),
            Type::Direct(dt) => dt.fmt(f),
        }
    }
}

impl fmt::Display for IType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IType::True(t) => t.fmt(f),
            IType::Infer => "*".fmt(f),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Tp<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.params.is_empty() {
            self.root.fmt(f)
        } else {
            write!(f, "{} {}", self.root, self.params.iter().format(" "))
        }
    }
}

impl<T: fmt::Display> fmt::Display for ITp<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.params.is_empty() {
            self.root.fmt(f)
        } else {
            write!(f, "{} {}", self.root, self.params.iter().format(" "))
        }
    }
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn gfmt(gid: &GenID) -> char {
            (gid.inner + b'a') as char
        }

        match self {
            Generic::Unbound(genid) => gfmt(genid).fmt(f),
            Generic::Bound(genid, bounds) => {
                write!(f, "{}_when[{}]", gfmt(genid), bounds.iter().format(","))
            }
        }
    }
}

impl fmt::Display for Direct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Direct::Tuple => "tuple".fmt(f),
            Direct::Primitive(prim) => prim.fmt(f),
            Direct::Pointer => "*".fmt(f),
            Direct::FnPointer => "fn".fmt(f),
            Direct::Defined(tid) => write!(f, "t{}", tid),
        }
    }
}
