use super::*;
use colored::*;
use itertools::Itertools;
use lumina_tokenizer::Span;
use lumina_util::{Location, LocationError};
use std::fmt;

/// Error enum for the `Source -> Token -> AST` pipeline.
///
/// We mainly pick up on syntax errors here.
#[derive(Debug)]
pub enum Error {
    Positioned(Span, Box<Error>),

    InvalidGeneric(String),
    InvalidIdentifier(LocationError),
    ExpectedButGot(String, String),
    Unexpected(String),
    UnexpectedAsPattern(String),
    OperatorMissingLeftSide(String),
    TypeParameterOnPrimitive(Type, TypeParameters),
    MissingReturnType,
    GenericNotDeclared(u8),
    UnknownAttribute(String),
    UnknownPFlag,
    IdentifierAsPath(Location),
    UnindentedWhereBinding,
    WhereBindingMissingHeader(String),
    PassMissingRightSide,
    DotCallMissingRightSide,

    TokenError,
    IOError(std::io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;

        match self {
            Positioned(span, err) => write!(f, "{:#?}.{}", span, err),

            InvalidGeneric(gen) => write!(f, "{} is not a valid generic", gen.purple()),
            InvalidIdentifier(LocationError::InvalidChar(c)) => write!(
                f,
                "Invalid identifier. The character {} is not allowed",
                c.to_string().purple()
            ),
            ExpectedButGot(exp, got) => write!(f, "expected {} but got {}", exp, got),
            Unexpected(got) => write!(f, "unexpected {}", got),
            UnexpectedAsPattern(pat) => write!(f, "{} is not a valid pattern", pat),
            OperatorMissingLeftSide(v) => {
                write!(f, "the operator {} has no value to its left", v.purple())
            }
            TypeParameterOnPrimitive(prim, tp) => write!(
                f,
                "{} is a primitive and cannot take parameters, but was given {}",
                prim.to_string().purple(),
                tp.iter().format(" ").to_string().purple()
            ),
            MissingReturnType => write!(f, "this function is missing a return type"),
            GenericNotDeclared(gid) => write!(
                f,
                "{} is a generic, but it wasn't declared as the declared types type parameter",
                ((gid + b'a') as char).to_string().purple()
            ),
            UnknownAttribute(attr) => write!(f, "{} is not a known attribute", attr.purple()),
            UnknownPFlag => "unknown pflag".fmt(f),
            IdentifierAsPath(_loc) => write!(f, "a path cannot be used as identifier"),
            UnindentedWhereBinding => write!(
                f,
                "where bindings needs to be indented above their attached function"
            ),
            WhereBindingMissingHeader(got) => write!(
                f,
                "a where binding needs to start with either `{}` or `{}`, but instead i got {}",
                "let".green(),
                "fn".green(),
                got.purple()
            ),
            PassMissingRightSide => write!(
                f,
                "this `{}` needs to be followed by an expression",
                "#".green()
            ),
            DotCallMissingRightSide => write!(f, "this `.` needs to be followed by an expression"),

            TokenError => "unknown token".fmt(f),
            IOError(err) => err.fmt(f),
        }
    }
}

impl Error {
    pub fn idx(self, i: lumina_tokenizer::Span) -> Error {
        if let Error::Positioned(_, _) = self {
            return self;
        }
        Error::Positioned(i, Box::new(self))
    }
    pub fn idxed(i: lumina_tokenizer::Span, e: Error) -> Error {
        e.idx(i)
    }

    /// pretty-print the error as a lumina error then panic
    pub fn test_failure<T>(v: Result<T, Error>, src: &str) -> T {
        use lumina_util::ToError;

        match v {
            Err(e) => {
                println!("{}", e.into_lumina_error(src, String::from("test")));
                panic!()
            }
            Ok(v) => v,
        }
    }
}

impl lumina_util::ToError for Error {
    fn name(&self) -> &'static str {
        use Error::*;

        match self {
            Positioned(_, e) => e.name(),

            InvalidGeneric(_) => "invalid generic",
            InvalidIdentifier(_) => "invalid identifier",
            ExpectedButGot(_, _) | Unexpected(_) => "unexpected entity",
            UnexpectedAsPattern(_) => "unexpected entity in pattern",
            OperatorMissingLeftSide(_) => "operator without left parameter",
            TypeParameterOnPrimitive(_, _) => "primitive type with parameters",
            MissingReturnType => "missing return type",
            GenericNotDeclared(_) => "undeclared generic",
            UnknownAttribute(_) => "unknown attribute",
            UnknownPFlag => "unknown pflag",
            IdentifierAsPath(_) => "path as identifier",
            UnindentedWhereBinding => "wrongly indented where-binding",
            WhereBindingMissingHeader(_) => "where-binding missing header",
            PassMissingRightSide => "empty closure pass",
            DotCallMissingRightSide => "dotcall without parameter",

            TokenError => "token failure",
            IOError(_) => "file failure",
        }
    }

    fn span(&self) -> Option<lumina_tokenizer::Span> {
        match &self {
            Self::Positioned(ptr, _) => Some(ptr.clone()),
            _ => None,
        }
    }

    fn text(&self) -> String {
        match self {
            Error::Positioned(_, inner) => inner.to_string(),
            other => other.to_string(),
        }
    }
}

impl From<LocationError> for Error {
    fn from(e: LocationError) -> Error {
        Error::InvalidIdentifier(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::IOError(e)
    }
}
