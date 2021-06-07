use crate::Span;
use crate::Typing as BaseTyping;
use colored::Colorize;
use itertools::Itertools;
use lumina_parser::ast;
use lumina_typesystem::Tp;
pub use lumina_typesystem::{IType, Type, Typed};
use lumina_util::{Location, Tr};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub mod function;
pub use function::Entity;

/// A function with all identifiers (including that of the body) having been
/// identified/confirmed that they exist, and indexed into indices/IDs.
///
/// `Ident("x")` might turn into `Param(3)`
/// `Ident("f")` might turn into `Function(5)`
///
/// etc.
#[derive(Clone, Debug)]
pub struct Function {
    pub span: Span,

    pub header: function::Header,
    pub kind: function::Kind,
}

/// A user-defined type with all its identifiers (including that of the body) having been
/// identified/confirmed that they exist, and indexed into indices/IDs.
#[derive(Clone, Debug)]
pub struct UserType {
    pub span: Span,
    pub location: Rc<Location>,
    pub kind: UserTypeKind,
    pub type_params: Vec<Tr<u8>>,
}

impl UserType {
    pub fn to_lumina_type(&self, tid: usize) -> Tp<Type> {
        Tp::new(
            Type::defined(tid),
            self.type_params
                .iter()
                .map(|gid| Tr::tr(gid.span.clone(), Tp::none(Type::unbound(**gid))))
                .collect(),
        )
    }

    pub fn create_accessor(&self, struct_: Rc<Location>, tid: usize, field: usize) -> Function {
        match &self.kind {
            UserTypeKind::Struct(fields) => {
                let selected = &fields[field];
                let returns = selected.clone();
                let takes = Tr::tr(self.span.clone(), self.to_lumina_type(tid));
                Function {
                    span: self.span.clone(),
                    header: function::Header::Known(function::Typing {
                        ptypes: vec![takes],
                        returns,
                    }),
                    kind: function::Kind::Accessor { field, struct_ },
                }
            }
            UserTypeKind::PoisonedStruct(fields) => todo!(),
            otherkind => panic!(
                "create_accessor called on non-struct value: {:?}",
                otherkind
            ),
        }
    }

    pub fn create_constructor(&self, enum_: Rc<Location>, tid: usize, variant: usize) -> Function {
        todo!();
    }
}

#[derive(Clone, Debug)]
pub enum UserTypeKind {
    Struct(UserStruct),
    Enum(UserEnum),

    PoisonedStruct(Vec<Option<Tr<Tp<Type>>>>),
    PoisonedEnum(Vec<Option<Vec<Tr<Tp<Type>>>>>),
}

pub type UserStruct = Vec<Tr<Tp<Type>>>;

pub type UserEnum = Vec<Variant>;
type Variant = Vec<Tr<Tp<Type>>>;

#[derive(Clone, Debug)]
pub struct UserTrait {
    pub span: Span,
    pub location: Rc<Location>,

    pub type_params_n: usize,

    pub headers: HashMap<usize, Method>,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub self_position: SelfPosition,
    pub typing: Rc<BaseTyping<TraitType>>,
    pub trait_: usize,
}

impl Method {
    pub fn new(
        self_position: SelfPosition,
        typing: BaseTyping<TraitType>,
        trait_: usize,
    ) -> Method {
        Method {
            self_position,
            typing: Rc::new(typing),
            trait_,
        }
    }
}

/// We cache the location of all `self` identifiers in a method
#[derive(Clone, Debug)]
pub enum SelfPosition {
    Return,
    Param(usize),
    Multiple(Vec<SelfPosition>),
    None,
}

impl Default for SelfPosition {
    fn default() -> Self {
        Self::None
    }
}

impl SelfPosition {
    pub fn param(&mut self, pid: usize) {
        let p = SelfPosition::Param(pid);

        match self {
            SelfPosition::None => *self = p,
            SelfPosition::Multiple(existing) => existing.push(p),
            _ => *self = SelfPosition::Multiple(vec![self.clone(), p]),
        }
    }

    pub fn returns(&mut self) {}
}

/// Wrapper around our ordinary types for the trait method headers, since they need special
/// handling of the identifier `self`.
#[derive(Clone, Debug)]
pub enum TraitType {
    True(Type),
    Self_,
    Poisoned,
}

/// A lookup of the implementations of the methods for an implementation of a traits methods
pub type UserImpl = HashMap<usize, Function>;

pub type Pattern = lumina_patterns::Pattern<usize, function::Entity, usize, ast::Inlinable>;
pub type PatternTable =
    lumina_patterns::PatternTable<usize, function::Entity, usize, ast::Inlinable>;

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "fn ".purple().fmt(f)?;

        let (name, body) = match &self.kind {
            function::Kind::Defined(loc, body) => (loc.to_string(), body.to_string()),
            function::Kind::Accessor { field: id, struct_ } => {
                (format!("{}:{}", struct_, id), format!("field {}", id))
            }
            function::Kind::Constructor { variant: id, enum_ } => {
                (format!("{}:{}", enum_, id), format!("variant {}", id))
            }
            function::Kind::DefinedMethod {
                traitid,
                methodid,
                body,
                ..
            } => (format!("m{}:{}", traitid, methodid), body.to_string()),
            function::Kind::Where(body) => ("<where-bind>".into(), body.to_string()),
            function::Kind::Method {
                trait_,
                traitid,
                methodid,
                ..
            } => (
                format!(
                    "{}:{} <method for {}>",
                    traitid,
                    methodid,
                    trait_.to_string().green().to_string()
                ),
                String::new(),
            ),
            function::Kind::Poisoned => ("failed-function".into(), String::new()),
        };

        if body.len() > 10 {
            write!(f, "{} {}\n  {}", name.green(), &self.header, &body)
        } else {
            write!(f, "{} {} {}", name.green(), &self.header, &body)
        }
    }
}

impl fmt::Display for function::Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            function::Header::Known(typing) => write!(f, "{}", &typing),
            function::Header::Failed(salvaged) => write!(f, "{}", &salvaged),
        }
    }
}

impl fmt::Display for function::Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExprCall(expr, params) => write!(f, "({} {})", &expr, params.iter().format(" ")),
            Self::DotCall(left, right) => write!(f, "{}.{}", left, right),
            Self::Lambda(header, body, params) => {
                write!(
                    f,
                    "(\\{} -> {}) {}",
                    std::iter::repeat("_").take(header.params_len()).format(" "),
                    body,
                    params.iter().format(" "),
                )
            }
            Self::First(header, body, params) => {
                write!(
                    f,
                    "first(\\{} -> {}) {}",
                    std::iter::repeat("_").take(header.params_len()).format(" "),
                    body,
                    params.iter().format(" ")
                )
            }
            Self::Tuple(entries) => write!(f, "{{ {} }}", entries.iter().format(", ")),
            Self::Cast(from, to) => write!(f, "{} {} {}", from, "as".green(), to),
            Self::Identifier(takes, params) if params.is_empty() => takes.fmt(f),
            Self::Identifier(takes, params) => {
                write!(f, "({}", takes)?;
                for p in params.iter() {
                    f.write_str(" ")?;
                    p.inner.fmt(f)?;
                }
                write!(f, ")")
            }
            Self::Inlined(v) => v.fmt(f),
            Self::Builtin(builtin) => builtin.fmt(f),
            Self::Pass(rec) => write!(f, "#{}", rec),
            Self::List(_list_tid, entries) => write!(f, "[{}]", entries.iter().format(" ")),
            Self::ModifyRecord(base, assignments) => {
                write!(
                    f,
                    "{{ {} . {} }}",
                    base,
                    assignments
                        .iter()
                        .map(|(name, v)| format!("{} {}", name, v))
                        .format(", ")
                )
            }
            Self::ConstructRecord(tid, tp, assignments) => {
                write!(
                    f,
                    "{{ {} {} . {} }}",
                    tid,
                    tp.iter().format(" "),
                    assignments.iter().format(", ")
                )
            }
            Self::If(entries) => {
                f.write_str("if ")?;
                entries
                    .iter_conditions()
                    .zip(entries.iter_evaluations())
                    .map(|(cond, eval)| format!(" {} then {}", cond, eval))
                    .format(" else if ")
                    .fmt(f)?;
                write!(f, " else {}", entries.r#else())
            }
            Self::Match {
                kind: function::MatchKind::UserWritten,
                of,
                patterns: table,
                evals,
            } => {
                writeln!(f, "match {} with", of)?;
                for (pat, v) in table.patterns.iter().zip(evals.iter()) {
                    writeln!(f, "   {} -> ({} -> {}),", pat, v.0, v.1)?;
                }
                Ok(())
            }
            Self::Match {
                kind: function::MatchKind::LetBind,
                of,
                patterns: table,
                evals,
            } => {
                write!(
                    f,
                    "{} {} {} {} {} ({} -> {})",
                    "let".green(),
                    &table.patterns[0],
                    "=".green(),
                    of,
                    "in".green(),
                    &evals[0].0,
                    &evals[0].1
                )
            }
            Self::Unimplemented => "todo".fmt(f),
        }
    }
}
impl fmt::Display for function::Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Function(fid) => write!(f, "f{}", fid),
            Self::Method { trid, mid } => write!(f, "m{}:{}", trid, mid),
            Self::Param(pid) => write!(f, "p{}", pid),
            Self::Where(wid) => write!(f, "w{}", wid),
            Self::Parent(rec) => write!(f, "c-{}", rec),
        }
    }
}

impl fmt::Display for UserType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}\n  {}",
            "type".red(),
            &self.location,
            &self
                .type_params
                .iter()
                .map(|a| (a.inner + b'a') as char)
                .format("\n  ")
        )
    }
}

impl fmt::Display for UserTrait {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}\n  {}",
            "trait".red(),
            self.location,
            self.headers
                .iter()
                .map(|(mid, method)| format!(
                    "{} {} {}",
                    "fn".red(),
                    mid.to_string().green(),
                    &method.typing
                ),)
                .format("\n  ")
        )
    }
}

impl fmt::Display for TraitType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TraitType::True(t) => t.fmt(f),
            TraitType::Poisoned => f.write_str("<poisoned>"),
            TraitType::Self_ => f.write_str("self"),
        }
    }
}

impl fmt::Display for function::Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            function::Value::Int(n) => n.fmt(f),
        }
    }
}
