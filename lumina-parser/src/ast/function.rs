use super::{walker::Bounds, Entity, Type};
use crate::Attr;
use colored::*;
use itertools::Itertools;
use lumina_util::{PFlags, Tr};
use std::fmt;

/// A function header paired with an AST Entity.
#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub header: Header,
    pub name: String,
    pub body: Body,
}

/// A function header. Contains thnings like parameter names and type annotations
#[derive(PartialEq, Debug, Clone)]
pub struct Header {
    pub pnames: Vec<Tr<String>>,
    pub ptypes: Vec<Tr<Type>>,
    pub pflags: PFlags,
    pub returns: Tr<Type>,
    pub bounds: Bounds<Type>,
    pub attributes: Vec<Attr>,
}

impl Header {
    pub fn get_pid(&self, name: &str) -> Option<(usize, Tr<Type>)> {
        self.pnames.iter().enumerate().find_map(|(idx, n)| {
            if **n == name {
                Some((idx, self.ptypes[idx].clone()))
            } else {
                None
            }
        })
    }
    pub fn get_param(&self, name: &str) -> Option<(usize, Tr<Type>)> {
        self.pnames
            .iter()
            .zip(self.ptypes.iter())
            .enumerate()
            .find_map(|(i, (n, t))| {
                if **n == name {
                    Some((i, t.clone()))
                } else {
                    None
                }
            })
    }
    pub fn is_generic(&self) -> bool {
        self.ptypes
            .iter()
            .any(|t| matches!(t.inner, Type::Generic(_, _)))
    }
    pub fn types_known(&self) -> bool {
        !(self.ptypes.iter().any(|t| **t == Type::Infer) || *self.returns == Type::Infer)
    }

    // Finds the first unused genid
    pub fn first_unused_generic(&self) -> u8 {
        let ptypes = &self.ptypes;

        let mut genid = 0;

        'outer: loop {
            for t in ptypes.iter() {
                if let Type::Generic(id, _) = &**t {
                    if genid == *id {
                        genid += 1;
                        continue 'outer;
                    }
                }
            }

            if let Type::Generic(id, _) = *self.returns {
                if genid == id {
                    genid += 1;
                    continue 'outer;
                }
            }
            break 'outer;
        }

        genid
    }
}

/// An AST Entity paired with any `where` bindings declared in its scope.
#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub entity: Tr<Entity>,
    pub wheres: Vec<(Tr<String>, WhereBinding)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhereBinding {
    pub kind: WhereKind,
    pub entity: Tr<Entity>,
}

/// Where bindings can both be lazily evaluated in the form of `fn` or strictly evaluated in the
/// form of `let`
#[derive(Debug, Clone, PartialEq)]
pub enum WhereKind {
    Fn(Header),
    Let,
}

impl WhereBinding {
    pub fn new(kind: WhereKind, entity: Tr<Entity>) -> Self {
        Self { kind, entity }
    }
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.entity.fmt(f)?;

        if self.wheres.is_empty() {
            return Ok(());
        }
        for (sym, bind) in self.wheres.iter() {
            writeln!(f)?;
            write!(f, "  {} ", "where".green())?;
            match &bind.kind {
                WhereKind::Let => write!(f, "let {} = {}", sym, &bind.entity),
                WhereKind::Fn(header) => write!(f, "fn {} {} -> {}", sym, header, &bind.entity),
            }?;
        }
        Ok(())
    }
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.pnames.iter().format(" "), "(".purple())?;
        if let Some(pt) = self.ptypes.get(0) {
            pt.fmt(f)?;
        }
        for pt in self.ptypes.iter().skip(1) {
            write!(f, ", ")?;
            pt.fmt(f)?;
        }
        write!(f, " {} ", "->".purple())?;
        self.returns.fmt(f)?;
        write!(f, "{}", ")".purple())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let large = self.body.entity.to_string().len() > 10;

        let start = format!("{} {} {}", "fn".purple(), self.name.green(), &self.header);

        if large {
            write!(f, "{}\n  {}", start, &self.body)
        } else {
            write!(f, "{} {}", start, &self.body)
        }
    }
}
