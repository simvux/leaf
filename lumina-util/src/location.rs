use std::fmt;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::path::PathBuf;

// NOTE: The rest of the location.rs code relies on these 4 being just ascii
pub const PROJECT_PATH: &str = "mod";
pub const STD_PATH: &str = "std";
pub const EXT_PATH: &str = "ext";
pub const BUILTIN_PATH: &str = "builtin";
pub const DELIM_CHAR: char = ':';

// TODO: We probably want to create `LocationBuf` and `&Location` that have the inner types `String` and `str`

/// Location represents an identifier in Lumina.
///
/// It can be either relative or absolute paths. For details see `Pathing`
#[derive(Debug, Clone)]
pub struct Location {
    inner: String,
    pub is_local: bool,
    pub pathing: Pathing,
}

impl Default for Location {
    fn default() -> Self {
        Self::prelude()
    }
}

/// Pathing represents what `mode` this identifier is in.
///
/// For example;
///
/// `Project  == mod:???`
/// `Library  == std:???`
/// `Relative == ???`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pathing {
    Prelude,
    Project,
    Library,
    Builtin,
    Relative,
}

impl Location {
    pub unsafe fn from_raw_parts(s: String, is_local: bool, pathing: Pathing) -> Location {
        Location {
            inner: s,
            is_local,
            pathing,
        }
    }

    pub fn borrow_raw(&self) -> &str {
        &self.inner
    }

    pub fn prelude() -> Self {
        Location {
            inner: String::new(),
            pathing: Pathing::Prelude,
            is_local: false,
        }
    }

    pub fn from_string(mut s: String) -> Result<Location, LocationError> {
        let location = match MarkResult::from(s.chars()) {
            MarkResult::Std | MarkResult::Ext => {
                verify_integrity(s[STD_PATH.as_bytes().len() - 1..].chars())?;
                Location {
                    is_local: false,
                    inner: s,
                    pathing: Pathing::Library,
                }
            }
            MarkResult::Mod => {
                s.replace_range(0..PROJECT_PATH.as_bytes().len() + 1, "");
                verify_integrity(s[PROJECT_PATH.as_bytes().len() - 1..].chars())?;
                Location {
                    is_local: false,
                    inner: s,
                    pathing: Pathing::Project,
                }
            }
            MarkResult::Builtin => {
                verify_integrity(s[BUILTIN_PATH.as_bytes().len() - 1..].chars())?;
                Location {
                    is_local: false,
                    inner: s,
                    pathing: Pathing::Builtin,
                }
            }
            MarkResult::None => {
                let is_local = verify_integrity(s.chars())?;
                Location {
                    is_local,
                    inner: s,
                    pathing: Pathing::Relative,
                }
            }
        };
        Ok(location)
    }

    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Location, LocationError> {
        let (is_local, inner, pathing) = match MarkResult::from(s.as_ref().chars()) {
            MarkResult::Std => {
                let to_check = &s.as_ref()[STD_PATH.as_bytes().len() + 1..];
                verify_integrity(to_check.chars())?;
                (false, s.as_ref(), Pathing::Library)
            }
            MarkResult::Ext => {
                let to_check = &s.as_ref()[EXT_PATH.as_bytes().len() + 1..];
                verify_integrity(to_check.chars())?;
                (false, s.as_ref(), Pathing::Library)
            }
            MarkResult::Builtin => {
                let s = &s.as_ref()[BUILTIN_PATH.as_bytes().len() + 1..];
                verify_integrity(s.chars())?;
                (false, s, Pathing::Builtin)
            }
            MarkResult::Mod => {
                let s = &s.as_ref()[PROJECT_PATH.as_bytes().len() + 1..];
                verify_integrity(s.chars())?;
                (false, s, Pathing::Project)
            }
            MarkResult::None => {
                let s = s.as_ref();
                let is_local = verify_integrity(s.chars())?;
                (is_local, s, Pathing::Relative)
            }
        };
        Ok(Location {
            inner: inner.to_string(),
            is_local,
            pathing,
        })
    }

    /// Expand an relative location using `self` as the parent module.
    ///
    /// If the identifier is not relative then this function does nothing.
    pub fn expand(&self, mut other: Location) -> Location {
        self.expand_mut(&mut other);
        other
    }

    /// In-place version of `expand`
    pub fn expand_mut(&self, other: &mut Location) {
        match other.pathing {
            Pathing::Prelude | Pathing::Project | Pathing::Library | Pathing::Builtin => {}
            Pathing::Relative => {
                if !other.is_local && other.first() == self.last() {
                    other.remove_first();
                }
                other.inner.insert(0, DELIM_CHAR);
                other.inner.insert_str(0, &self.inner);
                other.pathing = self.pathing.clone();
            }
        }
    }

    pub fn top_level(&self) -> Location {
        match self.pathing {
            Pathing::Prelude | Pathing::Project | Pathing::Relative | Pathing::Builtin => {
                panic!("Cannot get top-level from non-library location")
            }
            Pathing::Library => {}
        }
        if self.first() == STD_PATH {
            unsafe { Location::from_raw_parts("std".to_string(), false, Pathing::Library) }
        } else {
            debug_assert_eq!(self.first(), EXT_PATH);

            let raw = self.borrow_raw();
            let to_include = raw[EXT_PATH.len() + 1..]
                .chars()
                .position(|c| c == DELIM_CHAR)
                .map(|n| n + EXT_PATH.len() + 1)
                .unwrap_or(raw.len());

            unsafe {
                Location::from_raw_parts(raw[0..to_include].to_string(), false, Pathing::Library)
            }
        }
    }

    pub fn remove_first(&mut self) {
        let first_delim = self.inner.chars().position(|c| c == DELIM_CHAR).unwrap();
        self.inner.replace_range(0..=first_delim, "");
    }

    pub fn same_dir(&self, other: Location) -> Location {
        match other.pathing {
            Pathing::Prelude | Pathing::Project | Pathing::Library | Pathing::Builtin => other,
            Pathing::Relative => {
                let mut base = self.clone();
                loop {
                    match base.inner.pop() {
                        None => break,
                        Some(c) if c == DELIM_CHAR => break,
                        Some(_) => continue,
                    }
                }
                base.inner.push(DELIM_CHAR);
                base.inner += &other.inner;
                base
            }
        }
    }

    // TODO: Do I need to verify valid characters? I think I do.
    pub fn push<S: AsRef<str>>(&mut self, s: S) {
        if !self.inner.is_empty() {
            self.inner.push(DELIM_CHAR);
        }
        self.inner.push_str(s.as_ref());
    }
    pub fn push_char(&mut self, c: char) {
        if !self.inner.is_empty() {
            self.inner.push(DELIM_CHAR);
        }
        self.inner.push(c);
    }

    pub fn pop(&mut self) {
        match self.inner.pop() {
            None | Some(':') => (),
            _ => self.pop(),
        }
    }

    pub fn entries(&self) -> std::str::Split<'_, impl FnMut(char) -> bool> {
        self.inner.split(|c| c == DELIM_CHAR)
    }

    pub fn restore(mut self) -> String {
        match self.pathing {
            Pathing::Builtin => self.inner.insert_str(0, "builtin:"),
            Pathing::Project => self.inner.insert_str(0, "mod:"),
            _ => (),
        }
        self.inner
    }

    pub fn first(&self) -> &str {
        let stop_at = self
            .inner
            .chars()
            .position(|c| c == DELIM_CHAR)
            .unwrap_or(self.inner.len());
        &self.inner[0..stop_at]
    }
    pub fn last(&self) -> &str {
        let start_at = self
            .inner
            .chars()
            .rev()
            .position(|c| c == DELIM_CHAR)
            .unwrap_or(self.inner.len());
        &self.inner[self.inner.len() - start_at..]
    }

    pub fn to_pathbuf(&self, env: &lumina_env::Environment) -> PathBuf {
        let mut path = self.to_pathbuf_folder(env);
        path.set_extension("lm");
        path
    }
    pub fn to_pathbuf_folder(&self, env: &lumina_env::Environment) -> PathBuf {
        match self.pathing {
            Pathing::Project => {
                let mut base = env.project_folder.clone();
                let entries = self.entries();
                for entry in entries {
                    base.push(entry);
                }
                base
            }
            Pathing::Library => {
                let mut base = env.luminapath.clone();
                let entries = self.entries();
                base.push("modules");
                for entry in entries {
                    base.push(entry);
                }
                base
            }
            Pathing::Builtin => panic!("Cannot get path of builtin: {}", self.inner),
            Pathing::Relative => panic!("Cannot get path of relative: {}", self.inner),
            Pathing::Prelude => panic!("Cannot get path of prelude: {}", self.inner),
        }
    }

    pub fn read_source_code(
        &self,
        env: &lumina_env::Environment,
    ) -> Result<(String, PathBuf), std::io::Error> {
        let mut source = String::with_capacity(20);
        let path = self.to_pathbuf(env);
        File::open(&path)?.read_to_string(&mut source)?;
        Ok((source, path))
    }
}

#[derive(Debug)]
pub enum LocationError {
    InvalidChar(char),
}

fn verify_integrity<I: Iterator<Item = char>>(mut iter: I) -> Result<bool, LocationError> {
    // is_local remains true unless DELIM_CHAR exists.
    let mut is_local = true;

    loop {
        let c = match iter.next() {
            None => break,
            Some(c) => c,
        };
        if c == DELIM_CHAR {
            is_local = false;
            continue;
        }
    }

    Ok(is_local)
}

enum MarkResult {
    Mod,
    Std,
    Ext,
    Builtin,
    None,
}

// mod:hello -> PROJECTMARK + hello
// std:hello -> LEAFPATHMARK + std:hello
// ext:hello -> LEAFPATHMARK + ext:hello
// other:hello -> RELATIVE_MARK + hello
impl<I: Iterator<Item = char>> From<I> for MarkResult {
    fn from(mut iter: I) -> MarkResult {
        match iter.next() {
            Some('m') => {
                if iter.take(3).eq("od:".chars()) {
                    return MarkResult::Mod;
                }
            }
            Some('s') => {
                if iter.take(3).eq("td:".chars()) {
                    return MarkResult::Std;
                }
            }
            Some('e') => {
                if iter.take(3).eq("xt:".chars()) {
                    return MarkResult::Ext;
                }
            }
            Some('b') => {
                if iter.take(7).eq("uiltin:".chars()) {
                    return MarkResult::Builtin;
                }
            }
            Some(_) => {}
            None => panic!("empty location"),
        }
        MarkResult::None
    }
}

impl AsRef<str> for Location {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}
impl Into<String> for Location {
    fn into(self) -> String {
        self.inner
    }
}

impl PartialEq for Location {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner) && self.pathing.eq(&other.pathing)
    }
}
// TODO: Is this invalid? Maybe this type is PartialEq
impl Eq for Location {}

impl Hash for Location {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.inner.hash(hasher);
        self.pathing.hash(hasher);
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.pathing {
            Pathing::Project => write!(f, "mod:")?,
            Pathing::Builtin => write!(f, "builtin:")?,
            _ => (),
        };
        write!(f, "{}", &self.inner)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lumina_env;

    #[test]
    fn construct() {
        assert_eq!(
            Location::from_str("add").unwrap(),
            Location {
                inner: "add".to_owned(),
                is_local: true,
                pathing: Pathing::Relative,
            },
        );

        assert_eq!(
            Location::from_str("math:add").unwrap(),
            Location {
                inner: "math:add".to_owned(),
                is_local: false,
                pathing: Pathing::Relative,
            },
        );

        assert_eq!(
            Location::from_string("math:add".to_owned()).unwrap(),
            Location {
                inner: "math:add".to_owned(),
                is_local: false,
                pathing: Pathing::Relative,
            },
        );

        assert_eq!(
            Location::from_str("mod:main:add").unwrap(),
            Location {
                inner: "main:add".into(),
                is_local: false,
                pathing: Pathing::Project,
            }
        );

        assert_eq!(
            Location::from_str("ext:main:add").unwrap(),
            Location {
                inner: "ext:main:add".into(),
                is_local: false,
                pathing: Pathing::Library,
            }
        );

        assert_eq!(
            Location::from_str("builtin:add").unwrap(),
            Location {
                inner: "add".into(),
                is_local: false,
                pathing: Pathing::Builtin,
            }
        );
    }

    #[test]
    fn expand() {
        let base = Location::from_str("mod:main").unwrap();
        let local_func = Location::from_str("add").unwrap();
        assert_eq!(
            base.expand(local_func),
            Location {
                inner: "main:add".into(),
                is_local: true,
                pathing: Pathing::Project,
            },
        );
        let relative_call = Location::from_str("math:add").unwrap();
        assert_eq!(
            base.expand(relative_call),
            Location {
                inner: "main:math:add".into(),
                is_local: false,
                pathing: Pathing::Project,
            }
        );
        let abs_call = Location::from_str("std:list:range").unwrap();
        assert_eq!(
            base.expand(abs_call),
            Location {
                inner: "std:list:range".into(),
                is_local: false,
                pathing: Pathing::Library,
            },
        );

        assert_eq!(
            Location {
                inner: "std:list".into(),
                is_local: false,
                pathing: Pathing::Library,
            }
            .expand(Location::from_str("range").unwrap()),
            Location {
                inner: "std:list:range".into(),
                is_local: true,
                pathing: Pathing::Library,
            }
        );
    }

    #[test]
    fn to_pathbuf() {
        let env = lumina_env::Environment::from(
            "main".into(),
            PathBuf::from("/fake-project/"),
            PathBuf::from("/fake-luminapath/"),
        );
        let loc = Location::from_str("std:list").unwrap();
        assert_eq!(
            loc.to_pathbuf(&env),
            PathBuf::from("/fake-luminapath/modules/std/list.lm"),
        );
        let loc = Location::from_str("mod:main").unwrap();
        let mut exp = env.project_folder.clone();
        exp.push("main");
        exp.set_extension("lm");
        assert_eq!(loc.to_pathbuf(&env), exp);
    }

    #[test]
    fn first_last() {
        let loc1 = Location::from_str("test").unwrap();
        let loc2 = Location::from_str("std:list:range").unwrap();
        assert_eq!(loc1.first(), "test");
        assert_eq!(loc1.last(), "test");
        assert_eq!(loc2.first(), "std");
        assert_eq!(loc2.last(), "range");
    }
}
