use super::*;
use std::fmt;

/// Used for placing compile-time arguments to the compiler.
/// Can be used for things like generating code only for specific targets are enabling certain
/// compile-time magic features.
///
/// Is attached to a function, type, etc. Through the `attributes` keyword.
///
/// ```lumina
/// attributes [rc, linux]
/// struct File
///     fd int
/// ```
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Attr {
    Windows,
    Macos,
    Linux,
    Unix,

    Rc,
    IO,
}

impl Attr {
    #[cfg(target_os = "windows")]
    pub fn is_targeted_sys(self) -> Option<bool> {
        match self {
            Attr::Windows => Some(true),
            Attr::Linux | Attr::Macos | Attr::Unix => Some(false),
            _ => None,
        }
    }

    #[cfg(target_os = "linux")]
    pub fn is_targeted_sys(self) -> Option<bool> {
        match self {
            Attr::Linux => Some(true),
            Attr::Windows | Attr::Macos | Attr::Unix => Some(false),
            _ => None,
        }
    }

    #[cfg(target_os = "unix")]
    pub fn is_targeted_sys(self) -> Option<bool> {
        match self {
            Attr::Unix => Some(true),
            Attr::Linux | Attr::Macos | Attr::Windows => Some(false),
            _ => None,
        }
    }

    #[cfg(target_os = "macos")]
    pub fn is_targeted_sys(self) -> Option<bool> {
        match self {
            Attr::Macos => Some(true),
            Attr::Linux | Attr::Unix | Attr::Windows => Some(false),
            _ => None,
        }
    }

    #[cfg(not(any(
        target_os = "macos",
        target_os = "linux",
        target_os = "windows",
        target_os = "unix"
    )))]
    pub fn is_targeted_sys(self) -> Option<bool> {
        None
    }

    pub fn try_from(s: &str) -> Result<Attr, Error> {
        let attr = match s {
            "windows" => Attr::Windows,
            "linux" => Attr::Linux,
            "macos" | "darwin" => Attr::Macos,
            "rc" => Attr::Rc,
            "io" => Attr::IO,
            _ => return Err(Error::UnknownAttribute(s.into())),
        };
        Ok(attr)
    }
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?}", self).to_lowercase())
    }
}
