use colored::Colorize;
use lumina_util::Ign;
use std::fmt;

// TODO: Since we need to use the callstack to detect recursive depth of types as well, we can't
// use this `Call` enum. I suppose we'd need to make it more generic?

#[derive(Default, Debug)]
pub struct CallStack<C, T> {
    inner: Vec<(C, T)>,
}

/// FCall represents what kind of call a function is.
///
/// This is mainly used to detect recursion.
#[derive(Debug, Clone, PartialEq, Hash, Copy)]
pub enum FCall {
    Function(usize),
    Method {
        raw_fid: Ign<usize>,
        trid: usize,
        mid: usize,
    },
    Lambda,
    Let,
}

impl FCall {
    pub fn is_this_function(&self, fid: usize) -> bool {
        matches!(self,
            FCall::Function(f) if *f == fid
        )
    }

    pub fn is_this_method(&self, trid_: usize, mid_: usize) -> bool {
        matches!(self,
            FCall::Method { trid, mid, .. } if trid_ == *trid && mid_ == *mid
        )
    }

    pub fn fid(&self) -> Option<usize> {
        match self {
            FCall::Function(fid) => Some(*fid),
            FCall::Method { raw_fid, .. } => Some(**raw_fid),
            _ => None,
        }
    }
}

pub type Depth = u32;

impl<T> CallStack<FCall, T> {
    pub fn fn_is_called(&self, fid: usize) -> Option<(Depth, &T)> {
        self.get_with_depth(|call| call.is_this_function(fid))
    }

    pub fn method_is_called(&self, trid: usize, mid: usize) -> Option<(Depth, &T)> {
        self.get_with_depth(|call| call.is_this_method(trid, mid))
    }
}

impl<C, T> CallStack<C, T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(4),
        }
    }

    pub fn pop(&mut self) -> T {
        self.inner.pop().unwrap().1
    }

    pub fn push(&mut self, k: C, v: T) {
        self.inner.push((k, v))
    }

    fn get_with_depth(&self, check: impl Fn(&C) -> bool) -> Option<(Depth, &T)> {
        self.inner
            .iter()
            .rev()
            .enumerate()
            .find_map(|(depth, (call, v))| {
                if check(call) {
                    Some((depth as Depth, v))
                } else {
                    None
                }
            })
    }

    pub fn last(&self) -> &T {
        &self.inner.last().unwrap().1
    }
    pub fn last_mut(&mut self) -> &mut T {
        &mut self.inner.last_mut().unwrap().1
    }
}

impl fmt::Display for FCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FCall::Function(fid) => write!(f, "f{}", fid),
            FCall::Method { trid, mid, .. } => write!(f, "m{}:{}", trid, mid),
            FCall::Let => write!(f, "let"),
            FCall::Lambda => write!(f, "let"),
        }
    }
}

impl<C: fmt::Display, T: fmt::Display> fmt::Display for CallStack<C, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn print_at<C: fmt::Display, T: fmt::Display>(
            f: &mut fmt::Formatter,
            elem: &(C, T),
            depth: usize,
        ) -> fmt::Result {
            let start = if depth == 0 {
                String::from(" ")
            } else {
                let mut start = std::iter::repeat(' ').take(depth).collect::<String>();
                start.push_str("| ");
                start
            };

            write!(f, "{}| {} {} {}", start, elem.0, "-->".green(), elem.1)
        }

        self.inner
            .iter()
            .enumerate()
            .try_for_each(|(depth, elem)| print_at(f, elem, depth))
    }
}
