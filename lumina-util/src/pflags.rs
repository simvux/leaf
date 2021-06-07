use smallvec::SmallVec;
use std::collections::HashMap;

/// PFlags are used to explicitely tell the compiler to manage a parameter in some special way.
///
/// ```lumina
/// fn f p (int :: [mut] -> int)
/// ```
///
/// here the parameter `p` has the `mut` pflag attached to it
#[derive(Clone, PartialEq, Debug, Eq, Default)]
pub struct PFlags {
    params: HashMap<usize, FlagList>,
    returns: FlagList,
}

pub type FlagList = SmallVec<[ParameterFlag; 1]>;

impl PFlags {
    pub fn new() -> Self {
        Self {
            params: HashMap::new(),
            returns: FlagList::new(),
        }
    }

    pub fn insert(&mut self, pid: usize, flag: ParameterFlag) {
        self.entry(pid).push(flag);
    }

    pub fn entry(&mut self, pid: usize) -> &mut FlagList {
        self.params.entry(pid).or_insert_with(SmallVec::new)
    }

    pub fn entry_return(&mut self) -> &mut FlagList {
        &mut self.returns
    }

    pub fn insert_return(&mut self, flag: ParameterFlag) {
        self.returns.push(flag);
    }

    pub fn swap_param_with_return(&mut self, pid: usize) {
        if let Some(list) = self.params.get_mut(&pid) {
            std::mem::swap(list, &mut self.returns)
        }
    }
}

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum ParameterFlag {
    Mut,
    Debug,
}
