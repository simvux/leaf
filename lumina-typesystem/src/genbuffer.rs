use super::*;
use lumina_util::debug;

#[derive(Debug, Clone, Default)]
pub struct GenBuffer {
    assignments: HashMap<u8, Tr<Tp<Type>>>,
}

impl GenBuffer {
    pub fn new() -> Self {
        Self {
            assignments: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.assignments.clear();
    }

    pub fn assign(&mut self, gid: u8, t: Tr<Tp<Type>>) {
        if let Some(t) = self.assignments.insert(gid, t) {
            panic!(
                "double generic assignment. GenID {} was already assigned {}",
                gid + b'a',
                t
            );
        }
    }

    pub fn get_or_assign<T: Into<Tr<Tp<Type>>>>(&mut self, gid: u8, t: T) -> &Tr<Tp<Type>> {
        self.assignments.entry(gid).or_insert_with(|| t.into())
    }

    pub fn lookup(&self, gid: u8) -> Option<&Tr<Tp<Type>>> {
        self.assignments.get(&gid)
    }

    pub fn decode(&self, t: &mut Tr<Tp<Type>>) -> Result<(), Tr<Tp<Type>>> {
        match &mut t.root {
            Type::Generic(Generic::Bound(gid, _)) | Type::Generic(Generic::Unbound(gid)) => {
                match self.lookup(**gid) {
                    // None => return Err(t.clone()),
                    None => {
                        /*
                        debug!(
                            "genbuffer",
                            "warning: possibly unsafe return of generic from call"
                        );
                        */
                        return Err(t.clone());
                    }
                    Some(to_use) => *t = to_use.clone(),
                }
            }
            _ => {}
        }

        for t in t.params.iter_mut() {
            self.decode(t)?;
        }

        Ok(())
    }

    pub fn expect_or_assign(&mut self, gid: u8, t: Tr<&Tp<Type>>) -> Result<(), Tr<Tp<Type>>> {
        match self.lookup(gid) {
            Some(exp) if t == exp.as_ref() => Ok(()),
            Some(exp) => Err(exp.clone()),
            None => {
                self.assign(gid, t.cloned());
                Ok(())
            }
        }
    }
}
