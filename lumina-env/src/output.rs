use std::default::Default;

#[derive(Debug)]
pub struct Output {
    pub ast: bool,
    pub hir: bool,
    pub mir: bool,

    pub run: bool,
    pub help: bool,
}

#[cfg(debug_assertions)]
impl Default for Output {
    fn default() -> Self {
        Output {
            ast: false,
            hir: false,
            mir: false,
            run: true,
            help: false,
        }
    }
}

#[cfg(not(debug_assertions))]
impl Default for Output {
    fn default() -> Self {
        Output {
            ast: true,
            hir: true,
            mir: true,
            run: true,
            help: false,
        }
    }
}
