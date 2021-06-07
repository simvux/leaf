use super::type_check;
use crate::collector;
use lumina_util::ToError;

#[derive(Debug, Clone)]
pub enum Error {
    Positioned {
        span: std::ops::Range<usize>,
        error: Box<Self>,
    },

    TypeError(super::type_check::TypeError),

    Todo(&'static str),
}

/*
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

impl<'ast> fmt::Display for ErrorCompiler<'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.error.fmt(f)
    }
}
*/

impl Error {
    pub fn create_lumina_error(
        self,
        collected: &collector::Storage,
        src: &str,
        file: String,
    ) -> lumina_util::Error {
        ErrorCompiler {
            collected,
            error: self,
        }
        .into_lumina_error(src, file)
    }

    pub fn position(self, span: std::ops::Range<usize>) -> Self {
        match self {
            already @ Error::Positioned { .. } | already @ Error::TypeError(_) => already,
            error => Error::Positioned {
                span,
                error: Box::new(error),
            },
        }
    }

    pub fn name(&self) -> &'static str {
        "placeholder name"
    }
}

pub struct ErrorCompiler<'ast> {
    collected: &'ast collector::Storage,
    error: Error,
}

impl<'ast> ToError for ErrorCompiler<'ast> {
    fn name(&self) -> &'static str {
        self.error.name()
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        match &self.error {
            Error::Positioned { span, .. } => Some(span.clone()),
            Error::TypeError(type_check::TypeError::NotImplemented(got, _)) => {
                Some(got.span.clone())
            }
            _ => None,
        }
    }

    // we have access to `collected` to decode identifiers. So; view this as a fancy fmt::Display
    fn text(&self) -> String {
        format!("{:#?}", self.error)
    }
}
