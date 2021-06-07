use super::*;
use lumina_util::Location;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Positioned(std::ops::Range<usize>, Box<Self>),

    // Stacked(Vec<lumina_util::Error>), I think we used this before when we didn't have an
    // internal error buffer. But we don't need it anymore
    FunctionNotFound(Location),
    TraitNotFound(Location),
    InferenceFailure,
    InferenceForbidden,
    CannotTakeParameters,
    ReturnTypeMissmatch {
        exp: Tr<ITp<hir::IType>>,
        got: Tr<ITp<hir::IType>>,
    },
    GenericAssignedDifferently {
        gid: u8,
        expected: Tr<Tp<hir::Type>>,
        got: Tr<Tp<hir::Type>>,
    },

    Skipped,
}

impl Error {
    pub fn position(self, span: std::ops::Range<usize>) -> Self {
        match self {
            already @ Self::Positioned(_, _) => already,
            other => Self::Positioned(span, Box::new(other)),
        }
    }

    pub fn should_view(&self) -> bool {
        let skipped = *self == Error::Skipped
            || matches!(self, Error::Positioned(_, e) if **e == Error::Skipped);
        !skipped
    }
}

impl ToError for Error {
    fn name(&self) -> &'static str {
        "placeholder name"
    }

    fn span(&self) -> Option<std::ops::Range<usize>> {
        match self {
            Self::Positioned(span, _) => Some(span.clone()),
            _ => None,
        }
    }

    fn text(&self) -> String {
        format!("{:?}", self)
    }
}
