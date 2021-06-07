use super::*;
use lumina_tokenizer::{FileTokenizer, Span};

mod walker;

mod bounds;
pub use bounds::{Bounds, GenBound};

mod construct;

mod entity;
pub use entity::{
    first, r#if, r#patterns,
    r#patterns::{Pattern, PatternTable},
    Entity, Inlinable,
};

pub mod function;
pub use function::{Function, WhereBinding, WhereKind};

/// Builder attaches a tokenizer to a module, and handles our AST construction from the tokenizers
/// token stream. The generated AST is then streamed into `module.handler`
pub struct Builder<'src, 'a, 'h, H, E> {
    module: &'a mut FileParser<'h, H, E>,
    tokenizer: FileTokenizer<'src>,

    file: &'src str,

    indentation_spaces: usize,
}

impl<'src, 'a, 'h, H, E> Builder<'src, 'a, 'h, H, E> {
    /// This is only really used for testing.
    ///
    /// The common way of constructing a Builder is by doing it through a FileParsers `.parse`
    /// method.
    pub fn from_str(module: &'a mut FileParser<'h, H, E>, s: &'src str) -> Self {
        Self {
            module,
            tokenizer: FileTokenizer::new(s),

            file: s,

            indentation_spaces: 0,
        }
    }

    fn take_span(&self, span: std::ops::Range<usize>) -> &'src str {
        &self.file[span]
    }
}

impl<'src, 'a, 'h, H: Handler<E>, E> Builder<'src, 'a, 'h, H, E> {
    /// Start the `source code -> token -> AST -> handler` stream
    pub fn run(self) -> Result<(), HandlerError<E>> {
        self.headers(Vec::new())
    }
}
