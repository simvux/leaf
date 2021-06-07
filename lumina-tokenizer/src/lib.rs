use lumina_util::Tr;
pub use std::convert::TryFrom;
use std::ops::Range;

mod token;
pub use token::Token;

pub type Span = Range<usize>;

mod file;
pub use file::FileTokenizer;

/// Tokenizer is anything that can generate us tokens and also expose them under a `View`
pub trait Tokenizer {
    fn view(&mut self) -> View<'_, Token>;
    fn raw_buffer_access(&mut self) -> &'_ mut Option<Tr<Token>>;

    fn progress(&mut self) -> Tr<Token> {
        self.view().consume()
    }

    /// Returns token as either expected or unexepcted
    fn expect(&mut self, exp: Token) -> Result<Tr<Token>, Tr<Token>> {
        let t = self.progress();
        if t.inner == exp {
            Ok(t)
        } else {
            Err(t)
        }
    }
}

/// View allows us to peek into the next token, while deciding wether to consume it or not later
///
/// If we choose not to consume it, we can return it with the `undo` method.
///
/// If we need to check what it is without consuming it to decide what to do then we can use the
/// `std::borrow::Borrow::borrow` method
pub struct View<'a, T> {
    pub originating_buffer: &'a mut Option<Tr<T>>,
    pub value: T,
    pub span: std::ops::Range<usize>,
}

impl<'a, T> View<'a, T> {
    /// Put this token back
    pub fn undo(self) {
        *self.originating_buffer = Some(Tr::tr(self.span.clone(), self.value));
    }
    /// Get the owned value of this token
    pub fn consume(self) -> Tr<T> {
        Tr::tr(self.span, self.value)
    }
}

impl<'a, T> std::borrow::Borrow<T> for View<'a, T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}

/// Incase we already have the tokens and just want a Tokenizer over them.
pub struct TokenBuffer {
    tokens: Vec<Tr<Token>>,
    originating_buffer: Option<Tr<Token>>,
}

impl From<Vec<Tr<Token>>> for TokenBuffer {
    fn from(tokens: Vec<Tr<Token>>) -> Self {
        Self {
            tokens,
            originating_buffer: None,
        }
    }
}

impl Tokenizer for TokenBuffer {
    fn view(&mut self) -> View<'_, Token> {
        let (value, span) = if let Some(v) = self.originating_buffer.take() {
            self.originating_buffer = None;
            v.sep()
        } else {
            self.tokens.pop().unwrap().sep()
        };
        View {
            value,
            span,
            originating_buffer: &mut self.originating_buffer,
        }
    }

    fn raw_buffer_access(&mut self) -> &'_ mut Option<Tr<Token>> {
        &mut self.originating_buffer
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    fn init(src: &'static str) -> FileTokenizer<'static> {
        FileTokenizer::new(src)
    }

    fn eq(left: Tr<Token>, span: Span, token: Token) {
        assert_eq!(left, Tr::tr(span, token));
    }

    fn eqs(src: &mut FileTokenizer<'static>, tests: &[(Span, Token)]) {
        tests
            .iter()
            .cloned()
            .for_each(|(span, token)| eq(src.progress(), span, token))
    }

    #[test]
    fn func_call() {
        let mut src = init("add 1 2");
        eqs(
            &mut src,
            &[
                (0..2, Identifier),
                (2..3, Spaces),
                (3..4, Int(1)),
                (4..5, Spaces),
                (5..6, Int(2)),
            ],
        );
    }

    #[test]
    fn parenthesis() {
        let mut src = init("(1 2)(1  )( 2)");
        eqs(
            &mut src,
            &[
                (0..1, OpenParen),
                (1..2, Int(1)),
                (2..3, Spaces),
                (3..4, Int(2)),
                (4..5, CloseParen),
                (5..6, OpenParen),
                (6..7, Int(1)),
                (7..9, Spaces),
                (9..10, CloseParen),
                (10..11, OpenParen),
                (11..12, Spaces),
                (12..13, Token::Int(2)),
                (13..14, Token::CloseParen),
            ],
        );
    }

    #[test]
    fn string_literal() {
        let mut src = init(r#""a () \" ""#);
        eqs(&mut src, &[(0..8, Token::StringLiteral)]);
    }

    #[test]
    fn numbers() {
        let mut src = init("4 4.4 172");
        eqs(
            &mut src,
            &[
                (0..1, Token::Int(4)),
                (1..2, Token::Spaces),
                (2..5, Token::Float(4.4)),
                (5..6, Token::Spaces),
                (6..10, Token::Int(172)),
            ],
        );
    }

    #[test]
    fn comments() {
        let mut src = init("1 // ignored\n2");
        eqs(
            // TODO:
            // these spans are wrong. yet, the tast passes. that's not supposed to happen?
            &mut src,
            &[
                (0..1, Token::Int(1)),
                (1..2, Token::Spaces),
                (2..13, Token::LineComment),
                (12..13, Token::NewLines),
                (18..19, Token::Int(2)),
            ],
        );
    }
}
