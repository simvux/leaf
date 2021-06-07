use super::*;
use logos::{Lexer, SpannedIter};

pub struct FileTokenizer<'src> {
    lexer: SpannedIter<'src, Token>,
    originating_buffer: Option<Tr<Token>>,
}

impl<'src> FileTokenizer<'src> {
    pub fn new(code: &'src str) -> Self {
        Self {
            lexer: Lexer::new(code).spanned(),
            originating_buffer: None,
        }
    }
}

impl<'src> Tokenizer for FileTokenizer<'src> {
    fn view(&mut self) -> View<'_, Token> {
        match self.originating_buffer.take() {
            Some(t) => View {
                value: t.inner,
                span: t.span,
                originating_buffer: &mut self.originating_buffer,
            },
            None => {
                let (value, span) = self.lexer.next().unwrap_or((Token::EOF, 0..0));
                View {
                    value,
                    span,
                    originating_buffer: &mut self.originating_buffer,
                }
            }
        }
    }

    fn raw_buffer_access(&mut self) -> &mut Option<Tr<Token>> {
        &mut self.originating_buffer
    }
}
