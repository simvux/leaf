pub use super::*;
use colored::Colorize;
use lumina_tokenizer::{Token, Tokenizer, View};
use lumina_util::Tr;
use std::borrow::Borrow;

mod attr;
mod r#enum;
mod expr;
mod function;
mod r#impl;
mod pattern;
pub use pattern::Settings as PatternSettings;
mod r#struct;
mod r#trait;
mod r#type;
pub use r#type::TypeSettings;
mod r#use;

impl<'src, 'a, 'h, E, H: Handler<E>> Builder<'src, 'a, 'h, H, E> {
    /// Scan for and expected the next output to be a header
    pub fn headers(mut self, attributes: Vec<Attr>) -> Result<(), HandlerError<E>> {
        let v = self.progress()?;
        match &v.inner {
            Token::HeaderUse => self.r#use(),
            Token::HeaderEnum => self.r#enum(attributes),
            Token::HeaderStruct => self.r#struct(attributes),
            Token::HeaderTrait => self.r#trait(),
            Token::HeaderFn => self.function(attributes, v.span),
            Token::HeaderImpl => self.implementation(v.span),
            Token::Attributes => {
                let attributes = self.attributes()?;
                self.headers(attributes)
            }
            Token::EOF => Ok(()),
            _ => Err(self
                .err_expected(
                    format!("a header like {} or {}", "fn".purple(), "struct".purple()),
                    v,
                )
                .into()),
        }
    }
}

impl<'src, 'a, 'h, E, H> Builder<'src, 'a, 'h, H, E> {
    /// Works like self.tokenizer.view(), except that it uses Token::Spaces and Token::NewLines to
    /// determine indentation level rather than returning them.
    pub fn view(&mut self) -> Result<View<'_, Token>, Error> {
        let view = self.tokenizer.view();

        match view.borrow() {
            Token::NewLines => {
                self.check_indentation_after_newline()?;
                self.view()
            }
            Token::Spaces => self.view(),
            Token::LineComment => self.view(),
            Token::Error => Err(Error::TokenError.idx(view.span.clone())),
            _ => {
                let span = view.span.clone();
                let value = view.value;
                let originating_buffer = self.tokenizer.raw_buffer_access();
                Ok(View {
                    originating_buffer,
                    value,
                    span,
                })
            }
        }
    }
    /// Works like self.tokenizer.progress(), except that it uses Token::Spaces and Token::NewLines to
    /// determine indentation level rather than returning them.
    pub fn progress(&mut self) -> Result<Tr<Token>, Error> {
        let t = self.tokenizer.progress();

        match &t.inner {
            Token::NewLines => {
                self.check_indentation_after_newline()?;
                self.progress()
            }
            Token::Spaces => self.progress(),
            Token::LineComment => self.progress(),
            Token::Error => Err(Error::TokenError.idx(t.span)),
            _ => Ok(t),
        }
    }
    /// like `view` but retains `Spaces`, `LineComment` and `NewLines` tokens
    ///
    /// NOTE: This puts the responsibility to update `self.indentation_spaces` onto the callé
    /// see `check_indentation_after_newline`
    pub fn view_spooky(&mut self) -> Result<View<'_, Token>, Error> {
        let view = self.tokenizer.view();
        match view.borrow() {
            Token::Error => Err(Error::TokenError.idx(view.span)),
            _ => Ok(view),
        }
    }

    pub fn skip_spaces(&mut self) -> Result<(), Error> {
        let view = self.view_spooky()?;
        if let Token::Spaces = view.borrow() {
            self.skip_spaces()
        } else {
            view.undo();
            Ok(())
        }
    }

    /// Updates the self.indentation_spaces using the following spaces
    pub fn check_indentation_after_newline(&mut self) -> Result<(), Error> {
        self.indentation_spaces = 0;
        let after = self.tokenizer.view();

        match after.borrow() {
            Token::Spaces => {
                self.indentation_spaces = after.span.len();
                Ok(())
            }
            Token::NewLines => self.check_indentation_after_newline(),
            Token::Error => Err(Error::TokenError.idx(after.span)),
            _ => {
                after.undo();
                Ok(())
            }
        }
    }

    pub fn err_expected<S: Into<String>>(&self, exp: S, got: Tr<Token>) -> Error {
        let actual = self.take_span(got.span.clone());

        let gotmsg = if got.inner.is_showable() {
            format!("the {} {}", got.inner.describe(), actual.purple())
        } else {
            let msg = got.inner.describe();
            format!("{} {}", msg, msg.purple())
        };

        Error::ExpectedButGot(exp.into(), gotmsg).idx(got.span)
    }

    pub fn err_unexpected(&self, got: Tr<Token>) -> Error {
        Error::Unexpected(got.inner.describe().to_string()).idx(got.span)
    }
}
