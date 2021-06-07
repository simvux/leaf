use colored::Colorize;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    name: &'static str,
    file: String,
    line: Option<ErrorLine>,
    text: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorLine {
    code: String,
    linen: usize,
    arrow_span: std::ops::Range<usize>,
    arrow_text: String,
}

pub trait ToError: Sized {
    fn name(&self) -> &'static str;
    fn span(&self) -> Option<std::ops::Range<usize>>;
    fn text(&self) -> String;

    fn into_lumina_error(self, src: &str, file: String) -> Error {
        let line = self.span().map(|span| ErrorLine::load(src, span));
        Error {
            line,
            name: ToError::name(&self),
            file,
            text: self.text(),
        }
    }
}

impl Error {
    pub fn new(name: &'static str, file: String, line: Option<ErrorLine>, text: String) -> Error {
        Error {
            name,
            file,
            line,
            text,
        }
    }

    pub fn without_line(name: &'static str, file: String, text: String) -> Error {
        Error {
            name,
            file,
            text,
            line: None,
        }
    }
}

impl ErrorLine {
    pub fn load(src: &str, span: std::ops::Range<usize>) -> ErrorLine {
        let line_start_offset = src[..span.start]
            .bytes()
            .rev()
            .position(|b| b == b'\n')
            .unwrap_or(span.start);

        let line_end_offset = if src.as_bytes().get(span.end).copied() == Some(b'\n') {
            0
        } else {
            src[span.end..]
                .bytes()
                .position(|b| b == b'\n')
                .unwrap_or(src.len() - span.end)
        };

        let code = &src[span.start - line_start_offset..span.end + line_end_offset];

        let linen = src
            .bytes()
            .enumerate()
            .take_while(|(i, _)| *i != span.start)
            .fold(1, |n, (_, b)| if b == b'\n' { n + 1 } else { n });

        ErrorLine {
            arrow_span: line_start_offset..(line_start_offset + span.end - span.start),
            arrow_text: String::new(),
            code: code.to_string(),
            linen,
        }
    }

    pub fn text(mut self, text: String) -> Self {
        self.arrow_text = text;
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.line.as_ref() {
            Some(line) => write!(
                f,
                "{}\n{}: {}\n{}\n {}",
                self.file.purple(),
                "error".red(),
                self.name,
                &line,
                &self.text
            ),
            None if self.file.is_empty() => {
                write!(f, "{}: {}\n {}", "error".red(), self.name, &self.text)
            }
            None => write!(
                f,
                "{}\n{}: {}: {}",
                self.file.purple(),
                "error".red(),
                self.name,
                &self.text
            ),
        }
    }
}

impl fmt::Display for ErrorLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let code = format!(" {}: {}", &self.linen.to_string().yellow(), &self.code);

        // `3` represents the padding to the left and right side of the line number, as well as the `:` following it
        let linen_offset = self.linen.to_string().len() + 3;

        let arrow = std::iter::repeat(' ')
            .take(self.arrow_span.start + linen_offset)
            .chain(std::iter::repeat('^').take(self.arrow_span.end - self.arrow_span.start))
            .collect::<String>()
            .red();

        write!(f, "{}\n{} {}", code, arrow, &self.arrow_text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_CODE: &str = "map #(add 4.0) [1, 2, 3]";
    const ARROW_POS: std::ops::Range<usize> = 10..13;
    const LINEN: usize = 4;
    const ARROW_TEXT: &str = "expected int here";

    fn test_errorline() -> ErrorLine {
        ErrorLine {
            code: String::from(TEST_CODE),
            linen: 4,
            arrow_span: ARROW_POS,
            arrow_text: String::from(ARROW_TEXT),
        }
    }

    #[test]
    fn errorline() {
        colored::control::set_override(false);

        let line = test_errorline().to_string();

        println!("{}", &line);

        let expected = format!(
            " {}: {}\n{}",
            LINEN,
            TEST_CODE,
            format!("              ^^^ {}", ARROW_TEXT)
        );

        assert_eq!(line, expected);
    }

    #[test]
    fn load() {
        let src = "first line\n second line\nthird line\nfourth line";
        let line = ErrorLine::load(src, 27..29);

        println!("{}", &line);

        assert_eq!(
            line,
            ErrorLine {
                code: String::from("third line"),
                arrow_span: 3..5,
                arrow_text: String::new(),
                linen: 3,
            }
        );
    }
}
