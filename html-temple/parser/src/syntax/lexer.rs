use crate::syntax::{
    error::Error,
    source::{HasSource, Source},
};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    code: &'a str,
    cursor: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Token<'a> {
    Keyword(Keyword<'a>),
    Ident(&'a str),
    Punct(&'a str),
    Literal(&'a str),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Keyword<'a> {
    Null(&'a str),
    True(&'a str),
    False(&'a str),
    Fn(&'a str),
    If(&'a str),
    Then(&'a str),
    Else(&'a str),
    For(&'a str),
    In(&'a str),
    Do(&'a str),
    Is(&'a str),
    Include(&'a str),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum HtmlTerminator<'a> {
    EOF(&'a str),
    /// `@`
    Code(&'a str),
    /// `@@`
    Escape(&'a str),
    /// `}}`
    End(&'a str),
}

impl<'a> Token<'a> {
    pub fn as_str(self) -> &'a str {
        match self {
            Token::Keyword(keyword) => keyword.as_str(),
            Token::Ident(x) | Token::Punct(x) | Token::Literal(x) => x,
        }
    }
}
impl<'a> Keyword<'a> {
    pub fn as_str(self) -> &'a str {
        match self {
            Keyword::Null(x)
            | Keyword::True(x)
            | Keyword::False(x)
            | Keyword::Fn(x)
            | Keyword::If(x)
            | Keyword::Then(x)
            | Keyword::Else(x)
            | Keyword::For(x)
            | Keyword::In(x)
            | Keyword::Do(x)
            | Keyword::Is(x)
            | Keyword::Include(x) => x,
        }
    }
}
impl<'a> HtmlTerminator<'a> {
    pub fn as_str(self) -> &'a str {
        match self {
            Self::EOF(x) | Self::Code(x) | Self::Escape(x) | Self::End(x) => x,
        }
    }
}

impl HasSource for Token<'_> {
    fn source(&self) -> Source {
        self.as_str().source()
    }
}
impl HasSource for Keyword<'_> {
    fn source(&self) -> Source {
        self.as_str().source()
    }
}
impl HasSource for HtmlTerminator<'_> {
    fn source(&self) -> Source {
        self.as_str().source()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self { code, cursor: 0 }
    }
    fn consume(&mut self, len: Option<usize>) -> &'a str {
        match len {
            Some(len) => {
                let code = &self.code[self.cursor..self.cursor + len];
                self.cursor += len;
                code
            }
            None => {
                let code = &self.code[self.cursor..];
                self.cursor = self.code.len();
                code
            }
        }
    }
    /*fn consume_whitespace(&mut self) -> Result<(), Error<'a>> {
        loop {
            self.cursor = self.code.len() - self.code[self.cursor..].trim_start().len();
            let bytes = &self.code.as_bytes()[self.cursor..];
            if bytes.starts_with(b"/*") {
                let mut bytes = &bytes[2..];
                let mut real_len = 2;
                loop {
                    let Some(len) = bytes.iter().position(|x| *x == b'*') else {
                        let error = &self.code[self.cursor..self.cursor + 2];
                        self.cursor += 2;
                        return Err(Error::UnclosedMultilineComment(error));
                    };
                    if bytes.get(len + 1).copied() == Some(b'/') {
                        self.cursor += real_len + len + 2;
                        break;
                    } else {
                        bytes = &bytes[len + 1..];
                        real_len += len + 1;
                    }
                }
            } else if bytes.starts_with(b"//") {
                self.cursor += bytes
                    .iter()
                    .position(|x| matches!(x, b'\n' | b'\r'))
                    .unwrap_or(bytes.len());
            } else {
                break Ok(());
            }
        }
    }
    */
*/
    /// parses the remaining as html and returns the html, but only up to a terminator, and what terminated the chunk of html
    pub fn next_html(&mut self) -> (&'a str, HtmlTerminator<'a>) {
        let remaining = &self.code[self.cursor..];
        let code_terminator = remaining.find('@');
        let end_terminator = remaining.find("}}");
        match (code_terminator, end_terminator) {
            (None, None) => (
                self.consume(None),
                HtmlTerminator::EOF(&self.code[self.code.len()..]),
            ),
            (Some(code_terminator), None) => {
                let html = self.consume(Some(code_terminator));
                let terminator = if remaining[code_terminator..].starts_with("@@") {
                    HtmlTerminator::Escape(self.consume(Some(2)))
                } else {
                    HtmlTerminator::Code(self.consume(Some(1)))
                };
                (html, terminator)
            }
            (None, Some(end_terminator)) => (
                self.consume(Some(end_terminator)),
                HtmlTerminator::End(self.consume(Some(2))),
            ),
            (Some(code_terminator), Some(end_terminator)) => {
                debug_assert_ne!(code_terminator, end_terminator);
                if code_terminator < end_terminator {
                    let html = self.consume(Some(code_terminator));
                    let terminator = if remaining[code_terminator..].starts_with("@@") {
                        HtmlTerminator::Escape(self.consume(Some(2)))
                    } else {
                        HtmlTerminator::Code(self.consume(Some(1)))
                    };
                    (html, terminator)
                } else {
                    (
                        self.consume(Some(end_terminator)),
                        HtmlTerminator::End(self.consume(Some(2))),
                    )
                }
            }
        }
    }
    /// parses the next Token as code
    pub fn next_code(&mut self) -> Result<Option<Token<'a>>, Error<'a>> {
        // self.consume_whitespace()?;
        self.cursor = self.code.len() - self.code[self.cursor..].trim_start().len();
        if self.cursor == self.code.len() {
            return Ok(None);
        }
        Ok(Some(self.next_code_some()?))
    }
    /// parses the next Token as code, assuming there is code left and assuming whitespace was already removed
    fn next_code_some(&mut self) -> Result<Token<'a>, Error<'a>> {
        let bytes = self.code[self.cursor..].as_bytes();
        let next1 = bytes[0];
        let eq = bytes.get(1) == Some(&b'=');
        let gt = bytes.get(1) == Some(&b'>');
        let dup = bytes.get(1) == Some(&next1);
        let punct_len = match next1 {
            b'\t' | b'\n' | b'\x0B' | b'\x0C' | b'\r' | b' ' => unreachable!(),
            b'\x00'..=b'\x1F' => return Err(Error::IllegalCharacter(self.consume(Some(1)))),
            b'!' if eq || dup => 2,
            b'!' => 1,
            b'"' => 0,
            b'#' => 1,
            b'$' => 1,
            b'%' if eq => 2,
            b'%' => 1,
            b'&' if eq || dup => 2,
            b'&' => 1,
            b'\'' => 0,
            b'(' => 1,
            b')' => 1,
            b'*' if eq => 2,
            b'*' => 1,
            b'+' if eq => 2,
            b'+' => 1,
            b',' => 1,
            b'-' if eq || gt => 2,
            b'-' => 1,
            b'.' if bytes.starts_with(b"..=") => 3,
            b'.' if bytes.starts_with(b"...") => 3,
            b'.' if bytes.starts_with(b"..") => 2,
            b'.' => 1,
            b'/' if bytes.get(1) == Some(&b'>') => 2,
            b'/' if eq => 2,
            b'/' => 1,
            b'0'..=b'9' => 0,
            b':' => 1,
            b';' => 1,
            b'<' if bytes.get(1) == Some(&b'/')
                && matches!(bytes.get(2), Some(b'A'..=b'Z' | b'a'..=b'z')) =>
            {
                return Err(Error::UnexpectedHtml(self.consume(None)));
            }
            b'<' if matches!(bytes.get(1), Some(b'A'..=b'Z' | b'a'..=b'z' | b'!')) => {
                return Err(Error::UnexpectedHtml(self.consume(None)));
            }
            b'<' if eq => 2,
            b'<' => 1,
            b'=' if eq || gt => 2,
            b'=' => 1,
            b'>' if eq => 2,
            b'>' => 1,
            b'?' if bytes.starts_with(b"???") => 3,
            b'?' if eq || dup => 2,
            b'?' => 1,
            b'@' => 1,
            b'A'..=b'Z' => 0,
            b'[' => 1,
            b'\\' => 1,
            b']' => 1,
            b'^' => 1,
            b'_' => 0,
            b'`' => 0,
            b'a'..=b'z' => 0,
            b'{' if dup => 2,
            b'{' => 1,
            b'|' if dup => 2,
            b'|' => 1,
            b'}' => 1,
            b'~' => 1,
            b'\x7F' => return Err(Error::IllegalCharacter(self.consume(Some(1)))),
            0x80.. => {
                let len = match () {
                    () if next1 & 0b11111000 == 0b11110000 => 4,
                    () if next1 & 0b11110000 == 0b11100000 => 3,
                    () if next1 & 0b11100000 == 0b11000000 => 2,
                    () => unreachable!("bad unicode"),
                };
                return Err(Error::UnicodeCharacter(self.consume(Some(len))));
            }
        };
        if punct_len != 0 {
            return Ok(Token::Punct(self.consume(Some(punct_len))));
        }
        match next1 {
            b'A'..=b'Z' | b'_' | b'a'..=b'z' => {
                let len = bytes
                    .iter()
                    .position(|x| !x.is_ascii_alphanumeric() && *x != b'_');
                let ident = self.consume(len);
                match ident {
                    "null" => Ok(Token::Keyword(Keyword::Null(ident))),
                    "true" => Ok(Token::Keyword(Keyword::True(ident))),
                    "false" => Ok(Token::Keyword(Keyword::False(ident))),
                    "fn" => Ok(Token::Keyword(Keyword::Fn(ident))),
                    "if" => Ok(Token::Keyword(Keyword::If(ident))),
                    "then" => Ok(Token::Keyword(Keyword::Then(ident))),
                    "else" => Ok(Token::Keyword(Keyword::Else(ident))),
                    "for" => Ok(Token::Keyword(Keyword::For(ident))),
                    "in" => Ok(Token::Keyword(Keyword::In(ident))),
                    "do" => Ok(Token::Keyword(Keyword::Do(ident))),
                    "is" => Ok(Token::Keyword(Keyword::Is(ident))),
                    "include" => Ok(Token::Keyword(Keyword::Include(ident))),
                    ident => Ok(Token::Ident(ident)),
                }
            }
            b'"' | b'\'' | b'`' => {
                let terminator = next1;
                match bytes[1..].iter().position(|x| *x == terminator) {
                    Some(len) => Ok(Token::Literal(self.consume(Some(len + 2)))),
                    None => Err(Error::UnclosedStringLiteral(&self.consume(None)[0..1])),
                }
            }
            b'0'..=b'9' => {
                let mut seen_dot = false;
                let mut len = bytes
                    .iter()
                    .position(|x| !match x {
                        b'0'..=b'9' => true,
                        b'.' if !seen_dot => {
                            seen_dot = true;
                            true
                        }
                        _ => false,
                    })
                    .unwrap_or(bytes.len());
                if bytes[len - 1] == b'.' {
                    len -= 1;
                }
                Ok(Token::Literal(self.consume(Some(len))))
            }
            _ => unreachable!(),
        }
    }

    pub fn maybe_next_code<T>(
        &mut self,
        f: impl FnOnce(Option<Token<'a>>) -> Result<Option<T>, Error<'a>>,
    ) -> Result<Option<T>, Error<'a>> {
        let mut clone = self.clone();
        let option = f(clone.next_code()?)?;
        if option.is_some() {
            *self = clone;
        }
        Ok(option)
    }
}
