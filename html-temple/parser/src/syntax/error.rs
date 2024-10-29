use crate::syntax::{
    lexer::Token,
    parser::Expr,
    source::{HasSource, Source},
};
use std::fmt::{Debug, Display};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Error<'a> {
    // lexer error
    IllegalCharacter(&'a str),
    UnicodeCharacter(&'a str),
    UnclosedStringLiteral(&'a str),
    UnexpectedHtml(&'a str),

    // parser error
    Expected {
        found: Option<Token<'a>>,
        /// puncts and keywords
        expected: &'static [&'static str],
        ident_expected: bool,
        integer_expected: bool,
        string_expected: bool,
    },
    UnmatchedTernary {
        op: &'a str,
    },
    ExpectedPattern {
        expr: Expr<'a>,
    },
}

impl<'a> Error<'a> {
    pub(crate) fn found(found: Option<Token<'a>>) -> Self {
        Self::Expected {
            found,
            expected: &[],
            ident_expected: false,
            integer_expected: false,
            string_expected: false,
        }
    }
    pub(crate) fn expected(self, puncts_expected: &'static [&'static str]) -> Self {
        match self {
            Self::Expected {
                found,
                expected: _,
                ident_expected,
                integer_expected,
                string_expected,
            } => Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected,
                integer_expected,
                string_expected,
            },
            _ => self,
        }
    }
    pub(crate) fn ident(self) -> Self {
        match self {
            Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected: _,
                integer_expected,
                string_expected,
            } => Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected: true,
                integer_expected,
                string_expected,
            },
            _ => self,
        }
    }
    #[allow(dead_code)]
    pub(crate) fn integer(self) -> Self {
        match self {
            Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected,
                integer_expected: _,
                string_expected,
            } => Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected,
                integer_expected: true,
                string_expected,
            },
            _ => self,
        }
    }
    #[allow(dead_code)]
    pub(crate) fn string(self) -> Self {
        match self {
            Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected,
                integer_expected,
                string_expected: _,
            } => Self::Expected {
                found,
                expected: puncts_expected,
                ident_expected,
                integer_expected,
                string_expected: true,
            },
            _ => self,
        }
    }
}

impl HasSource for Error<'_> {
    fn source(&self) -> Source {
        match self {
            Error::IllegalCharacter(x) => x.source(),
            Error::UnicodeCharacter(x) => x.source(),
            Error::UnclosedStringLiteral(x) => x.source(),
            Error::UnexpectedHtml(x) => x.source(),
            Error::Expected {
                found: Some(found), ..
            } => found.source(),
            Error::Expected { found: None, .. } => Source::end(),
            Error::ExpectedPattern { expr } => expr.source(),
            Error::UnmatchedTernary { op } => op.source(),
        }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Error::IllegalCharacter(_) => "illegal character",
            Error::UnicodeCharacter(_) => "unicode character",
            Error::UnclosedStringLiteral(_) => "unclosed string literal",
            Error::UnexpectedHtml(_) => "unexpected html",
            Error::Expected {
                found: _,
                expected,
                ident_expected,
                integer_expected,
                string_expected,
            } => {
                let mut iter = expected
                    .iter()
                    .map(|x| (true, *x))
                    .chain(ident_expected.then_some((false, "an identifier")))
                    .chain(integer_expected.then_some((false, "an integer")))
                    .chain(string_expected.then_some((false, "a string")))
                    .enumerate()
                    .peekable();
                if iter.peek().is_none() {
                    "unexpected token"
                } else {
                    f.write_str("expected ")?;
                    while let Some((index, (quote, text))) = iter.next() {
                        if index != 0 {
                            if iter.peek().is_none() {
                                f.write_str(" or ")?;
                            } else {
                                f.write_str(", ")?;
                            }
                        }
                        if quote {
                            f.write_str("\"")?;
                        }
                        f.write_str(text)?;
                        if quote {
                            f.write_str("\"")?;
                        }
                    }
                    return Ok(());
                }
            }
            Error::ExpectedPattern { expr: _ } => "expected a pattern",
            Error::UnmatchedTernary { op: _ } => "unmatched ternary",
        };
        f.write_str(message)
    }
}
