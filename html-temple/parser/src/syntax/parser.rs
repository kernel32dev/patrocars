use crate::syntax::{
    error::Error,
    lexer::{HtmlTerminator, Keyword, Lexer, Token},
    source::{HasSource, Source},
};
use std::{borrow::Cow, fmt::Debug};

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Ident(&'a str),
    Literal(Box<(&'a str, ExprLiteral<'a>)>),
    Include(Box<(&'a str, (&'a str, Cow<'a, str>), Option<Expr<'a>>)>),
    Html(Vec<ExprHtml<'a>>),
    Array(Box<(&'a str, Vec<ExprItem<'a>>, &'a str)>),
    Record(Box<(&'a str, Vec<(ExprKey<'a>, Expr<'a>)>, &'a str)>),
    Function(Box<(&'a str, Vec<Pat<'a>>, Option<Pat<'a>>, Expr<'a>)>),
    Assingment(Box<(Pat<'a>, &'a str, Expr<'a>)>),
    Prefix(Box<(&'a str, Expr<'a>)>),
    Postfix(Box<(Expr<'a>, &'a str)>),
    Member(Box<(Expr<'a>, &'a str)>),
    Infix(Box<(Expr<'a>, &'a str, Expr<'a>)>),
    Call(Box<(Expr<'a>, &'a str, Vec<ExprItem<'a>>, &'a str)>),
    Index(Box<(Expr<'a>, &'a str, Vec<ExprItem<'a>>, &'a str)>),
    Method(Box<(Expr<'a>, &'a str, &'a str, Vec<ExprItem<'a>>, &'a str)>),
    Ternary(Box<(Expr<'a>, Expr<'a>, Expr<'a>)>),
    Block(Box<(&'a str, Vec<Expr<'a>>, Option<Expr<'a>>, &'a str)>),
    If(Box<(&'a str, Expr<'a>, Expr<'a>, Option<Expr<'a>>)>),
    For(Box<(&'a str, Option<&'a str>, &'a str, Expr<'a>, Expr<'a>)>),
    RangeExclusive(Box<(Option<Expr<'a>>, &'a str, Option<Expr<'a>>)>),
    RangeInclusive(Box<(Option<Expr<'a>>, &'a str, Expr<'a>)>),
}
#[derive(Debug, Clone)]
pub struct ExprItem<'a> {
    pub rest: Option<&'a str>,
    pub expr: Expr<'a>,
}
#[derive(Debug, Clone)]
pub enum ExprKey<'a> {
    Ident(&'a str),
    Integer(&'a str, i64),
    String(&'a str, Cow<'a, str>),
    Expr(Expr<'a>),
    Rest(&'a str),
}
#[derive(Debug, Clone)]
pub enum ExprHtml<'a> {
    Html(&'a str),
    Expr(Expr<'a>),
}
#[derive(Debug, Clone)]
pub enum ExprLiteral<'a> {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Cow<'a, str>),
}
#[derive(Debug, Clone)]
pub enum Pat<'a> {
    Ident(&'a str),
    Array(&'a str, Vec<PatItem<'a>>, &'a str),
    Record(&'a str, Vec<(Cow<'a, str>, Pat<'a>)>, &'a str),
}
#[derive(Debug, Clone)]
pub enum PatItem<'a> {
    Pat(Pat<'a>),
    Rest {
        rest: &'a str,
        ident: Option<&'a str>,
    },
}

// after postfix then only postfix, infix or end
// after prefix then only valur or prefix

pub fn parse<'a>(code: &'a str) -> Result<Expr<'a>, Error<'a>> {
    let mut tokens = Lexer::new(code);
    let mut vec = Vec::new();
    loop {
        let (html, terminator) = tokens.next_html();
        if !html.is_empty() {
            vec.push(ExprHtml::Html(html));
        }
        match terminator {
            HtmlTerminator::EOF(_) => break,
            HtmlTerminator::Code(_) => {
                vec.push(ExprHtml::Expr(parse_expr(&mut tokens)?));
                let _ = tokens
                    .maybe_next_code(|x| Ok(matches!(x, Some(Token::Punct(";"))).then_some(())));
            }
            HtmlTerminator::Escape(escape) => {
                vec.push(ExprHtml::Html(&escape[..1]));
            }
            HtmlTerminator::End(end) => vec.push(ExprHtml::Html(end)),
        }
    }
    Ok(Expr::Html(vec))
}

fn parse_short_expr<'a>(tokens: &mut Lexer<'a>) -> Result<Expr<'a>, Error<'a>> {
    // parse prefix
    let mut vec_prefix = Vec::new();
    while let Some(prefix) = tokens.maybe_next_code(|x| {
        Ok(match x {
            Some(Token::Punct(op @ ("!" | "!!" | "-" | "#" | ".." | "..="))) => Some(op),
            _ => None,
        })
    })? {
        vec_prefix.push(prefix);
    }
    let mut value = if vec_prefix.last().is_some_and(|x| *x == "..")
        && !match tokens.clone().next_code() {
            Ok(Some(token)) => match token {
                Token::Keyword(kw) => match kw {
                    Keyword::Null(_) => true,
                    Keyword::True(_) => true,
                    Keyword::False(_) => true,
                    Keyword::Fn(_) => true,
                    Keyword::If(_) => true,
                    Keyword::Then(_) => false,
                    Keyword::Else(_) => false,
                    Keyword::For(_) => true,
                    Keyword::In(_) => false,
                    Keyword::Do(_) => false,
                    Keyword::Is(_) => false,
                    Keyword::Include(_) => true,
                },
                Token::Ident(_) => true,
                Token::Punct(punct) => matches!(punct, "(" | "[" | "{" | "{{"),
                Token::Literal(_) => true,
            },
            Ok(None) | Err(_) => false,
        } {
        let range = vec_prefix.pop().expect("last just returned some");
        Expr::RangeExclusive(Box::new((None, range, None)))
    } else {
        // parse value
        match tokens.next_code()?.ok_or(Error::found(None))? {
            Token::Ident(ident) => Expr::Ident(ident),
            Token::Literal(literal) => parse_literal(literal)?,
            Token::Keyword(Keyword::Null(kw)) => Expr::Literal(Box::new((kw, ExprLiteral::Null))),
            Token::Keyword(Keyword::True(kw)) => {
                Expr::Literal(Box::new((kw, ExprLiteral::Boolean(true))))
            }
            Token::Keyword(Keyword::False(kw)) => {
                Expr::Literal(Box::new((kw, ExprLiteral::Boolean(false))))
            }
            Token::Keyword(Keyword::Fn(kw)) => {
                let mut pats = Vec::new();
                let rest = loop {
                    match tokens.maybe_next_code(|x| {
                        Ok(match x {
                            Some(Token::Punct(punct @ ("=>" | "..."))) => Some(punct),
                            _ => None,
                        })
                    })? {
                        Some("=>") => {
                            break None;
                        }
                        Some("...") => {
                            let rest = parse_pat(parse_expr(tokens)?)?;
                            match tokens.next_code()? {
                                Some(Token::Punct("=>")) => {}
                                found => return Err(Error::found(found).expected(&["=>"])),
                            }
                            break Some(rest);
                        }
                        Some(_) => unreachable!(),
                        None => {
                            pats.push(parse_pat(parse_expr(tokens)?)?);
                            match tokens.next_code()? {
                                Some(Token::Punct(",")) => {}
                                Some(Token::Punct("=>")) => break None,
                                found => return Err(Error::found(found).expected(&[",", "=>"])),
                            }
                        }
                    }
                };
                match tokens.clone().next_code()? {
                    Some(Token::Punct("(" | "[" | "{" | "{{")) => {}
                    found => return Err(Error::found(found).expected(&["(", "[", "{", "{{"])),
                }
                let expr = parse_short_expr(tokens)?;
                Expr::Function(Box::new((kw, pats, rest, expr)))
            }
            Token::Keyword(Keyword::If(kw)) => {
                let condition = parse_expr(tokens)?;
                match tokens.next_code()? {
                    Some(Token::Keyword(Keyword::Then(_))) => {}
                    found => return Err(Error::found(found).expected(&["then"])),
                }
                let when_true = parse_short_expr(tokens)?;
                let when_false = if let Some(()) = tokens
                    .maybe_next_code(|x| {
                        Ok(matches!(x, Some(Token::Keyword(Keyword::Else(_)))).then_some(()))
                    })
                    .or_else(|err| match err {
                        Error::UnexpectedHtml(_) => Ok(None),
                        err => Err(err),
                    })? {
                    Some(parse_short_expr(tokens)?)
                } else {
                    None
                };
                Expr::If(Box::new((kw, condition, when_true, when_false)))
            }
            Token::Keyword(Keyword::For(kw)) => {
                let first = match tokens.next_code()? {
                    Some(Token::Ident(ident)) => ident,
                    found => return Err(Error::found(found).ident()),
                };
                let second = if let Some(()) = tokens.maybe_next_code(|x| {
                    Ok(matches!(x, Some(Token::Punct(","))).then_some(()))
                })? {
                    let second = match tokens.next_code()? {
                        Some(Token::Ident(ident)) => ident,
                        found => return Err(Error::found(found).expected(&[","])),
                    };
                    Some(second)
                } else {
                    None
                };
                let (index, item) = match second {
                    Some(second) => (Some(first), second),
                    None => (None, first),
                };
                match tokens.next_code()? {
                    Some(Token::Keyword(Keyword::In(_))) => {}
                    found => return Err(Error::found(found).expected(&["in"])),
                }
                let iterated = parse_expr(tokens)?;
                match tokens.next_code()? {
                    Some(Token::Keyword(Keyword::Do(_))) => {}
                    found => return Err(Error::found(found).expected(&["do"])),
                }
                let body = parse_short_expr(tokens)?;
                Expr::For(Box::new((kw, index, item, iterated, body)))
            }
            Token::Keyword(Keyword::Include(kw)) => {
                match tokens.next_code()? {
                    Some(Token::Punct("(")) => {}
                    found => return Err(Error::found(found).expected(&["("])),
                }
                let path = match tokens.next_code()? {
                    Some(Token::Literal(literal))
                        if literal.starts_with(|x| matches!(x, '"' | '\'' | '`')) =>
                    {
                        literal
                    }
                    found => return Err(Error::found(found).string()),
                };
                let parsed_path = parse_string(path)?;
                match tokens.next_code()? {
                    Some(Token::Punct(",")) => {
                        let variables = parse_expr(tokens)?;
                        match tokens.next_code()? {
                            Some(Token::Punct(")")) => {}
                            found => return Err(Error::found(found).expected(&[")"])),
                        }
                        Expr::Include(Box::new((kw, (path, parsed_path), Some(variables))))
                    }
                    Some(Token::Punct(")")) => {
                        Expr::Include(Box::new((kw, (path, parsed_path), None)))
                    }
                    found => return Err(Error::found(found).expected(&[",", ")"])),
                }
            }
            Token::Keyword(
                found @ (Keyword::Then(_)
                | Keyword::Else(_)
                | Keyword::In(_)
                | Keyword::Do(_)
                | Keyword::Is(_)),
            ) => return Err(Error::found(Some(Token::Keyword(found)))),
            Token::Punct(punct) => match punct {
                "(" => {
                    let (stmts, tail, end) = parse_block(tokens)?;
                    Expr::Block(Box::new((punct, stmts, tail, end)))
                }
                "[" => {
                    let (items, end) = parse_array(tokens)?;
                    Expr::Array(Box::new((punct, items, end)))
                }
                "{" => {
                    let (entries, end) = parse_record(tokens)?;
                    Expr::Record(Box::new((punct, entries, end)))
                }
                "{{" => Expr::Html(parse_html(tokens)?),
                found => return Err(Error::found(Some(Token::Punct(found)))),
            },
        }
    };
    // add prefix to value
    for prefix in vec_prefix.into_iter().rev() {
        value = match prefix {
            ".." => Expr::RangeExclusive(Box::new((None, prefix, Some(value)))),
            "..=" => Expr::RangeInclusive(Box::new((None, prefix, value))),
            _ => Expr::Prefix(Box::new((prefix, value))),
        };
    }
    // parse postfix
    while let Some(postfix) = tokens
        .maybe_next_code(|x| {
            Ok(match x {
                Some(Token::Punct(op @ ("???" | "(" | "[" | "."))) => Some(op),
                _ => None,
            })
        })
        .or_else(|err| match err {
            Error::UnexpectedHtml(_) => Ok(None),
            err => Err(err),
        })?
    {
        match postfix {
            "???" => {
                value = Expr::Postfix(Box::new((value, postfix)));
            }
            "(" => {
                let (args, end) = parse_arguments(tokens)?;
                if let Expr::Member(member) = value {
                    let (expr, member) = *member;
                    value = Expr::Method(Box::new((expr, member, postfix, args, end)));
                } else {
                    value = Expr::Call(Box::new((value, postfix, args, end)));
                }
            }
            "[" => {
                let (args, end) = parse_index(tokens)?;
                value = Expr::Index(Box::new((value, postfix, args, end)));
            }
            "." => match tokens.next_code()? {
                Some(Token::Ident(ident)) => {
                    value = Expr::Member(Box::new((value, ident)));
                }
                found => return Err(Error::found(found).ident()),
            },
            _ => unreachable!(),
        }
    }
    Ok(value)
}

fn parse_expr<'a>(tokens: &mut Lexer<'a>) -> Result<Expr<'a>, Error<'a>> {
    let mut components = Vec::new();
    let last = loop {
        let value = parse_short_expr(tokens)?;
        // parse infix
        let Some(infix) = tokens
            .maybe_next_code(|x| match x {
                Some(Token::Punct(op) | Token::Keyword(Keyword::In(op) | Keyword::Is(op)))
                    if precedence(op).is_some() =>
                {
                    Ok(Some(op))
                }
                _ => Ok(None),
            })
            .or_else(|err| match err {
                Error::UnexpectedHtml(_) => Ok(None),
                err => Err(err),
            })?
        else {
            break value;
        };
        if infix == ".." {
            // ".." is a special case, in order for ".." to be able to be parsed as a postfix operator, we need to allow the expression to end after it
            // so we peek at the next token, and if it is not a prefix or value, then we exit the function

            if !match tokens.clone().next_code() {
                Ok(Some(token)) => match token {
                    Token::Keyword(kw) => match kw {
                        Keyword::Null(_) => true,
                        Keyword::True(_) => true,
                        Keyword::False(_) => true,
                        Keyword::Fn(_) => true,
                        Keyword::If(_) => true,
                        Keyword::Then(_) => false,
                        Keyword::Else(_) => false,
                        Keyword::For(_) => true,
                        Keyword::In(_) => false,
                        Keyword::Do(_) => false,
                        Keyword::Is(_) => false,
                        Keyword::Include(_) => true,
                    },
                    Token::Ident(_) => true,
                    Token::Punct(punct) => matches!(
                        punct,
                        "!" | "!!" | "-" | "#" | ".." | "..=" | "(" | "[" | "{" | "{{"
                    ),
                    Token::Literal(_) => true,
                },
                Ok(None) | Err(_) => false,
            } {
                break Expr::RangeExclusive(Box::new((Some(value), infix, None)));
            }
        }
        components.push((value, infix));
    };
    resolve_precedence(&mut components, last)
}

fn resolve_precedence<'a>(
    comps: &mut [(Expr<'a>, &'a str)],
    last: Expr<'a>,
) -> Result<Expr<'a>, Error<'a>> {
    let Some((next_split, _)) = comps.iter().map(|x| precedence(x.1)).enumerate().min() else {
        return Ok(last);
    };
    let (left, rest) = comps.split_at_mut(next_split);
    let ((left_last, op), right) = rest.split_first_mut().unwrap();
    let left_last = std::mem::replace(left_last, Expr::Ident(""));
    let right_last = last;
    match *op {
        "?" => {
            let mut depth = 0usize;
            let Some(pos) = right.iter().position(|(_, op)| {
                match *op {
                    ":" if depth == 0 => return true,
                    "?" => {
                        depth += 1;
                    }
                    ":" => {
                        depth -= 1;
                    }
                    _ => {}
                }
                false
            }) else {
                return Err(Error::UnmatchedTernary { op: *op });
            };
            let condition = resolve_precedence(left, left_last)?;
            let (left, rest) = right.split_at_mut(pos);
            let ((left_last, op), right) = rest.split_first_mut().unwrap();
            assert_eq!(*op, ":");
            let left_last = std::mem::replace(left_last, Expr::Ident(""));
            let when_true = resolve_precedence(left, left_last)?;
            let when_false = resolve_precedence(right, right_last)?;
            return Ok(Expr::Ternary(Box::new((condition, when_true, when_false))));
        }
        ":" => return Err(Error::UnmatchedTernary { op: *op }),
        _ => {}
    }
    let left = resolve_precedence(left, left_last)?;
    let right = resolve_precedence(right, right_last)?;
    match *op {
        "=" | "+=" | "-=" | "*=" | "/=" | "\\=" | "%=" | "&=" | "?=" => {
            let left = parse_pat(left)?;
            Ok(Expr::Assingment(Box::new((left, *op, right))))
        }
        ".." => Ok(Expr::RangeExclusive(Box::new((
            Some(left),
            *op,
            Some(right),
        )))),
        "..=" => Ok(Expr::RangeInclusive(Box::new((Some(left), *op, right)))),
        _ => Ok(Expr::Infix(Box::new((left, *op, right)))),
    }
}

fn parse_literal<'a>(literal: &'a str) -> Result<Expr<'a>, Error<'a>> {
    let value = match literal.chars().next().expect("literals cannot be empty") {
        '\'' | '"' | '`' => ExprLiteral::String(parse_string(literal)?),
        '0'..='9' if literal.contains('.') || literal.contains('-') => {
            ExprLiteral::Float(literal.parse().expect("bad float literal"))
        }
        '0'..='9' => ExprLiteral::Integer(literal.parse().expect("bad integer literal")),
        _ => panic!("bad literal: {literal:#?}"),
    };
    Ok(Expr::Literal(Box::new((literal, value))))
}

fn parse_html<'a>(tokens: &mut Lexer<'a>) -> Result<Vec<ExprHtml<'a>>, Error<'a>> {
    let mut vec = Vec::new();
    loop {
        let (html, terminator) = tokens.next_html();
        if !html.is_empty() {
            vec.push(ExprHtml::Html(html));
        }
        match terminator {
            HtmlTerminator::EOF(_) => return Err(Error::found(None).expected(&["}}"])),
            HtmlTerminator::Code(_) => {
                vec.push(ExprHtml::Expr(parse_expr(tokens)?));
            }
            HtmlTerminator::Escape(escape) => {
                vec.push(ExprHtml::Html(&escape[..1]));
            }
            HtmlTerminator::End(_) => return Ok(vec),
        }
    }
}

fn parse_string<'a>(literal: &'a str) -> Result<Cow<'a, str>, Error<'a>> {
    // TODO! descape string
    Ok(Cow::Borrowed(&literal[1..literal.len() - 1]))
}

fn parse_arguments<'a>(tokens: &mut Lexer<'a>) -> Result<(Vec<ExprItem<'a>>, &'a str), Error<'a>> {
    if let Some(end) = tokens.expect_punct(")")? {
        return Ok((Vec::new(), end));
    }
    let mut args = Vec::new();
    loop {
        let rest = tokens.expect_punct("...")?;
        let expr = parse_expr(tokens)?;
        args.push(ExprItem { rest, expr });
        match tokens.next_code()? {
            Some(Token::Punct(end @ ")")) => return Ok((args, end)),
            Some(Token::Punct(",")) => continue,
            found => return Err(Error::found(found).expected(&[")", ","])),
        }
    }
}

fn parse_index<'a>(tokens: &mut Lexer<'a>) -> Result<(Vec<ExprItem<'a>>, &'a str), Error<'a>> {
    if let Some(end) = tokens.expect_punct("]")? {
        return Ok((Vec::new(), end));
    }
    let mut args = Vec::new();
    loop {
        let rest = tokens.expect_punct("...")?;
        let expr = parse_expr(tokens)?;
        args.push(ExprItem { rest, expr });
        match tokens.next_code()? {
            Some(Token::Punct(end @ "]")) => return Ok((args, end)),
            Some(Token::Punct(",")) => continue,
            found => return Err(Error::found(found).expected(&["]", ","])),
        }
    }
}

fn parse_block<'a>(
    tokens: &mut Lexer<'a>,
) -> Result<(Vec<Expr<'a>>, Option<Expr<'a>>, &'a str), Error<'a>> {
    let last = parse_expr(tokens)?;
    match tokens.next_code()? {
        Some(Token::Punct(end @ ")")) => return Ok((Vec::new(), Some(last), end)),
        Some(Token::Punct(";")) => {}
        found => {
            return Err(Error::found(found).expected(&[")", ";"]));
        }
    }
    let mut stmts = vec![last];
    loop {
        if let Some(end) = tokens.expect_punct(")")? {
            return Ok((stmts, None, end));
        }
        let last = parse_expr(tokens)?;
        match tokens.next_code()? {
            Some(Token::Punct(end @ ")")) => {
                return Ok((stmts, Some(last), end));
            }
            Some(Token::Punct(";")) => {
                stmts.push(last);
                continue;
            }
            found => {
                return Err(Error::found(found).expected(&[")", ";"]));
            }
        }
    }
}

fn parse_array<'a>(tokens: &mut Lexer<'a>) -> Result<(Vec<ExprItem<'a>>, &'a str), Error<'a>> {
    let mut items = Vec::new();
    loop {
        if let Some(end) = tokens.expect_punct("]")? {
            return Ok((items, end));
        }
        let rest = tokens.expect_punct("...")?;
        let expr = parse_expr(tokens)?;
        items.push(ExprItem { rest, expr });
        match tokens.next_code()? {
            Some(Token::Punct(end @ "]")) => return Ok((items, end)),
            Some(Token::Punct(",")) => {
                continue;
            }
            found => {
                return Err(Error::found(found).expected(&["]", ","]));
            }
        }
    }
}

fn parse_record<'a>(
    tokens: &mut Lexer<'a>,
) -> Result<(Vec<(ExprKey<'a>, Expr<'a>)>, &'a str), Error<'a>> {
    let mut entries = Vec::new();
    loop {
        match tokens.next_code()? {
            Some(Token::Punct(start @ "(")) => {
                let (stmts, tail, end) = parse_block(tokens)?;
                let key = Expr::Block(Box::new((start, stmts, tail, end)));
                match tokens.next_code()? {
                    Some(Token::Punct(":")) => {}
                    found => return Err(Error::found(found).expected(&[":"])),
                }
                let value = parse_expr(tokens)?;
                entries.push((ExprKey::Expr(key), value));
            }
            Some(Token::Punct(end @ "}")) => {
                return Ok((entries, end));
            }
            Some(Token::Punct(rest @ "...")) => {
                entries.push((ExprKey::Rest(rest), parse_expr(tokens)?));
            }
            Some(Token::Ident(key)) => {
                let expr = if tokens.expect_punct(":")?.is_some() {
                    parse_expr(tokens)?
                } else {
                    Expr::Ident(key)
                };
                entries.push((ExprKey::Ident(key), expr));
            }
            Some(Token::Literal(key)) if matches!(key.chars().next(), Some('"' | '\'' | '`')) => {
                let parsed_key = parse_string(key)?;
                let value = if tokens.expect_punct(":")?.is_some() {
                    parse_expr(tokens)?
                } else {
                    Expr::Literal(Box::new((key, ExprLiteral::String(parsed_key.clone()))))
                };
                entries.push((ExprKey::String(key, parsed_key), value));
            }
            Some(Token::Literal(key))
                if matches!(key.chars().next(), Some('0'..='9'))
                    && !key.contains('.')
                    && !key.contains('-') =>
            {
                let parsed_key = key.parse().expect("bad integer literal");
                let value = if tokens.expect_punct(":")?.is_some() {
                    parse_expr(tokens)?
                } else {
                    Expr::Literal(Box::new((key, ExprLiteral::Integer(parsed_key))))
                };
                entries.push((ExprKey::Integer(key, parsed_key), value));
            }
            found => {
                return Err(Error::found(found).expected(&["}", ","]).ident());
            }
        }
        match tokens.next_code()? {
            Some(Token::Punct(end @ "}")) => {
                return Ok((entries, end));
            }
            Some(Token::Punct(",")) => {
                continue;
            }
            found => {
                return Err(Error::found(found).expected(&["}", ","]));
            }
        }
    }
}

fn parse_pat<'a>(expr: Expr<'a>) -> Result<Pat<'a>, Error<'a>> {
    match expr {
        Expr::Ident(ident) => Ok(Pat::Ident(ident)),
        Expr::Array(array) => {
            let (start, array, end) = *array;
            let array = array
                .into_iter()
                .map(|x| match x {
                    ExprItem {
                        rest: Some(rest),
                        expr: Expr::Ident(ident),
                    } => Ok(PatItem::Rest {
                        rest,
                        ident: Some(ident),
                    }),
                    ExprItem {
                        rest: Some(_),
                        expr,
                    } => Err(Error::ExpectedPattern { expr }),
                    ExprItem { rest: None, expr } => Ok(PatItem::Pat(parse_pat(expr)?)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pat::Array(start, array, end))
        }
        Expr::Record(record) => {
            let (start, record, end) = *record;
            let record = record
                .into_iter()
                .map(|(key, value)| {
                    let key = match key {
                        ExprKey::Ident(key) | ExprKey::Integer(key, _) | ExprKey::Rest(key) => {
                            Cow::Borrowed(key)
                        }
                        ExprKey::String(_, key) => key,
                        ExprKey::Expr(expr) => return Err(Error::ExpectedPattern { expr }),
                    };
                    Ok((key, parse_pat(value)?))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Pat::Record(start, record, end))
        }
        _ => Err(Error::ExpectedPattern { expr }),
    }
}

impl<'a> Lexer<'a> {
    fn expect_punct(&mut self, expected_punct: &'static str) -> Result<Option<&'a str>, Error<'a>> {
        let mut cloned = self.clone();
        match cloned.next_code()? {
            Some(Token::Punct(next_punct)) if next_punct == expected_punct => {
                *self = cloned;
                Ok(Some(next_punct))
            }
            _ => Ok(None),
        }
    }
}

fn precedence(op: &str) -> Option<u8> {
    Some(match op {
        // assingment operators
        "=" | "+=" | "-=" | "*=" | "/=" | "\\=" | "%=" | "&=" | "?=" => 1,
        // ternary operator
        "?" | ":" => 2,
        // range operators
        ".." | "..=" => 3,
        // null coalescing and null verification (executes the right operand only if the left operand is not null)
        "??" | "!!" => 4,
        // boolean or
        "||" => 5,
        // boolean and
        "&&" => 6,
        // equality operators
        "==" | "!=" => 7,
        // equality operators
        "is" => 8,
        // presence operators (TODO! fix this precedence)
        "in" => 8,
        // comparison operators
        "<" | ">" | "<=" | ">=" => 9,
        // string concatenation
        "&" => 10,
        // addition, subtraction
        "+" | "-" => 11,
        // other operators (\ is integer division, / is float division)
        "*" | "/" | "\\" | "%" => 12,
        _ => return None,
    })
}

impl HasSource for Expr<'_> {
    fn source(&self) -> Source {
        match self {
            Self::Ident(ident) => ident.source(),
            Self::Literal(literal) => literal.0.source(),
            Self::Include(include) => include.source(),
            Self::Html(html) => html.source(),
            Self::Array(array) => array.source(),
            Self::Record(record) => record.source(),
            Self::Function(function) => function.source(),
            Self::Assingment(assingment) => assingment.source(),
            Self::Prefix(prefix) => prefix.source(),
            Self::Postfix(postfix) => postfix.source(),
            Self::Member(member) => member.source(),
            Self::Infix(infix) => infix.source(),
            Self::Call(call) => call.source(),
            Self::Index(index) => index.source(),
            Self::Method(method) => method.source(),
            Self::Ternary(ternary) => ternary.source(),
            Self::Block(block) => block.source(),
            Self::If(if_expr) => if_expr.source(),
            Self::For(for_expr) => for_expr.source(),
            Self::RangeExclusive(range) => range.source(),
            Self::RangeInclusive(range) => range.source(),
        }
    }
}
impl HasSource for ExprItem<'_> {
    fn source(&self) -> Source {
        (&self.rest, &self.expr).source()
    }
}
impl HasSource for ExprKey<'_> {
    fn source(&self) -> Source {
        match self {
            Self::Ident(src) | Self::Integer(src, _) | Self::String(src, _) => src.source(),
            Self::Expr(expr) => expr.source(),
            Self::Rest(rest) => rest.source(),
        }
    }
}
impl HasSource for ExprHtml<'_> {
    fn source(&self) -> Source {
        match self {
            Self::Html(html) => html.source(),
            Self::Expr(expr) => expr.source(),
        }
    }
}
impl HasSource for Pat<'_> {
    fn source(&self) -> Source {
        match self {
            Self::Ident(ident) => ident.source(),
            Self::Array(start, _, end) => (start, end).source(),
            Self::Record(start, _, end) => (start, end).source(),
        }
    }
}
impl HasSource for PatItem<'_> {
    fn source(&self) -> Source {
        match self {
            Self::Pat(pat) => pat.source(),
            Self::Rest { rest, ident } => (rest, ident).source(),
        }
    }
}
