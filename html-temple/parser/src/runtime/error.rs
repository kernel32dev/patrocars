use std::borrow::Cow;

use crate::{syntax::source::Source, PathedIoError};

use super::value::Value;

#[derive(Debug)]
pub enum Error<'a> {
    Runtime(Vec<(Cow<'static, str>, Source, Option<Value<'a>>)>),
    Io(PathedIoError),
}

// constructors
impl<'a> Error<'a> {
    fn new(message: impl Into<Cow<'static, str>>) -> Self {
        let mut info = Vec::with_capacity(64);
        info.push((message.into(), Source::nowhere(), None));
        Self::Runtime(info)
    }
    pub(super) fn undefined() -> Self {
        Self::new("a variavel não foi encontrada")
    }
    pub(super) fn expected_array() -> Self {
        Self::new("o valor não é um array")
    }
    pub(super) fn expected_record() -> Self {
        Self::new("o valor não é um record")
    }
    pub(super) fn expected_range_integer() -> Self {
        Self::new("o valor não é um integer, ranges precisam de integers")
    }
    pub(super) fn expected_number() -> Self {
        Self::new("o valor não é um number")
    }
    pub(super) fn undefined_member() -> Self {
        Self::new("esse membro do record não existe")
    }
    pub(super) fn expected_indexable() -> Self {
        Self::new("esperado algo indexável, ou seja string, array ou record")
    }
    pub(super) fn expected_function() -> Self {
        Self::new("esperado uma função")
    }
    pub(super) fn expected_iterable() -> Self {
        Self::new("esperado algo iterável, ou seja null, array, record e range")
    }
    pub(super) fn cannot_stringify_html() -> Self {
        Self::new("não é possível converter html em uma string")
    }
    pub(super) fn expected_measureable() -> Self {
        Self::new("esperado algo medível, ou seja, null, string, array, record e range")
    }
    pub(super) fn cannot_compare() -> Self {
        Self::new("esses valores não podem ser comparados")
    }
    pub(super) fn cannot_add() -> Self {
        Self::new("esses valores não podem ser somados")
    }
    pub(super) fn cannot_subtract() -> Self {
        Self::new("esses valores não podem ser subtraidos")
    }
    pub(super) fn cannot_multiply() -> Self {
        Self::new("esses valores não podem ser multiplicados")
    }
    pub(super) fn cannot_divide() -> Self {
        Self::new("esses valores não podem ser dividos")
    }
    pub(super) fn cannot_remain() -> Self {
        Self::new("esses valores não podem ser módulados")
    }
    pub(super) fn cannot_repeat_array_negative() -> Self {
        Self::new("não é possível repetir um array um número negativo de vezes")
    }
    pub(super) fn cannot_repeat_string_negative() -> Self {
        Self::new("não é possível repetir um array um número negativo de vezes")
    }
    pub(super) fn out_of_bounds() -> Self {
        Self::new("índice inválido")
    }
    pub(super) fn method_does_not_exist(method: &'a str) -> Self {
        Self::new(format!("o método {method} não existe neste valor"))
    }
}

// annotations
impl<'a> Error<'a> {
    pub(super) fn because(mut self, message: impl Into<Cow<'static, str>>) -> Self {
        match &mut self {
            Error::Runtime(info) => info.push((message.into(), Source::nowhere(), None)),
            Error::Io(_) => {}
        }
        self
    }
    pub(super) fn source(mut self, source: impl Into<Source>) -> Self {
        match &mut self {
            Error::Runtime(info) => {
                if let Some(last) = info.last_mut() {
                    last.1 = source.into();
                }
            }
            Error::Io(_) => {}
        }
        self
    }
    pub(super) fn value(mut self, value: Value<'a>) -> Self {
        match &mut self {
            Error::Runtime(info) => {
                if let Some(last) = info.last_mut() {
                    last.2 = Some(value);
                }
            }
            Error::Io(_) => {}
        }
        self
    }
    pub(super) fn maybe(self, condition: bool, f: impl FnOnce(Self) -> Self) -> Self {
        if condition {
            f(self)
        } else {
            self
        }
    }

    pub(super) fn because_type(mut self, value: Value<'a>) -> Self {
        let message = match value {
            Value::Null => "o valor é um null",
            Value::Boolean(_) => "o valor é um boolean",
            Value::Integer(_) => "o valor é um integer",
            Value::Float(_) => "o valor é um float",
            Value::String(_) => "o valor é um string",
            Value::Html(_) => "o valor é um html",
            Value::Array(_) => "o valor é um array",
            Value::Record(_) => "o valor é um record",
            Value::RangeExclusiveOpen(_)
            | Value::RangeExclusiveClosed(_, _)
            | Value::RangeInclusive(_, _) => "o valor é um range",
            Value::Function(_) => "o valor é uma função",
        };
        match &mut self {
            Error::Runtime(info) => info.push((message.into(), Source::nowhere(), Some(value))),
            Error::Io(_) => {}
        }
        self
    }
    pub(super) fn because_left_type(mut self, value: Value<'a>) -> Self {
        let message = match value {
            Value::Null => "o valor a esquerda é um null",
            Value::Boolean(_) => "o valor a esquerda é um boolean",
            Value::Integer(_) => "o valor a esquerda é um integer",
            Value::Float(_) => "o valor a esquerda é um float",
            Value::String(_) => "o valor a esquerda é um string",
            Value::Html(_) => "o valor a esquerda é um html",
            Value::Array(_) => "o valor a esquerda é um array",
            Value::Record(_) => "o valor a esquerda é um record",
            Value::RangeExclusiveOpen(_)
            | Value::RangeExclusiveClosed(_, _)
            | Value::RangeInclusive(_, _) => "o valor a esquerda é um range",
            Value::Function(_) => "o valor a esquerda é uma função",
        };
        match &mut self {
            Error::Runtime(info) => info.push((message.into(), Source::nowhere(), Some(value))),
            Error::Io(_) => {}
        }
        self
    }
    pub(super) fn because_right_type(mut self, value: Value<'a>) -> Self {
        let message = match value {
            Value::Null => "o valor a direita é um null",
            Value::Boolean(_) => "o valor a direita é um boolean",
            Value::Integer(_) => "o valor a direita é um integer",
            Value::Float(_) => "o valor a direita é um float",
            Value::String(_) => "o valor a direita é um string",
            Value::Html(_) => "o valor a direita é um html",
            Value::Array(_) => "o valor a direita é um array",
            Value::Record(_) => "o valor a direita é um record",
            Value::RangeExclusiveOpen(_)
            | Value::RangeExclusiveClosed(_, _)
            | Value::RangeInclusive(_, _) => "o valor a direita é um range",
            Value::Function(_) => "o valor a direita é uma função",
        };
        match &mut self {
            Error::Runtime(info) => info.push((message.into(), Source::nowhere(), Some(value))),
            Error::Io(_) => {}
        }
        self
    }
}
