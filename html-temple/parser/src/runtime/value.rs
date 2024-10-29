use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{Debug, Display, Write},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{
    arena::{Arena, Liftable},
    syntax::parser::{Expr, Pat},
};

pub trait IntoValue {
    fn into_value<'a>(self, arena: &'a Arena) -> Result<Value<'a>, serde_json::Error>
    where
        Self: 'a;
}
impl IntoValue for Value<'_> {
    fn into_value<'a>(self, arena: &'a Arena) -> Result<Value<'a>, serde_json::Error>
    where
        Self: 'a,
    {
        Ok(self.deep_clone(arena))
    }
}
impl IntoValue for &Value<'_> {
    fn into_value<'a>(self, arena: &'a Arena) -> Result<Value<'a>, serde_json::Error>
    where
        Self: 'a,
    {
        Ok(self.deep_clone(arena))
    }
}
impl<T: serde::Serialize> IntoValue for T {
    fn into_value<'a>(self, arena: &'a Arena) -> Result<Value<'a>, serde_json::Error>
    where
        Self: 'a,
    {
        serde_json::to_value(self).map(|value| Value::from_json_value(value, arena))
    }
}

pub mod hlist {
    pub struct HListValues<T, R>(pub T, pub R);
    pub struct HListDone;
}

pub trait IntoValues {
    fn into_values<'a, E>(
        self,
        arena: &'a Arena,
        callback: impl for<'b> FnMut(&'b str, Result<Value<'a>, serde_json::Error>) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a;
}
impl IntoValues for hlist::HListDone {
    fn into_values<'a, E>(
        self,
        _: &'a Arena,
        _: impl for<'b> FnMut(&'b str, Result<Value<'a>, serde_json::Error>) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        Ok(())
    }
}
impl<K, V, T> IntoValues for hlist::HListValues<(K, V), T>
where
    K: AsRef<str>,
    V: IntoValue + 'static,
    T: IntoValues,
{
    fn into_values<'a, E>(
        self,
        arena: &'a Arena,
        mut callback: impl for<'b> FnMut(&'b str, Result<Value<'a>, serde_json::Error>) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        let hlist::HListValues((key, value), tail) = self;
        callback(key.as_ref(), value.into_value(arena))?;
        tail.into_values(arena, callback)
    }
}
impl<K: AsRef<str>> IntoValues for Vec<(K, Value<'_>)> {
    fn into_values<'a, E>(
        self,
        arena: &'a Arena,
        mut callback: impl for<'b> FnMut(&'b str, Result<Value<'a>, serde_json::Error>) -> Result<(), E>,
    ) -> Result<(), E>
    where
        Self: 'a,
    {
        for (key, value) in self {
            callback(key.as_ref(), value.into_value(arena))?;
        }
        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
pub enum Value<'a> {
    #[default]
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(ValueString<'a>),
    Html(ValueString<'a>),
    Array(&'a RefCell<ValueArray<'a>>),
    Record(&'a RefCell<ValueRecord<'a>>),
    RangeExclusiveOpen(Option<i64>),
    RangeExclusiveClosed(Option<i64>, i64),
    RangeInclusive(Option<i64>, i64),
    Function(&'a (&'a str, Vec<Pat<'a>>, Option<Pat<'a>>, Expr<'a>)),
}

#[derive(Debug, Clone)]
pub enum ValueString<'a> {
    Shared(&'a str),
    Owned {
        buffer: Rc<str>,
        range: std::ops::Range<u32>,
    },
}

pub type ValueArray<'a> = Vec<Value<'a>>;

#[derive(Debug, Clone)]
pub struct ValueRecord<'a> {
    pub entries: Vec<(ValueString<'a>, Value<'a>)>,
}

impl<'a> Value<'a> {
    pub fn from_json(
        ser: impl serde::Serialize,
        arena: &'a Arena,
    ) -> Result<Self, serde_json::Error> {
        serde_json::to_value(ser).map(|value| Self::from_json_value(value, arena))
    }
    pub fn from_json_value(value: serde_json::Value, arena: &'a Arena) -> Self {
        match value {
            serde_json::Value::Null => Self::Null,
            serde_json::Value::Bool(x) => Self::Boolean(x),
            serde_json::Value::Number(x) => {
                if let Some(x) = x.as_i64() {
                    Self::Integer(x)
                } else if let Some(x) = x.as_f64() {
                    Self::Float(x)
                } else {
                    Self::Float(f64::INFINITY)
                }
            }
            serde_json::Value::String(x) => Self::String(x.into()),
            serde_json::Value::Array(x) => Self::Array(
                Box::new(RefCell::new(
                    x.into_iter()
                        .map(|value| Self::from_json_value(value, arena))
                        .collect(),
                ))
                .lift(arena),
            ),
            serde_json::Value::Object(x) => Self::Record(
                Box::new(RefCell::new(ValueRecord {
                    entries: x
                        .into_iter()
                        .map(|(key, value)| (key.into(), Self::from_json_value(value, arena)))
                        .collect(),
                }))
                .lift(arena),
            ),
        }
    }

    pub fn deep_clone<'b>(&self, arena: &'b Arena) -> Value<'b>
    where
        'a: 'b,
    {
        return match *self {
            Self::Null => Value::Null,
            Self::Boolean(x) => Value::Boolean(x),
            Self::Integer(x) => Value::Integer(x),
            Self::Float(x) => Value::Float(x),
            Self::String(ref x) => Value::String(x.clone()),
            Self::Html(ref x) => Value::Html(x.clone()),
            Self::Array(array) => Value::Array(
                Box::new(RefCell::new(
                    array.borrow().iter().map(|x| x.deep_clone(arena)).collect(),
                ))
                .lift(arena),
            ),
            Self::Record(record) => Value::Record(
                Box::new(RefCell::new(ValueRecord {
                    entries: record
                        .borrow()
                        .iter()
                        .map(|(k, v)| (k.clone(), v.deep_clone(arena)))
                        .collect(),
                }))
                .lift(arena),
            ),
            Self::RangeExclusiveOpen(x) => Value::RangeExclusiveOpen(x),
            Self::RangeExclusiveClosed(x, y) => Value::RangeExclusiveClosed(x, y),
            Self::RangeInclusive(x, y) => Value::RangeInclusive(x, y),
            Self::Function(f) => Value::Function(f),
        };
    }
}
impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct ForceDisplay<T>(T);
        impl<T: Display> Debug for ForceDisplay<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Display::fmt(&self.0, f)
            }
        }
        match self {
            Value::Null => f.write_str("null"),
            Value::Boolean(true) => f.write_str("true"),
            Value::Boolean(false) => f.write_str("false"),
            Value::Integer(value) => Display::fmt(value, f),
            Value::Float(value) => Display::fmt(value, f),
            Value::String(value) => Debug::fmt(value.as_str(), f),
            Value::Html(value) => {
                f.write_str("{{")?;
                f.write_str(value)?;
                f.write_str("}}")
            }
            Value::Array(array) => f
                .debug_list()
                .entries(array.borrow().iter().map(ForceDisplay))
                .finish(),
            Value::Record(record) => f
                .debug_map()
                .entries(
                    record
                        .borrow()
                        .iter()
                        .map(|(k, v)| (k.as_str(), ForceDisplay(v))),
                )
                .finish(),
            Value::RangeExclusiveOpen(from) => {
                if let Some(from) = from {
                    Display::fmt(&from, f)?;
                }
                f.write_str("..")?;
                Ok(())
            }
            Value::RangeExclusiveClosed(from, to) => {
                if let Some(from) = from {
                    Display::fmt(&from, f)?;
                }
                f.write_str("..")?;
                Display::fmt(&to, f)?;
                Ok(())
            }
            Value::RangeInclusive(from, to) => {
                if let Some(from) = from {
                    Display::fmt(&from, f)?;
                }
                f.write_str("..=")?;
                Display::fmt(&to, f)
            }
            Value::Function(_) => f.write_str("fn (...) => (...)"),
        }
    }
}
impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Boolean(x), Self::Boolean(y)) => x == y,
            (Self::Integer(x), Self::Integer(y)) => x == y,
            (Self::Float(x), Self::Float(y)) => x == y,
            (Self::String(x), Self::String(y)) => x == y,
            (Self::Html(x), Self::Html(y)) => x == y,
            (Self::Array(x), Self::Array(y)) => x.borrow().as_slice() == y.borrow().as_slice(),
            (Self::Record(x), Self::Record(y)) => x.borrow().as_slice() == y.borrow().as_slice(),
            (Self::RangeExclusiveOpen(x), Self::RangeExclusiveOpen(y)) => x == y,
            (Self::RangeExclusiveClosed(x1, x2), Self::RangeExclusiveClosed(y1, y2)) => {
                x1 == y1 && x2 == y2
            }
            (Self::RangeInclusive(x1, x2), Self::RangeInclusive(y1, y2)) => x1 == y1 && x2 == y2,
            (Self::Function(x), Self::Function(y)) => x as *const _ == y as *const _,
            (_, _) => false,
        }
    }
}
impl Eq for Value<'_> {}

impl Deref for ValueString<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
impl Display for ValueString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
impl Default for ValueString<'_> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> ValueString<'a> {
    pub const fn new() -> Self {
        Self::Shared("")
    }
    pub fn into_string(self) -> String {
        self.to_string()
    }
    pub fn as_str(&self) -> &str {
        match self {
            ValueString::Shared(shared) => shared,
            ValueString::Owned { buffer, range } => {
                &buffer[range.start as usize..range.end as usize]
            }
        }
    }
    pub fn push_str(&mut self, text: &str) {
        *self = ValueString::Owned {
            buffer: format!("{}{}", self.as_str(), text).into(),
            range: 0..(self.len() + text.len()) as u32,
        }
    }
    pub fn slice(&self, start: usize, end: usize) -> Option<Self> {
        match self {
            Self::Shared(shared) => shared.get(start..end).map(Self::Shared),
            Self::Owned { buffer, range } => {
                assert!((range.end - range.start) as usize >= end);
                let range = range.start + start as u32..range.start + end as u32 - 1;
                buffer.get(range.start as usize..range.end as usize)?;
                Some(Self::Owned {
                    buffer: buffer.clone(),
                    range,
                })
            }
        }
    }
}
impl<'a> From<&'a str> for ValueString<'a> {
    fn from(value: &'a str) -> Self {
        Self::Shared(value)
    }
}
impl<'a> From<String> for ValueString<'a> {
    fn from(value: String) -> Self {
        Self::Owned {
            range: 0..value.len() as u32,
            buffer: value.into(),
        }
    }
}
impl<'a> From<Cow<'a, str>> for ValueString<'a> {
    fn from(value: Cow<'a, str>) -> Self {
        match value {
            Cow::Borrowed(borrowed) => borrowed.into(),
            Cow::Owned(owned) => owned.into(),
        }
    }
}
impl Write for ValueString<'_> {
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        let mut buf = self.to_string();
        buf.write_char(c)?;
        *self = buf.into();
        Ok(())
    }
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        let mut buf = self.to_string();
        buf.write_fmt(args)?;
        *self = buf.into();
        Ok(())
    }
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let mut buf = self.to_string();
        buf.write_str(s)?;
        *self = buf.into();
        Ok(())
    }
}
impl PartialEq for ValueString<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}
impl Eq for ValueString<'_> {}
impl PartialOrd for ValueString<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ValueString<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<'a> ValueRecord<'a> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
    pub fn insert(&mut self, key: ValueString<'a>, value: Value<'a>) {
        if let Some((_, stored)) = self.entries.iter_mut().find(|x| x.0 == key) {
            *stored = value;
        } else {
            self.entries.push((key, value));
        }
    }
    pub fn get_map<'s>(&'s self, key: &str) -> Option<&'s Value<'a>> {
        self.iter().find_map(|(k, v)| (&**k == key).then_some(v))
    }

    pub(super) fn summarize_members(&self) -> String {
        let mut string = String::new();
        for (key, _) in &**self {
            string
                .write_fmt(format_args!("{:?}, ", key.as_str()))
                .unwrap();
        }
        string.pop();
        string.pop();
        string
    }
}
impl<'a> Deref for ValueRecord<'a> {
    type Target = Vec<(ValueString<'a>, Value<'a>)>;
    fn deref(&self) -> &Self::Target {
        &self.entries
    }
}
impl<'a> DerefMut for ValueRecord<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.entries
    }
}
impl<'a> IntoIterator for ValueRecord<'a> {
    type IntoIter = <Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::IntoIter;
    type Item = <Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}
impl<'a, 'b> IntoIterator for &'b ValueRecord<'a> {
    type IntoIter = <&'b Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::IntoIter;
    type Item = <&'b Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        (&self.entries).into_iter()
    }
}
impl<'a, 'b> IntoIterator for &'b mut ValueRecord<'a> {
    type IntoIter = <&'b mut Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::IntoIter;
    type Item = <&'b mut Vec<(ValueString<'a>, Value<'a>)> as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        (&mut self.entries).into_iter()
    }
}
