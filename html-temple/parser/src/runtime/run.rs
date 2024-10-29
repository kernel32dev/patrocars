use std::{borrow::Cow, cell::RefCell, cmp::Ordering, fmt::Write};

use crate::{arena::{Arena, Liftable}, syntax::source::Source};

use super::{error::Error, value::{Value, ValueArray, ValueRecord, ValueString}};

impl<'a> Value<'a> {
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }
    pub fn to_bool(&self) -> bool {
        match *self {
            Self::Null => false,
            Self::Boolean(x) => x,
            Self::Integer(x) => x != 0,
            Self::Float(x) => x != 0.0,
            Self::String(ref x) => !x.is_empty(),
            Self::Html(ref x) => !x.is_empty(),
            Self::Array(x) => !x.borrow().is_empty(),
            Self::Record(x) => !x.borrow().is_empty(),
            Self::RangeExclusiveOpen(_) => true,
            Self::RangeExclusiveClosed(Some(start), end) => start < end,
            Self::RangeExclusiveClosed(None, i64::MIN) => false,
            Self::RangeExclusiveClosed(_, _) => true,
            Self::RangeInclusive(Some(start), end) => start <= end,
            Self::RangeInclusive(None, _) => true,
            Self::Function(_) => true,
        }
    }
    pub fn to_html(&self, html: &mut ValueString) {
        match self {
            Self::Null => {}
            Self::Boolean(_) => {}
            Self::Integer(x) => write!(html, "{}", x).unwrap(),
            Self::Float(x) => write!(html, "{}", x).unwrap(),
            Self::String(x) => {
                let mut x = x.as_str();
                while let Some(index) =
                    x.find(|x| matches!(x, '<' | '>' | '&' | '"' | '\''))
                {
                    html.push_str(&x[..index]);
                    let entity = match &x[index..index + 1] {
                        "<" => "&lt;",
                        ">" => "&gt;",
                        "&" => "&amp;",
                        //"\"" => "&quot;",
                        //"'" => "&#39;",
                        _ => unreachable!(),
                    };
                    html.push_str(entity);
                    x = &x[index + 1..];
                }
                html.push_str(x);
            }
            Self::Html(x) => html.push_str(x),
            Self::Array(x) => {
                for i in &*x.borrow() {
                    i.to_html(html);
                }
            }
            Self::Record(x) => {
                for (_, i) in &**x.borrow() {
                    i.to_html(html);
                }
            }
            Self::RangeExclusiveOpen(_) => {}
            Self::RangeExclusiveClosed(_, _) => {}
            Self::RangeInclusive(_, _) => {}
            Self::Function(_) => {}
        }
    }
    pub fn into_string(self) -> Result<ValueString<'a>, Error<'a>> {
        Ok(match self {
            Self::Null => ValueString::new(),
            Self::Boolean(_) => ValueString::new(),
            Self::Integer(v) => v.to_string().into(),
            Self::Float(v) => v.to_string().into(),
            Self::String(text) => text,
            Self::Html(_) => return Err(Error::cannot_stringify_html()),
            Self::Array(v) => {
                let mut buf = ValueString::new();
                for i in &*v.borrow() {
                    buf.push_str(&i.clone().into_string()?);
                }
                buf
            }
            Self::Record(v) => {
                let mut buf = ValueString::new();
                for (_, i) in &**v.borrow() {
                    buf.push_str(&i.clone().into_string()?);
                }
                buf
            }
            Self::RangeExclusiveOpen(_) => ValueString::new(),
            Self::RangeExclusiveClosed(_, _) => ValueString::new(),
            Self::RangeInclusive(_, _) => ValueString::new(),
            Self::Function(_) => ValueString::new(),
        })
    }
    pub fn into_html(self) -> ValueString<'a> {
        if let Value::Html(html) = self {
            html
        } else {
            let mut buf = ValueString::new();
            self.to_html(&mut buf);
            buf
        }
    }
}

pub fn for_each<'a>(arena: &'a Arena, value: Value<'a>, value_source: Source, mut body: impl FnMut(Value<'a>) -> Result<Value<'a>, Error<'a>>) -> Result<Value<'a>, Error<'a>> {
    let vec = match value {
        Value::RangeExclusiveClosed(Some(start), end) => {
            let mut vec = ValueArray::new();
            if end > start {
                vec.reserve((end - start) as usize);
            }
            for index in start..end {
                vec.push(body(Value::Integer(index))?);
            }
            vec
        }
        Value::RangeInclusive(Some(start), end) => {
            let mut vec = ValueArray::new();
            if end >= start {
                vec.reserve((end - start) as usize + 1);
            }
            for index in start..=end {
                vec.push(body(Value::Integer(index))?);
            }
            vec
        }
        value @ (Value::RangeExclusiveOpen(_)
        | Value::RangeExclusiveClosed(_, _)
        | Value::RangeInclusive(_, _)) => {
            return Err(Error::expected_iterable()
                    .value(value)
                    .source(value_source)
                    .because("ranges só são iteráveis se tiverem o começo e fim especificado, ranges como (..3), (4..), (..=7) não são iteráveis"));
        }
        value => {
            return for_each_enumerated(arena, value, value_source, |_, x| body(x))
        }
    };
    Ok(Value::Array(Box::new(RefCell::new(vec)).lift(arena)))
}

pub fn for_each_enumerated<'a>(arena: &'a Arena, value: Value<'a>, value_source: Source, mut body: impl FnMut(Value<'a>, Value<'a>) -> Result<Value<'a>, Error<'a>>) -> Result<Value<'a>, Error<'a>> {
    let mut vec = ValueArray::new();
    match value {
        Value::Null => {
        }
        Value::Array(array) => {
            let array = array.borrow().clone();
            for (index, item) in array.into_iter().enumerate() {
                vec.push(body(Value::Integer(index as i64), item)?);
            }
        }
        Value::Record(record) => {
            let entries = record.borrow().clone();
            for (index, item) in entries {
                vec.push(body(Value::String(index), item)?);
            }
        }
        value @ (Value::Boolean(_)
        | Value::Integer(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Html(_)
        | Value::Function(_)) => {
            return Err(Error::expected_iterable().value(value).source(value_source));
        }
        value @ (Value::RangeExclusiveOpen(_)
        | Value::RangeExclusiveClosed(_, _)
        | Value::RangeInclusive(_, _)) => {
            return Err(Error::expected_iterable()
                    .value(value)
                    .source(value_source)
                    .because("TODO! explciar que for i,j in 1..2 não dá"));
        }
    }
    Ok(Value::Array(Box::new(RefCell::new(vec)).lift(arena)))
}

pub fn range_integer<'a>(value: Value<'a>, value_source: Source) -> Result<i64, Error<'a>> {
    match value {
        Value::Integer(value) => Ok(value),
        value => Err(Error::expected_range_integer().value(value).source(value_source)),
    }
}

pub fn spread_args<'a>(target: &mut Vec<Value<'a>>, value: Value<'a>, value_source: Source) -> Result<(), Error<'a>> {
    match value {
        Value::Array(array) => {
            target.extend_from_slice(&*array.borrow());
            Ok(())
        }
        value => {
             Err(Error::expected_array()
                .because("você só pode mergir arrays nos argumentos de uma função")
                .because_type(value).source(value_source))
        }
    }
}
pub fn spread_array<'a>(target: &mut ValueArray<'a>, value: Value<'a>, value_source: Source) -> Result<(), Error<'a>> {
    match value {
        Value::Array(array) => {
            target.extend_from_slice(&*array.borrow());
            Ok(())
        }
        value => {
             Err(Error::expected_array()
                .because("você só pode mergir um array com outros arrays")
                .because_type(value).source(value_source))
        }
    }
}

pub fn spread_record<'a>(target: &mut ValueRecord<'a>, value: Value<'a>, value_source: Source) -> Result<(), Error<'a>> {
    match value {
        Value::Record(record) => {
            for (k, v) in &*record.borrow() {
                if target.entries.iter().all(|x| x.0 != *k) {
                    target.entries.push((k.clone(), v.clone()));
                }
            }
            Ok(())
        }
        value => {
             Err(Error::expected_record()
                .because("você só pode mergir um record com outros records")
                .because_type(value).source(value_source))
        }
    }
}

pub fn ptr_eq<'a>(l: &Value<'a>, r: &Value<'a>) -> bool {
    match (l, r) {
        (Value::Array(x), Value::Array(y)) => (*x) as *const _ == (*y) as *const _,
        (Value::Record(x), Value::Record(y)) => (*x) as *const _ == (*y) as *const _,
        (l, r) => l == r,
    }
}

pub fn negate<'a>(op: Source, value: Value<'a>, value_source: Source) -> Result<Value<'a>, Error<'a>> {
    match value {
        Value::Boolean(false) => Ok(Value::Integer(0)),
        Value::Boolean(true) => Ok(Value::Integer(-1)),
        Value::Integer(i) => Ok(Value::Integer(i.wrapping_neg())),
        Value::Float(f) => Ok(Value::Float(-f)),
        _ => Err(Error::expected_number()
            .value(value)
            .source(value_source)
            .because("o sinal de menos só trabalha com integers, floats e booleans")
            .source(op)),
    }
}
pub fn measure<'a>(op: Source, value: Value<'a>, value_source: Source) -> Result<Value<'a>, Error<'a>> {
    Ok(Value::Integer(match value {
        Value::Null => 0,
        Value::String(string) => string.len() as i64,
        Value::Html(html) => html.len() as i64,
        Value::Array(array) => array.borrow().len() as i64,
        Value::Record(record) => record.borrow().entries.len() as i64,
        Value::RangeExclusiveClosed(Some(start), end) => {
            // TODO! check if this correctly calculates the number of items in the range
            // TODO! do the same check on the branch for RangeInclusive
            end.checked_sub(start).map(|x| x.max(0)).unwrap_or(0)
        }
        Value::RangeInclusive(Some(start), end) => end
            .checked_sub(start)
            .and_then(|x| x.checked_add(1))
            .map(|x| x.max(0))
            .unwrap_or(0),
        value => {
            let range_info = matches!(
                value,
                Value::RangeExclusiveOpen(_)
                    | Value::RangeExclusiveClosed(None, _)
                    | Value::RangeInclusive(None, _)
            );
            return Err(Error::expected_measureable().value(value).source(value_source).because("o operador de medição só funciona com coisas que tem um tamanho, que são null, string, html, array, record e ranges").source(op).maybe(range_info, |x| x.because("ranges só são iteráveis se tiverem o começo e fim especificado, ranges como (..3), (4..), (..=7) não são iteráveis")));
        }
    }))
}

pub fn cmp<'a>(
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Ordering, Error<'a>> {
    // orders NaN before all floating point numbers, including before negative infinity
    let ord_cmp_f64 = |x: f64, y: f64| -> Ordering {
        x.partial_cmp(&y).unwrap_or((!x.is_nan()).cmp(&!y.is_nan()))
    };
    Ok(match (left, right) {
        (Value::Null, Value::Null) => Ordering::Equal,
        (Value::Null, _) => Ordering::Less,
        (_, Value::Null) => Ordering::Greater,
        (Value::Boolean(x), Value::Boolean(y)) => x.cmp(&y),
        (Value::Integer(x), Value::Integer(y)) => x.cmp(&y),
        (Value::Float(x), Value::Float(y)) => ord_cmp_f64(x, y),

        (Value::Boolean(x), Value::Integer(y)) => (x as i64).cmp(&y),
        (Value::Integer(x), Value::Boolean(y)) => x.cmp(&(y as i64)),

        (Value::Boolean(x), Value::Float(y)) => ord_cmp_f64(x as i8 as f64, y),
        (Value::Float(x), Value::Boolean(y)) => ord_cmp_f64(x, y as i8 as f64),

        (Value::Integer(x), Value::Float(y)) => ord_cmp_f64(x as f64, y),
        (Value::Float(x), Value::Integer(y)) => ord_cmp_f64(x, y as f64),

        (Value::String(x), Value::String(y)) => x.cmp(&y),
        (Value::Html(x), Value::Html(y)) => x.cmp(&y),
        (Value::Array(left), Value::Array(right)) => {
            let x = left.borrow();
            let y = right.borrow();
            for (i, (x, y)) in x.iter().zip(y.iter()).enumerate() {
                let cmp = cmp(x.clone(), left_source, op, y.clone(), right_source).map_err(|x| x.because(format!("esse erro ocorreu quando o valor na posição {i} dos arrays estavam sendo comparados")))?;
                if cmp.is_ne() {
                    return Ok(cmp);
                }
            }
            x.len().cmp(&y.len())
        }
        (Value::Record(left), Value::Record(right)) => {
            let x = left.borrow();
            let y = right.borrow();
            if x.len() == y.len() && x.iter().zip(y.iter()).all(|((a, _), (b, _))| a == b) {
                for ((i, x), (_, y)) in x.iter().zip(y.iter()) {
                    let cmp = cmp(x.clone(), left_source, op, y.clone(), right_source).map_err(|x| x.because(format!("esse erro ocorreu quando o valor do membro {:?} dos records estavam sendo comparados", i.as_str())))?;
                    if cmp.is_ne() {
                        return Ok(cmp);
                    }
                }
                Ordering::Equal
            } else {
                let left_members = x.summarize_members();
                let right_members = y.summarize_members();
                return Err(Error::cannot_compare()
                    .source(op)
                    .because("apenas é possível comparar records quando eles tem os mesmos itens na mesma ordem")
                    .because( if left_members.is_empty() {
                        Cow::Borrowed("o valor a esquerda é um record, e não tem nenhum membro")
                    } else {
                        Cow::Owned(format!("o valor a esquerda é um record, e tem os seguintes membros: {left_members}"))
                    })
                    .value(Value::Record(left))
                    .source(left_source)
                    .because( if right_members.is_empty() {
                        Cow::Borrowed("o valor a direita é um record, e não tem nenhum membro")
                    } else {
                        Cow::Owned(format!("o valor a direita é um record, e tem os seguintes membros: {right_members}"))
                    })
                    .value(Value::Record(right))
                    .source(right_source));
            }
        }
        (
            left @ (Value::RangeExclusiveOpen(_)
            | Value::RangeExclusiveClosed(_, _)
            | Value::RangeInclusive(_, _)),
            right @ (Value::RangeExclusiveOpen(_)
            | Value::RangeExclusiveClosed(_, _)
            | Value::RangeInclusive(_, _)),
        ) => {
            return Err(Error::cannot_compare()
                .source(op)
                .because("não é possível comparar ranges")
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source));
        }
        (left @ Value::Function(_), right @ Value::Function(_)) => {
            return Err(Error::cannot_compare()
                .source(op)
                .because("não é possível comparar funções")
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source));
        }
        (left, right) => {
            return Err(Error::cannot_compare()
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}

pub fn concat<'a>(left: Value<'a>, op: Source, right: Value<'a>) -> Result<Value<'a>, Error<'a>> {
    match (left, right) {
        (Value::Html(mut left), right) => {
            right.to_html(&mut left);
            Ok(Value::Html(left))
        }
        (left, Value::Html(right)) => {
            let mut left = left.into_html();
            left.push_str(&right);
            Ok(Value::Html(left))
        }
        (left, right) => match left.into_string().and_then(|mut l| {
            l.push_str(&right.into_string()?);
            Ok(l)
        }) {
            Ok(x) => Ok(Value::String(x)),
            Err(error) => Err(error
                .because(
                    "foi necessário converter em string por causa do operador de concatenação",
                )
                .source(op)),
        },
    }
}
pub fn add<'a>(
    arena: &'a Arena,
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle integer overflow
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Integer(x as i64 + y as i64),
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),

        (Value::Boolean(x), Value::Integer(y)) => Value::Integer(x as i64 + y),
        (Value::Integer(x), Value::Boolean(y)) => Value::Integer(x + y as i64),

        (Value::Boolean(x), Value::Float(y)) => Value::Float(x as i64 as f64 + y),
        (Value::Float(x), Value::Boolean(y)) => Value::Float(x + y as i64 as f64),

        (Value::Integer(x), Value::Float(y)) => Value::Float(x  as f64 + y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x + y as f64),
        (Value::Array(x), Value::Array(y)) => {
            let vec = x.borrow().iter().chain(y.borrow().iter()).cloned().collect();
            Value::Array(
                Box::new(RefCell::new(vec)).lift(arena)
            )
        },
        (left, right) => {
            return Err(Error::cannot_add()
                .maybe(matches!(left, Value::String(_) | Value::Html(_) ) || matches!(right, Value::String(_) | Value::Html(_) ), |x| x.because("se você estiver tentando concatenar string ou html, use o operador de concatenação \"&\""))
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}
pub fn subtract<'a>(
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle integer overflow
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Integer(x as i64 - y as i64),
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x - y),

        (Value::Boolean(x), Value::Integer(y)) => Value::Integer(x as i64 - y),
        (Value::Integer(x), Value::Boolean(y)) => Value::Integer(x - y as i64),

        (Value::Boolean(x), Value::Float(y)) => Value::Float(x as i64 as f64 - y),
        (Value::Float(x), Value::Boolean(y)) => Value::Float(x - y as i64 as f64),

        (Value::Integer(x), Value::Float(y)) => Value::Float(x  as f64 - y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x - y as f64),
        (left, right) => {
            return Err(Error::cannot_subtract()
                .maybe(matches!(left, Value::String(_) | Value::Html(_) ) || matches!(right, Value::String(_) | Value::Html(_) ), |x| x.because("se você estiver tentando concatenar string ou html, use o operador de concatenação \"&\""))
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}
pub fn multiply<'a>(
    arena: &'a Arena,
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle integer overflow
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Integer(x as i64 * y as i64),
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),

        (Value::Boolean(x), Value::Integer(y)) => Value::Integer(x as i64 * y),
        (Value::Integer(x), Value::Boolean(y)) => Value::Integer(x * y as i64),

        (Value::Boolean(x), Value::Float(y)) => Value::Float(x as i64 as f64 * y),
        (Value::Float(x), Value::Boolean(y)) => Value::Float(x * y as i64 as f64),

        (Value::Integer(x), Value::Float(y)) => Value::Float(x  as f64 * y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x * y as f64),

        (Value::Array(x), Value::Integer(y)) => {
            match y { 
                1.. => {
                    let vec= std::iter::repeat(x.borrow().iter().cloned()).take(y as usize).flatten().collect();
                    Value::Array(
                        Box::new(RefCell::new(vec)).lift(arena)
                    )
                }
                0 => {
                    Value::Array(
                        Box::new(RefCell::new(Vec::new())).lift(arena)
                    )
                }
                ..=-1 => {
                    return Err(Error::cannot_repeat_array_negative().because("esse número é negativo").value(Value::Integer(y)).source(right_source))
                }
            }
        },
        (Value::Integer(x), Value::Array(y)) => {
            match x { 
                1.. => {
                    let vec= std::iter::repeat(y.borrow().iter().cloned()).take(x as usize).flatten().collect();
                    Value::Array(
                        Box::new(RefCell::new(vec)).lift(arena)
                    )
                }
                0 => {
                    Value::Array(
                        Box::new(RefCell::new(Vec::new())).lift(arena)
                    )
                }
                ..=-1 => {
                    return Err(Error::cannot_repeat_array_negative().because("esse número é negativo").value(Value::Integer(x)).source(left_source))
                }
            }
        },
        (Value::String(x), Value::Integer(y)) => {
            if !y.is_negative() {
                Value::String(x.as_str().repeat(y as usize).into())
            } else {
                return Err(Error::cannot_repeat_string_negative().because("esse número é negativo").value(Value::Integer(y)).source(right_source))
            }
        },
        (Value::Integer(x), Value::String(y)) => {
            if !x.is_negative() {
                Value::String(y.as_str().repeat(x as usize).into())
            } else {
                return Err(Error::cannot_repeat_string_negative().because("esse número é negativo").value(Value::Integer(x)).source(right_source))
            }
        },
        (left, right) => {
            return Err(Error::cannot_multiply()
                .maybe(matches!(left, Value::String(_) | Value::Html(_) ) || matches!(right, Value::String(_) | Value::Html(_) ), |x| x.because("se você estiver tentando concatenar string ou html, use o operador de concatenação \"&\""))
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}
pub fn divide<'a>(
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle division by zero
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Float(x as i64 as f64 / y as i64 as f64),
        (Value::Integer(x), Value::Integer(y)) => Value::Float(x as f64 / y as f64),
        (Value::Float(x), Value::Float(y)) => Value::Float(x / y),

        (Value::Boolean(x), Value::Integer(y)) => Value::Float(x as i64 as f64 / y as f64),
        (Value::Integer(x), Value::Boolean(y)) => Value::Float(x as f64 / y as i64 as f64),

        (Value::Boolean(x), Value::Float(y)) => Value::Float(x as i64 as f64 / y),
        (Value::Float(x), Value::Boolean(y)) => Value::Float(x / y as i64 as f64),

        (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 / y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x / y as f64),
        (left, right) => {
            return Err(Error::cannot_divide()
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}
pub fn integer_divide<'a>(
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle division by zero
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Integer(x as i64 / y as i64),
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x / y),
        (Value::Float(x), Value::Float(y)) => Value::Integer((x / y) as i64),

        (Value::Boolean(x), Value::Integer(y)) => Value::Integer(x as i64 / y as i64),
        (Value::Integer(x), Value::Boolean(y)) => Value::Integer(x / y as i64),

        (Value::Boolean(x), Value::Float(y)) => Value::Integer(x as i64 / y as i64),
        (Value::Float(x), Value::Boolean(y)) => Value::Integer(x as i64 / y as i64),

        (Value::Integer(x), Value::Float(y)) => Value::Integer((x as f64 / y) as i64),
        (Value::Float(x), Value::Integer(y)) => Value::Integer((x / y as f64) as i64),
        (left, right) => {
            return Err(Error::cannot_divide()
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}
pub fn remain<'a>(
    left: Value<'a>,
    left_source: Source,
    op: Source,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    // TODO! handle remain by zero
    Ok(match (left, right) {
        (Value::Boolean(x), Value::Boolean(y)) => Value::Integer(x as i64 % y as i64),
        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x % y ),
        (Value::Float(x), Value::Float(y)) => Value::Float(x % y),

        (Value::Boolean(x), Value::Integer(y)) => Value::Integer(x as i64 % y),
        (Value::Integer(x), Value::Boolean(y)) => Value::Integer(x % y as i64),

        (Value::Boolean(x), Value::Float(y)) => Value::Float(x as i64 as f64 % y),
        (Value::Float(x), Value::Boolean(y)) => Value::Float(x % y as i64 as f64),

        (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 % y),
        (Value::Float(x), Value::Integer(y)) => Value::Float(x % y as f64),
        (left, right) => {
            return Err(Error::cannot_remain()
                .source(op)
                .because_left_type(left)
                .source(left_source)
                .because_right_type(right)
                .source(right_source))
        }
    })
}

pub fn method<'a>(
    arena: &'a Arena,
    value: Value<'a>,
    value_source: Source,
    method: &'a str,
    args: Result<Vec<Value<'a>>, Error<'a>>,
    arg_sources: impl Fn(usize) -> Option<Source>,
) -> Result<Value<'a>, Error<'a>> {
    match (value, method) {
        (Value::Array(array), "sum" | "avg") => {
            if !args?.is_empty() {
                todo!("bad arguments")
            }
            let array = array.borrow();
            let mut total = 0;
            for (index, item) in array.iter().enumerate() {
                match item {
                    Value::Null | Value::Boolean(false) => {}
                    Value::Boolean(true) => total += 1, // TODO! handle integer overflow
                    Value::Integer(value) => total += value, // TODO! handle integer overflow
                    Value::Float(value) => {
                        let mut total = total as f64 + value;
                        for item in &array[index + 1..] {
                            match item {
                                Value::Null | Value::Boolean(false) => {}
                                Value::Boolean(true) => total += 1.0,
                                Value::Integer(value) => total += *value as f64,
                                Value::Float(value) => total += *value,
                                Value::String(_)
                                | Value::Html(_)
                                | Value::Array(_)
                                | Value::Record(_)
                                | Value::RangeExclusiveOpen(_)
                                | Value::RangeExclusiveClosed(_, _)
                                | Value::RangeInclusive(_, _)
                                | Value::Function(_) => {
                                    todo!()
                                }
                            }
                        }
                        if method == "avg" {
                            total /= array.len() as f64;
                        }
                        return Ok(Value::Float(total));
                    }
                    Value::String(_)
                    | Value::Html(_)
                    | Value::Array(_)
                    | Value::Record(_)
                    | Value::RangeExclusiveOpen(_)
                    | Value::RangeExclusiveClosed(_, _)
                    | Value::RangeInclusive(_, _)
                    | Value::Function(_) => {
                        todo!()
                    }
                }
            }
            if method == "avg" {
                Ok(Value::Float(total as f64 / array.len() as f64))
            } else {
                Ok(Value::Integer(total))
            }
        }
        (Value::Integer(value), "decimal") => {
            let precision = match *args? {
                [] => 2, // default precision
                [Value::Integer(value)] => value,
                [..] => todo!("bad arguments"),
            };
            if precision < 0 || precision > 18 {
                todo!("bad arguments")
            }
            Ok(Value::String(
                fmt_decimal_int(value, precision as u16).into(),
            ))
        }
        (Value::Float(value), "decimal") => {
            let precision = match *args? {
                [] => 2, // default precision
                [Value::Integer(value)] => value,
                [..] => todo!("bad arguments"),
            };
            if precision < 0 || precision > 18 {
                todo!("bad arguments")
            }
            Ok(Value::String(
                fmt_decimal_float(value, precision as u16).into(),
            ))
        }
        (value, method) => Err(Error::method_does_not_exist(method).because_type(value)),
    }
}

pub fn member<'a> (
    value: Value<'a>,
    value_source: Source,
    member: &'a str
) -> Result<Value<'a>, Error<'a>> {
    match value {
        Value::Record(record) => {
            if let Some(value) = record
                .borrow()
                .get_map(member)
                .cloned()
            {
                Ok(value)
            } else {
                let members = record.borrow().summarize_members();
                if members.is_empty() {
                    Err(Error::undefined_member().because(format!("o membro {member:?} não existe nesse record, ele não tem nenhum membro")).value(Value::Record(record)).source(value_source))
                } else {
                    Err(Error::undefined_member().because(format!("o membro {member:?} não existe nesse record, ele tem os seguintes membros: {members}")).value(Value::Record(record)).source(value_source))
                }
            }
        }
        value => Err(Error::expected_record().because("foi esperado um record porque a sintaxe de acesso de membro foi usada (record.membro), se você queria era chamar um método, adicione parentêsis depois do membro (valor.metodo())").value(value).source(value_source)),
    }
}

pub fn index<'a>(
    arena: &'a Arena,
    value: Value<'a>,
    value_source: Source,
    indexes: Result<Vec<Value<'a>>, Error<'a>>,
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<Value<'a>, Error<'a>> {
    match value {
        Value::String(string) => index_string(string, value_source, &indexes?, index_sources),
        Value::Array(array) => index_array(arena, array, value_source, &indexes?, index_sources),
        Value::Record(record) => index_record(arena, record, value_source, &indexes?, index_sources),
        value => Err(Error::expected_indexable()
            .value(value)
            .source(value_source)),
    }
}
fn index_value<'a>(
    arena: &'a Arena,
    value: Value<'a>,
    value_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<Value<'a>, Error<'a>> {
    if indexes.is_empty() {
        return Ok(value);
    }
    match value {
        Value::String(string) => index_string(string, value_source, indexes, index_sources),
        Value::Array(array) => index_array(arena, array, value_source, indexes, index_sources),
        Value::Record(record) => index_record(arena, record, value_source, indexes, index_sources),
        value => Err(Error::expected_indexable()
            .value(value)
            .source(value_source)),
    }
}
fn index_array_values<'a>(
    arena: &'a Arena,
    array: &mut ValueArray<'a>,
    array_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<(), Error<'a>> {
    if !indexes.is_empty() {
        for (index, item) in array.iter_mut().enumerate() {
            *item = index_value(
                arena,
                std::mem::take(item),
                array_source,
                indexes,
                index_sources,
            )
            .map_err(|x| {
                x.because(format!(
                    "erro ocorreu enquanto o item [{index}] estava sendo indexado"
                ))
            })?;
        }
    }
    Ok(())
}
fn index_record_values<'a>(
    arena: &'a Arena,
    record: &mut ValueRecord<'a>,
    record_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<(), Error<'a>> {
    if !indexes.is_empty() {
        for (key, item) in record {
            *item = index_value(
                arena,
                std::mem::take(item),
                record_source,
                indexes,
                index_sources,
            )
            .map_err(|x| {
                x.because(format!(
                    "erro ocorreu enquanto o membro [{:?}] estava sendo indexado",
                    key.as_str()
                ))
            })?;
        }
    }
    Ok(())
}

pub(crate) fn index_string<'a>(
    string: ValueString<'a>,
    string_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<Value<'a>, Error<'a>> {
    match *indexes {
        [] => Ok(Value::String(string)),
        [Value::Integer(index)] => {
            let index = if index >= 0 {
                index
            } else {
                index + string.len() as i64
            };
            match Some(index)
                .filter(|x| *x >= 0 && *x <= u32::MAX as i64)
                .and_then(|index| string.slice(index as usize, 1 + index as usize))
            {
                Some(slice) => Ok(Value::String(slice)),
                None => Err(Error::out_of_bounds()
                    .value(Value::Integer(index))
                    .source(index_sources.1(index_sources.0).unwrap_or_default())
                    .because("essa posição não existe nesta string")
                    .source(string_source)
                    .value(Value::String(string))),
            }
        }
        [Value::RangeExclusiveOpen(None)] => Ok(Value::String(string)),
        [Value::RangeExclusiveOpen(Some(start))] => todo!("slice string"),
        [Value::RangeExclusiveClosed(start, end)] => todo!("slice string"),
        [Value::RangeInclusive(start, end)] => todo!("slice string"),
        _ => todo!("error bad indexes for string"),
    }
}

pub(crate) fn index_array<'a>(
    arena: &'a Arena,
    array: &'a RefCell<ValueArray<'a>>,
    array_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<Value<'a>, Error<'a>> {
    match *indexes {
        [] => Ok(Value::Array(array)),
        [Value::Integer(index), ..] => {
            let array_ref = array.borrow();
            let index = if index >= 0 {
                index
            } else {
                index + array_ref.len() as i64
            };
            if index >= 0 && index < array_ref.len() as i64 {
                let value = array_ref[index as usize].clone();
                drop(array_ref);
                index_value(
                    arena,
                    value,
                    array_source,
                    &indexes[1..],
                    (index_sources.0 + 1, index_sources.1),
                )
            } else {
                Err(Error::out_of_bounds()
                    .value(Value::Integer(index))
                    .source(index_sources.1(index_sources.0).unwrap_or_default())
                    .because("essa posição não existe neste array")
                    .source(array_source)
                    .value(Value::Array(array)))
            }
        }
        [Value::RangeExclusiveOpen(None), ..] => {
            let mut array = array.borrow().clone();
            index_array_values(
                arena,
                &mut array,
                array_source,
                &indexes[1..],
                (index_sources.0 + 1, index_sources.1),
            )?;
            Ok(Value::Array(Box::new(RefCell::new(array)).lift(arena)))
        }
        [Value::RangeExclusiveOpen(Some(start)), ..] => todo!("slice array"),
        [Value::RangeExclusiveClosed(start, end), ..] => todo!("slice array"),
        [Value::RangeInclusive(start, end), ..] => todo!("slice array"),
        [ref bad_index, ..] => todo!("error bad array index"),
    }
}

pub(crate) fn index_record<'a>(
    arena: &'a Arena,
    record: &'a RefCell<ValueRecord<'a>>,
    record_source: Source,
    indexes: &[Value<'a>],
    index_sources: (usize, &impl Fn(usize) -> Option<Source>),
) -> Result<Value<'a>, Error<'a>> {
    match *indexes {
        [] => Ok(Value::Record(record)),
        [Value::Integer(index), ..] => {
            let record_ref = record.borrow();
            let index = if index >= 0 {
                index
            } else {
                index + record_ref.len() as i64
            };
            if index >= 0 && index < record_ref.len() as i64 {
                let value = record_ref[index as usize].1.clone();
                drop(record_ref);
                index_value(
                    arena,
                    value,
                    record_source,
                    &indexes[1..],
                    (index_sources.0 + 1, index_sources.1),
                )
            } else {
                Err(Error::out_of_bounds()
                    .value(Value::Integer(index))
                    .source(index_sources.1(index_sources.0).unwrap_or_default())
                    .because("essa posição não existe neste array")
                    .source(record_source)
                    .value(Value::Record(record)))
            }
        }
        [Value::String(ref index), ..] => {
            let record_ref = record.borrow();
            if let Some(value) = record_ref.get_map(index).cloned() {
                drop(record_ref);
                index_value(
                    arena,
                    value,
                    record_source,
                    &indexes[1..],
                    (index_sources.0 + 1, index_sources.1),
                )
            } else {
                Err(Error::out_of_bounds()
                    .value(Value::String(index.clone()))
                    .source(index_sources.1(index_sources.0).unwrap_or_default())
                    .because("essa posição não existe neste array")
                    .source(record_source)
                    .value(Value::Record(record)))
            }
        }
        [Value::RangeExclusiveOpen(None), ..] => {
            let mut record = record.borrow().clone();
            index_record_values(
                arena,
                &mut record,
                record_source,
                &indexes[1..],
                (index_sources.0 + 1, index_sources.1),
            )?;
            Ok(Value::Record(
                Box::new(RefCell::new(record)).lift(arena),
            ))
        }
        [ref bad_index, ..] => todo!("error bad record index"),
    }
}

fn fmt_decimal_float(value: f64, precision: u16) -> String {
    let s = format!("{:.*}", precision as usize, value);

    let Some((int_part, dec_part)) = s.split_once('.') else {
        return fmt_decimal_int(value as i64, precision);
    };

    let mut formatted = String::new();
    let mut count = 0;

    for c in int_part.chars().rev() {
        if count == 3 {
            formatted.push('.');
            count = 0;
        }
        formatted.push(c);
        count += 1;
    }

    formatted = formatted.chars().rev().collect::<String>();
    formatted.push_str(",");
    formatted.push_str(dec_part);
    formatted
}

fn fmt_decimal_int(value: i64, precision: u16) -> String {
    let mut formatted = String::new();
    let mut count = 0;

    for c in value.abs().to_string().chars().rev() {
        if count == 3 {
            formatted.push('.');
            count = 0;
        }
        formatted.push(c);
        count += 1;
    }
    if value.is_negative() {
        formatted.push('-');
    }

    if precision == 0 {
        formatted.chars().rev().collect()
    } else {
        formatted
            .chars()
            .rev()
            .chain(Some(','))
            .chain(std::iter::repeat('0').take(precision as usize))
            .collect()
    }
}
