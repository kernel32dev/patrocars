use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use crate::{
    arena::{Arena, Liftable},
    syntax::{
        parser::{Expr, ExprHtml, ExprItem, ExprKey, ExprLiteral, Pat},
        source::{HasSource, Source},
    },
    GlobalEnv,
};

use super::{
    error::Error,
    run,
    value::{Value, ValueArray, ValueRecord, ValueString},
};

pub struct Env<'a, 'g> {
    global_env: &'g GlobalEnv<'g>,
    pub(crate) arena: &'a Arena,
    path: &'a str,
    added: HashSet<&'a str>,
    map: HashMap<&'a str, Value<'a>>,
}

impl<'a, 'g> Env<'a, 'g> {
    pub(crate) fn new(global_env: &'g GlobalEnv<'g>, arena: &'a Arena, path: &'a str) -> Self {
        Self {
            global_env,
            arena,
            path,
            added: HashSet::new(),
            map: HashMap::new(),
        }
    }
    /// variables defined inside the nest are deleted once the nest is exited
    ///
    /// mutations to variables defined outside the nest are preserved
    fn nest<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_added = std::mem::take(&mut self.added);
        let result = f(self);
        let added = std::mem::replace(&mut self.added, old_added);
        for key in added {
            self.map.remove(key);
        }
        result
    }
    pub fn get(&self, name: &'a str) -> Result<Value<'a>, Error<'a>> {
        self.map
            .get(name)
            .cloned()
            .ok_or(Error::undefined().source(name))
    }
    pub fn set(&mut self, name: &'a str, value: Value<'a>) {
        if let Some(mut_ref) = self.map.get_mut(name) {
            *mut_ref = value;
        } else {
            self.added.insert(name);
            self.map.insert(name, value);
        }
    }
}

pub fn eval_expr<'a>(env: &mut Env<'a, '_>, expr: &'a Expr<'a>) -> Result<Value<'a>, Error<'a>> {
    match expr {
        Expr::Ident(ident) => env.get(ident),
        Expr::Literal(literal) => eval_literal(&literal.1),
        Expr::Include(include) => eval_include(env, &**include),
        Expr::Html(html) => {
            let mut buf = ValueString::new();
            for i in html {
                match i {
                    ExprHtml::Html(html) => buf.push_str(html),
                    ExprHtml::Expr(expr) => match eval_expr(env, expr) {
                        Ok(value) => value.to_html(&mut buf),
                        Err(error) => {
                            let files = env.global_env.files.borrow();
                            let sources = |source: Source| {
                                files.iter().find_map(|(file, (code, _))| {
                                    source
                                        .infer_range_str(code)
                                        .map(|range| (file, *code, range))
                                })
                            };
                            buf.push_str(&error.to_html(sources))
                        }
                    },
                }
            }
            Ok(Value::Html(buf))
        }
        Expr::Array(array) => {
            let mut vec = ValueArray::new();
            for i in &array.1 {
                let value = eval_expr(env, &i.expr)?;
                if i.rest.is_none() {
                    vec.push(value);
                } else {
                    run::spread_array(&mut vec, value, i.expr.source())?;
                }
            }
            Ok(Value::Array(Box::new(RefCell::new(vec)).lift(env.arena)))
        }
        Expr::Record(record) => {
            let (_, entries, _) = &**record;
            let mut record = ValueRecord::new();
            for (k, v) in entries {
                let k = match k {
                        ExprKey::Ident(k) => (*k).into(),
                        ExprKey::Integer(_, k) => k.to_string().into(),
                        ExprKey::String(_, k) => k.clone().into(),
                        ExprKey::Expr(k) => eval_expr(env,k)?.into_string().map_err(|x| x.because("foi necessário converter para uma string, porque foi usado como a chave evaluada de um record, (chave evaluada é essa syntaxe: {(chave): valor}, onde a chave se encontra entre parêntesis)").source(k) )?,
                        ExprKey::Rest(_) => {
                            let value = eval_expr(env, v)?;
                            let value_source = v.source();
                            run::spread_record(&mut record, value, value_source)?;
                            continue;
                        }
                    };
                let v = eval_expr(env, v)?;
                record.entries.push((k, v));
            }
            Ok(Value::Record(
                Box::new(RefCell::new(record)).lift(env.arena),
            ))
        }
        Expr::Function(function) => Ok(Value::Function(&**function)),
        Expr::Assingment(assingment) => {
            let (pat, op, expr) = &**assingment;
            if let (Pat::Ident(pat), "?=") = (pat, *op) {
                if env.get(pat).is_ok_and(|x| !x.is_null()) {
                    return Ok(Value::Null);
                }
            }
            eval_assingment(env, pat, op, expr)?;
            Ok(Value::Null)
        }
        Expr::Prefix(prefix) => {
            let (op, expr) = &**prefix;
            eval_prefix(env, op, expr)
        }
        Expr::Postfix(postfix) => {
            let (expr, op) = &**postfix;
            eval_postfix(env, expr, op)
        }
        Expr::Member(member) => {
            let (expr, member) = &**member;
            run::member(eval_expr(env, expr)?, expr.source(), member)
        }
        Expr::Infix(infix) => {
            let (left, op, right) = &**infix;
            eval_infix(env, left, op, right)
        }
        Expr::Index(index) => {
            let (expr, _, args, _) = &**index;
            eval_index(env, expr, args)
        }
        Expr::Call(call) => {
            let (expr, _, args, _) = &**call;
            eval_call(env, expr, args)
        }
        Expr::Method(method) => {
            let (expr, member, _, args, _) = &**method;
            eval_method(env, expr, &member, &args)
        }
        Expr::Ternary(ternary) => {
            let (condition, when_true, when_false) = &**ternary;
            if eval_expr(env, condition)?.to_bool() {
                eval_expr(env, when_true)
            } else {
                eval_expr(env, when_false)
            }
        }
        Expr::Block(block) => {
            let (_, stmts, tail, _) = &**block;
            env.nest(|env| {
                for i in stmts {
                    eval_expr(env, i)?;
                }
                if let Some(tail) = tail {
                    eval_expr(env, tail)
                } else {
                    Ok(Value::Null)
                }
            })
        }
        Expr::If(if_expr) => {
            let (_, condition, when_true, when_false) = &**if_expr;
            if eval_expr(env, condition)?.to_bool() {
                eval_expr(env, when_true)
            } else if let Some(when_false) = when_false {
                eval_expr(env, when_false)
            } else {
                Ok(Value::Null)
            }
        }
        Expr::For(for_expr) => {
            let (_, index_bind, item_bind, iterable, body) = &**for_expr;
            let mut vec = Vec::new();
            match eval_expr(env, iterable)? {
                Value::Null => {}
                Value::Array(array) => {
                    let array = array.borrow().clone();
                    env.nest(|env| {
                        for (index, item) in array.into_iter().enumerate() {
                            if let Some(index_bind) = index_bind {
                                env.set(*index_bind, Value::Integer(index as i64));
                            }
                            env.set(*item_bind, item);
                            vec.push(eval_expr(env, body)?);
                        }
                        Ok(())
                    })?;
                }
                Value::Record(record) => {
                    let entries = record.borrow().clone();
                    env.nest(|env| {
                        for (index, item) in entries {
                            if let Some(index_bind) = index_bind {
                                env.set(*index_bind, Value::String(index));
                            }
                            env.set(*item_bind, item);
                            vec.push(eval_expr(env, body)?);
                        }
                        Ok(())
                    })?;
                }
                Value::RangeExclusiveClosed(Some(start), end) if index_bind.is_none() => {
                    env.nest(|env| {
                        for index in start..end {
                            env.set(*item_bind, Value::Integer(index));
                            vec.push(eval_expr(env, body)?);
                        }
                        Ok(())
                    })?;
                }
                Value::RangeInclusive(Some(start), end) if index_bind.is_none() => {
                    env.nest(|env| {
                        for index in start..=end {
                            env.set(*item_bind, Value::Integer(index));
                            vec.push(eval_expr(env, body)?);
                        }
                        Ok(())
                    })?;
                }
                value @ (Value::Boolean(_)
                | Value::Integer(_)
                | Value::Float(_)
                | Value::String(_)
                | Value::Html(_)
                | Value::Function(_)) => {
                    return Err(Error::expected_iterable().value(value).source(iterable));
                }
                value @ (Value::RangeExclusiveOpen(_)
                | Value::RangeExclusiveClosed(_, _)
                | Value::RangeInclusive(_, _)) => {
                    return Err(Error::expected_iterable()
                            .value(value)
                            .source(iterable)
                            .because("ranges só são iteráveis se tiverem o começo e fim especificado, ranges como (..3), (4..), (..=7) não são iteráveis"));
                }
            }
            Ok(Value::Array(Box::new(RefCell::new(vec)).lift(env.arena)))
        }
        Expr::RangeExclusive(range) => {
            let (start, _, end) = &**range;
            let start = start
                .as_ref()
                .map(|expr| {
                    eval_expr(env, expr).and_then(|x| run::range_integer(x, expr.source()))
                })
                .transpose()?;
            let end = end
                .as_ref()
                .map(|expr| {
                    eval_expr(env, expr).and_then(|x| run::range_integer(x, expr.source()))
                })
                .transpose()?;
            match end {
                Some(end) => Ok(Value::RangeExclusiveClosed(start, end)),
                None => Ok(Value::RangeExclusiveOpen(start)),
            }
        }
        Expr::RangeInclusive(range) => {
            let (start, _, end) = &**range;
            let start = start
                .as_ref()
                .map(|expr| {
                    eval_expr(env, expr).and_then(|x| run::range_integer(x, expr.source()))
                })
                .transpose()?;
            let end = eval_expr(env, end).and_then(|x| match x {
                Value::Integer(value) => Ok(value),
                value => Err(Error::expected_range_integer().value(value).source(end)),
            })?;
            Ok(Value::RangeInclusive(start, end))
        }
    }
}

fn eval_literal<'a>(lit: &'a ExprLiteral<'a>) -> Result<Value<'a>, Error<'a>> {
    match lit {
        ExprLiteral::Null => Ok(Value::Null),
        ExprLiteral::Boolean(v) => Ok(Value::Boolean(*v)),
        ExprLiteral::Integer(v) => Ok(Value::Integer(*v)),
        ExprLiteral::Float(v) => Ok(Value::Float(*v)),
        ExprLiteral::String(v) => Ok(Value::String(ValueString::Shared(v.as_ref()))),
    }
}

fn eval_assingment<'a>(
    env: &mut Env<'a, '_>,
    pat: &'a Pat<'a>,
    op: &'a str,
    expr: &'a Expr<'a>,
) -> Result<(), Error<'a>> {
    let value = eval_expr(env, expr)?;
    let value_source = expr.source();
    match pat {
        Pat::Ident(ident) => match op {
            "=" => {
                env.set(ident, value);
                Ok(())
            }
            "+=" | "-=" | "*=" | "/=" | "\\=" | "%=" | "&=" => {
                let old_value = env.get(ident).map_err(|x| x.because("esse tipo de atribuição que faz uma operação no valor existente exige que o valor já exista, por tanto não é capaz de criar a variável, apenas é capaz de modificar, tente inicializar o valor da variável antes de modificar").source(op))?;
                let value = run_infix(
                    env,
                    old_value,
                    ident.source(),
                    &op[0..1],
                    value,
                    value_source,
                )?;
                env.set(*ident, value);
                Ok(())
            }
            "?=" => {
                if !env.get(ident).is_ok_and(|x| !x.is_null()) {
                    env.set(*ident, value);
                }
                Ok(())
            }
            _ => panic!("bad assingment operator {op:?}"),
        },
        Pat::Array(_, array, _) => {
            if let Value::Array(array) = value {
                todo!("array pattern")
            } else {
                Err(Error::expected_array().value(value).source(value_source).because("isso é uma desestruturação, que ocorre quando você coloca chaves antes de uma atribuição, exemplo: ([primeiro, segundo] = row), por tanto, row tem que ser um array").source(pat))
            }
        }
        Pat::Record(_, record, _) => {
            if let Value::Record(record) = value {
                todo!("record pattern")
            } else {
                Err(Error::expected_record().value(value).source(value_source).because("isso é uma desestruturação, que ocorre quando você coloca chaves antes de uma atribuição, exemplo: ({x, y} = ponto), por tanto, ponto tem que ser um record").source(pat))
            }
        }
    }
}

fn eval_prefix<'a>(
    env: &mut Env<'a, '_>,
    op: &'a str,
    expr: &'a Expr<'a>,
) -> Result<Value<'a>, Error<'a>> {
    let value = eval_expr(env, expr)?;
    let value_source = expr.source();
    match op {
        "!" => Ok(Value::Boolean(!value.to_bool())),
        "!!" => Ok(Value::Boolean(value.to_bool())),
        "-" => run::negate(op.source(), value, value_source),
        "#" => run::measure(op.source(), value, value_source),
        _ => panic!("bad prefix operator {op:?}"),
    }
}
fn eval_postfix<'a>(
    env: &mut Env<'a, '_>,
    expr: &'a Expr<'a>,
    op: &'a str,
) -> Result<Value<'a>, Error<'a>> {
    let value = eval_expr(env, expr)?;
    match op {
        "???" => Ok(Value::String(format!("{value}").into())),
        _ => panic!("bad postfix operator {op:?}"),
    }
}
fn eval_infix<'a>(
    env: &mut Env<'a, '_>,
    left: &'a Expr<'a>,
    op: &'a str,
    right: &'a Expr<'a>,
) -> Result<Value<'a>, Error<'a>> {
    let left_value = eval_expr(env, left)?;
    let early = match op {
        "??" => !left_value.is_null(),
        "!!" => left_value.is_null(),
        "||" => left_value.to_bool(),
        "&&" => !left_value.to_bool(),
        _ => {
            let right_value = eval_expr(env, right)?;
            return run_infix(
                env,
                left_value,
                left.source(),
                op,
                right_value,
                right.source(),
            );
        }
    };
    if early {
        Ok(left_value)
    } else {
        eval_expr(env, right)
    }
}

fn run_infix<'a>(
    env: &mut Env<'a, '_>,
    left: Value<'a>,
    left_source: Source,
    op: &'a str,
    right: Value<'a>,
    right_source: Source,
) -> Result<Value<'a>, Error<'a>> {
    match op {
        "??" | "!!" | "||" | "&&" => panic!("run infix cannot run lazy evaluation operators"),
        "==" => Ok(Value::Boolean(left == right)),
        "!=" => Ok(Value::Boolean(left != right)),
        "is" => Ok(Value::Boolean(run::ptr_eq(&left, &right))),
        "in" => todo!("in operator"),
        "<" => Ok(Value::Boolean(
            run::cmp(left, left_source, op.source(), right, right_source)?.is_lt(),
        )),
        ">" => Ok(Value::Boolean(
            run::cmp(left, left_source, op.source(), right, right_source)?.is_gt(),
        )),
        "<=" => Ok(Value::Boolean(
            run::cmp(left, left_source, op.source(), right, right_source)?.is_le(),
        )),
        ">=" => Ok(Value::Boolean(
            run::cmp(left, left_source, op.source(), right, right_source)?.is_ge(),
        )),
        "&" => run::concat(left, op.source(), right),
        "+" => run::add(
            env.arena,
            left,
            left_source,
            op.source(),
            right,
            right_source,
        ),
        "-" => run::subtract(left, left_source, op.source(), right, right_source),
        "*" => run::multiply(
            env.arena,
            left,
            left_source,
            op.source(),
            right,
            right_source,
        ),
        "/" => run::divide(left, left_source, op.source(), right, right_source),
        "\\" => run::integer_divide(left, left_source, op.source(), right, right_source),
        "%" => run::remain(left, left_source, op.source(), right, right_source),
        _ => panic!("bad infix operator {op:?}"),
    }
}

fn eval_index<'a>(
    env: &mut Env<'a, '_>,
    expr: &'a Expr<'a>,
    index_exprs: &'a [ExprItem<'a>],
) -> Result<Value<'a>, Error<'a>> {
    let index_sources = |index: usize| index_exprs.get(index).map(|x| x.source());
    run::index(
        env.arena,
        eval_expr(env, expr)?,
        expr.source(),
        eval_items(env, index_exprs),
        (0, &index_sources),
    )
}

fn eval_call<'a>(
    env: &mut Env<'a, '_>,
    expr: &'a Expr<'a>,
    arg_exprs: &'a [ExprItem<'a>],
) -> Result<Value<'a>, Error<'a>> {
    match eval_expr(env, expr)? {
        Value::Function((_, pats, rest, body)) => {
            let args = eval_items(env, arg_exprs)?;
            env.nest(|env| {
                // TODO! bind args to pats and rest
                eval_expr(env, body)
            })
        }
        value => Err(Error::expected_function().value(value).source(expr)),
    }
}

fn eval_include<'a>(
    env: &mut Env<'a, '_>,
    include: &'a (&'a str, (&'a str, Cow<'a, str>), Option<Expr<'a>>),
) -> Result<Value<'a>, Error<'a>> {
    let (_, (src_source, src), inputs) = include;
    let inputs = match inputs {
        None => Vec::new(),
        Some(inputs) => match eval_expr(env, inputs)? {
            Value::Record(record) => record
                .borrow()
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
            _ => todo!("non record on include"),
        },
    };
    let path = if src.starts_with('/') || src.starts_with('\\') {
        Cow::Borrowed(src.as_ref())
    } else {
        Cow::Owned(format!("{}/{}", env.path, src))
    };

    //let output = env.global_env.eval_file(&path, inputs).map_err(Error::Io)?;
    let output = env.global_env.eval_file(&path, inputs).unwrap_or_else(|e| {
        let files = env.global_env.files.borrow();
        let source = files.iter().find_map(|(file, (code, _))| {
            src_source
                .source()
                .infer_range_str(code)
                .map(|range| (file, *code, range))
        });
        e.to_html(source)
    });
    Ok(Value::Html(output.into()))
}

fn eval_method<'a>(
    env: &mut Env<'a, '_>,
    expr: &'a Expr<'a>,
    method: &'a str,
    args: &'a [ExprItem<'a>],
) -> Result<Value<'a>, Error<'a>> {
    let value = eval_expr(env, expr)?;
    let arg_sources = |index: usize| args.get(index).map(|x| x.source());
    let args = eval_items(env, args);
    run::method(env.arena, value, expr.source(), method, args, arg_sources)
}

fn eval_items<'a>(
    env: &mut Env<'a, '_>,
    args: &'a [ExprItem<'a>],
) -> Result<Vec<Value<'a>>, Error<'a>> {
    let mut vec = Vec::new();
    for i in args {
        let value = eval_expr(env, &i.expr)?;
        if i.rest.is_none() {
            vec.push(value);
        } else {
            if let Value::Array(array) = value {
                vec.extend_from_slice(&**array.borrow());
            } else {
                return Err(Error::expected_array()
                    .because("você só pode mergir arrays nos argumentos de uma função")
                    .value(value));
            }
        }
    }
    Ok(vec)
}
