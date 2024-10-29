use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use html_temple_parser::syntax::parser::{Expr, ExprKey, ExprLiteral, Pat};
use proc_macro2::TokenStream as Tokens;
use quote::{format_ident, quote};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarState {
    UnusedInput,
    Defined,
    Undefined,
}

pub struct Env {
    vars: Vec<(String, VarState)>,
    files: Vec<(PathBuf, &'static str)>,
    curdir: PathBuf,
}

impl Env {
    fn insert(&mut self, var: &str, state: VarState) {
        if let Some((_, stored_state)) = self.vars.iter_mut().find(|(x, _)| x == var) {
            if *stored_state == VarState::UnusedInput {
                *stored_state = state;
            }
        } else {
            self.vars.push((var.to_string(), state));
        }
    }
}

impl Drop for Env {
    fn drop(&mut self) {
        unsafe {
            self.vars.clear();
            for (_, leaked) in std::mem::take(&mut self.files) {
                drop(Box::from_raw(leaked as *const str as *mut str));
            }
        }
    }
}

pub fn compile_template(path: &str, inputs: &[crate::input::Parameter]) -> Tokens {
    let mut env = Env {
        vars: inputs
            .iter()
            .map(|x| (x.name.to_string(), VarState::UnusedInput))
            .collect(),
        files: Vec::new(),
        curdir: Path::new(path)
            .parent()
            .unwrap_or(Path::new(""))
            .to_path_buf(),
    };
    let file_name_len = Path::new(path).file_name().map(|x| x.len()).unwrap_or(0);
    let value = compile_path(&mut env, &path[path.len() - file_name_len..]);
    let vars = env
        .vars
        .iter()
        .filter(|(_, x)| *x == VarState::Defined)
        .map(|(x, _)| quote::format_ident!("{x}"));
    let files = env.files.iter().map(|x| x.1);
    let input_exprs = inputs
        .iter()
        .map(|crate::input::Parameter { name, value }| match &value {
            Some(value) => quote!(#value),
            None => quote!(#name),
        });
    let input_names = inputs.iter().map(|x| &x.name).collect::<Vec<_>>();
    quote!({
        fn template(
            #(#input_names: impl ::html_temple::parser::value::IntoValue,)*
        ) -> ::std::string::String {
            #[allow(unused)]
            let r#do = ::html_temple::parser::arena::Arena::new();
            #(let #input_names = #input_names.into_value(&r#do).expect("TODO! handle serde_json serialization error");)*
            #[allow(unused)]
            let r#in = (#(#files,)*);
            #[allow(unused)]
            #[allow(unused_mut)]
            #[allow(non_snake_case)]
            let r#return = (|| {
                #(let mut #vars = ::html_temple::parser::value::Value::Null;)*
                ::std::result::Result::<::html_temple::parser::value::Value, ::html_temple::parser::runtime::error::Error>::Ok(#value)
            })();
            match r#return {
                ::std::result::Result::Ok(value) => value.into_html().into_string(),
                ::std::result::Result::Err(error) => error.to_html(|_| None),
            }
        }
        template(#(#input_exprs,)*)
    })
}

fn compile_path(env: &mut Env, path: &str) -> Tokens {
    let text = path;
    let path = if text.chars().nth(0).is_some_and(|x| matches!(x, '\\' | '/'))
        && !text.chars().nth(1).is_some_and(|x| matches!(x, '\\' | '/'))
    {
        let workspace_dir = std::env::var_os("CARGO_MANIFEST_DIR")
            .expect("environment variable CARGO_MANIFEST_DIR must be present");
        Path::new(&workspace_dir).join(&text[1..])
    } else if Path::new(path).is_absolute() {
        Path::new(path).to_path_buf()
    } else {
        env.curdir.join(path)
    };

    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            panic!(
                "failed to canonicalize {} ({text:?}) [curdir = {:?}] (CMD = {:?}), {:?}",
                path.display(),
                env.curdir,
                std::env::var_os("CARGO_MANIFEST_DIR")
                    .expect("environment variable CARGO_MANIFEST_DIR must be present"),
                error
            )
        }
    };

    let code = match env.files.iter().find(|x| x.0 == path) {
        Some((_, code)) => *code,
        None => {
            let code = match std::fs::read_to_string(&path) {
                Ok(code) => code,
                Err(error) => panic!("failed to read {}, {:?}", path.display(), error),
            };
            let code = &*Box::leak(code.into_boxed_str());
            env.files.push((path.clone(), code));
            code
        }
    };
    match html_temple_parser::syntax::parser::parse(code) {
        Ok(expr) => {
            let old = std::mem::replace(
                &mut env.curdir,
                path.parent().unwrap_or(Path::new("")).to_path_buf(),
            );
            let tokens = compile_expr(env, &expr);
            env.curdir = old;
            tokens
        }
        Err(error) => {
            let error = format!("{error:#?}");
            quote!(compile_error!(#error))
        }
    }
}

fn compile_expr<'a>(env: &mut Env, expr: &'a Expr<'a>) -> Tokens {
    match expr {
        Expr::Ident(ident) => {
            env.insert(*ident, VarState::Undefined);
            let ident = quote::format_ident!("{ident}");
            quote!(#ident.clone())
        }
        Expr::Literal(literal) => {
            let literal = match &literal.1 {
                ExprLiteral::Null => quote!(Null),
                ExprLiteral::Boolean(true) => quote!(Boolean(true)),
                ExprLiteral::Boolean(false) => quote!(Boolean(false)),
                ExprLiteral::Integer(integer) => quote!(Integer(#integer)),
                ExprLiteral::Float(float) => quote!(Float(#float)),
                ExprLiteral::String(Cow::Borrowed(string)) => {
                    // TODO! replace string literal with a slice of the source
                    quote!(String(::html_temple::parser::value::ValueString::Shared(#string)))
                }
                ExprLiteral::String(Cow::Owned(string)) => {
                    quote!(String(::html_temple::parser::value::ValueString::Shared(#string)))
                }
            };
            quote!(::html_temple::parser::value::Value::#literal)
        }
        Expr::Include(include) => {
            let (_, (_, path), inputs) = &**include;
            compile_path(env, path)
        }
        Expr::Html(html) => {
            let html = html.iter().map(|x| {
                match x {
                    html_temple_parser::syntax::parser::ExprHtml::Html(html) => {
                        // TODO! replace string literal with a slice of the source
                        quote!(r#return.push_str(#html);)
                    }
                    html_temple_parser::syntax::parser::ExprHtml::Expr(expr) => {
                        let expr = compile_expr(env, expr);
                        quote!(#expr.to_html(&mut r#return);)
                    }
                }
            });
            quote!({
                let mut r#return = ::html_temple::parser::value::ValueString::new();
                #(#html)*
                ::html_temple::parser::value::Value::Html(r#return)
            })
        }
        Expr::Array(array) => {
            let array = array.1.iter().map(|x| {
                let expr = compile_expr(env, &x.expr);
                if x.rest.is_none() {
                    quote!(r#return.push(#expr))
                } else {
                    quote!(::html_temple::parser::runtime::run::spread_array(&mut r#return, #expr, Default::default())?)
                }
            });
            quote!({
                let mut r#return = ::html_temple::parser::value::ValueArray::new();
                #(#array;)*
                ::html_temple::parser::value::Value::Array(&*r#do.boxed(::std::cell::RefCell::new(r#return)))
            })
        }
        Expr::Record(record) => {
            let record = record.1.iter().map(|(key, expr)| {
                let expr = compile_expr(env, expr);
                match key {
                    ExprKey::Ident(key) | ExprKey::String(_, Cow::Borrowed(key)) => {
                        // TODO! replace string literal with a slice of the source
                        quote!(r#return.insert(::html_temple::parser::value::ValueString::Shared(#key), #expr))
                    },
                    ExprKey::Integer(_, key) => {
                        let key = key.to_string();
                        quote!(r#return.insert(::html_temple::parser::value::ValueString::Shared(#key), #expr))
                    },
                    ExprKey::String(_, Cow::Owned(key)) => {
                        quote!(r#return.insert(::html_temple::parser::value::ValueString::Shared(#key), #expr))
                    },
                    ExprKey::Expr(_) => todo!("ExprKey::Expr"),
                    ExprKey::Rest(_) => {
                        quote!(::html_temple::parser::runtime::run::spread_record(&mut r#return, #expr, Default::default())?)
                    },
                }
            });
            quote!({
                let mut r#return = ::html_temple::parser::value::ValueRecord::new();
                #(#record;)*
                ::html_temple::parser::value::Value::Record(&*r#do.boxed(::std::cell::RefCell::new(r#return)))
            })
        }
        Expr::Function(_) => quote!(todo!()),
        Expr::Assingment(assingment) => {
            let (pat, op, expr) = &**assingment;
            let assignment = match pat {
                Pat::Ident(ident) => {
                    if matches!(*op, "=" | "?=") {
                        env.insert(*ident, VarState::Defined);
                    } else {
                        env.insert(*ident, VarState::Undefined);
                    }
                    let ident = format_ident!("{}", ident);
                    let expr = compile_expr(env, expr);
                    match *op {
                        "=" => {
                            quote!(#ident = #expr)
                        }
                        "?=" => {
                            quote!(if #ident.is_null() { #ident = #expr })
                        }
                        "+=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::add(&r#do, #ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "-=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::subtract(#ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "*=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::multiply(&r#do, #ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "/=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::divide(#ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "\\=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::integer_divide(#ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "%=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::remain(#ident.clone(), Default::default(), Default::default(), #expr, Default::default())?)
                        }
                        "&=" => {
                            quote!(#ident = ::html_temple::parser::runtime::run::concat(#ident.clone(), Default::default(), #expr)?)
                        }
                        _ => panic!("bad assingment operator"),
                    }
                }
                Pat::Array(_, _, _) => todo!("Pat::Array"),
                Pat::Record(_, _, _) => todo!("Pat::Record"),
            };
            quote!({
                #assignment;
                ::html_temple::parser::value::Value::Null
            })
        }
        Expr::Prefix(prefix) => {
            let (op, expr) = &**prefix;
            let expr = compile_expr(env, expr);
            match *op {
                "!" => {
                    quote!(::html_temple::parser::value::Value::Boolean(!#expr.to_bool()))
                }
                "!!" => {
                    quote!(::html_temple::parser::value::Value::Boolean(#expr.to_bool()))
                }
                "-" => {
                    quote!(::html_temple::parser::runtime::run::negate(Default::default(), #expr, Default::default())?)
                }
                "#" => {
                    quote!(::html_temple::parser::runtime::run::measure(Default::default(), #expr, Default::default())?)
                }
                _ => panic!("bad prefix operator {op:?}"),
            }
        }
        Expr::Postfix(postfix) => {
            let (expr, op) = &**postfix;
            let expr = compile_expr(env, expr);
            match *op {
                "???" => {
                    quote!(::html_temple::parser::value::Value::String(
                        ::html_temple::parser::value::ValueString::from(format!("{}", #expr))
                    ))
                }
                _ => panic!("bad postfix operator {op:?}"),
            }
        }
        Expr::Member(member) => {
            let (expr, member) = &**member;
            let expr = compile_expr(env, expr);
            // TODO! replace string literal with a slice of the source
            quote!(::html_temple::parser::runtime::run::member(#expr, Default::default(), #member)?)
        }
        Expr::Infix(infix) => {
            let (left, op, right) = &**infix;
            let left = compile_expr(env, left);
            let right = compile_expr(env, right);
            match *op {
                "??" => {
                    quote!(match #left {
                        ::html_temple::parser::value::Value::Null => #right,
                        r#match => r#match
                    })
                }
                "!!" => {
                    quote!(match #left {
                        ::html_temple::parser::value::Value::Null => ::html_temple::parser::value::Value::Null,
                        _ => #right
                    })
                }
                "||" => {
                    quote!(match #left {
                        r#match if r#match.to_bool() => r#match,
                        _ => #right
                    })
                }
                "&&" => {
                    quote!(match #left {
                        r#match if !r#match.to_bool() => r#match,
                        _ => #right
                    })
                }
                "==" => quote!(::html_temple::parser::value::Value::Boolean(#left == #right)),
                "!=" => quote!(::html_temple::parser::value::Value::Boolean(#left != #right)),
                "is" => {
                    quote!(::html_temple::parser::value::Value::Boolean(::html_temple::parser::runtime::run::ptr_eq(&#left, &#right)))
                }
                "in" => quote!(todo!("in operator")),
                "<" | ">" | "<=" | ">=" => {
                    let op = match *op {
                        "<" => quote!(is_lt),
                        ">" => quote!(is_gt),
                        "<=" => quote!(is_le),
                        ">=" => quote!(is_ge),
                        _ => unreachable!(),
                    };
                    quote!(::html_temple::parser::value::Value::Boolean(::html_temple::parser::runtime::run::cmp(#left, Default::default(), Default::default(), #right, Default::default())?.#op()))
                }
                "&" => {
                    quote!(::html_temple::parser::runtime::run::concat(#left, Default::default(), #right)?)
                }
                "+" | "-" | "*" | "/" | "\\" | "%" => {
                    let arena = matches!(*op, "+" | "*").then(|| quote!(&r#do,));
                    let op = match *op {
                        "+" => quote!(add),
                        "-" => quote!(subtract),
                        "*" => quote!(multiply),
                        "/" => quote!(divide),
                        "\\" => quote!(integer_divide),
                        "%" => quote!(remain),
                        _ => unreachable!(),
                    };
                    quote!(::html_temple::parser::runtime::run::#op(#arena #left, Default::default(), Default::default(), #right, Default::default())?)
                }
                _ => panic!("bad infix operator {op:?}"),
            }
        }
        Expr::Call(_) => quote!(todo!("Expr::Call")),
        Expr::Index(index) => {
            let (expr, _, args, _) = &**index;
            let expr = compile_expr(env, expr);

            let args = args.iter().map(|x| {
                let expr = compile_expr(env, &x.expr);
                if x.rest.is_none() {
                    quote!(r#return.push(#expr))
                } else {
                    quote!(::html_temple::parser::runtime::run::spread_args(&mut r#return, #expr, Default::default())?)
                }
            });

            let args = quote!((|| {
                let mut r#return = ::std::vec::Vec::new();
                #(#args;)*
                ::std::result::Result::<Vec<::html_temple::parser::value::Value>, ::html_temple::parser::runtime::error::Error>::Ok(r#return)
            })());

            quote!(::html_temple::parser::runtime::run::index(&r#do, #expr, Default::default(), #args, (0, &|_| ::std::option::Option::None))?)
        }
        Expr::Method(method) => {
            let (expr, method, _, args, _) = &**method;
            let expr = compile_expr(env, expr);

            let args = args.iter().map(|x| {
                let expr = compile_expr(env, &x.expr);
                if x.rest.is_none() {
                    quote!(r#return.push(#expr))
                } else {
                    quote!(::html_temple::parser::runtime::run::spread_args(&mut r#return, #expr?, Default::default())?)
                }
            });

            let args = quote!((|| {
                let mut r#return = ::std::vec::Vec::new();
                #(#args;)*
                ::std::result::Result::<Vec<::html_temple::parser::value::Value>, ::html_temple::parser::runtime::error::Error>::Ok(r#return)
            })());

            // TODO! replace string literal with a slice of the source
            quote!(::html_temple::parser::runtime::run::method(&r#do, #expr, Default::default(), #method, #args, |_| ::std::option::Option::None)?)
        }
        Expr::Ternary(_) => quote!(todo!("Expr::Ternary")),
        Expr::Block(block) => {
            let (_, stmts, tail, _) = &**block;
            let stmts = stmts.iter().map(|x| compile_expr(env, x));
            let stmts = quote!(#(let _ = #stmts;)*);
            let tail = match tail {
                Some(tail) => compile_expr(env, tail),
                None => quote!(::html_temple::parser::value::Value::Null),
            };
            quote!({ #stmts #tail })
        }
        Expr::If(_) => quote!(todo!("Expr::If")),
        Expr::For(for_expr) => {
            let (_, index_bind, item_bind, iterable, body) = &**for_expr;
            let iterable = compile_expr(env, iterable);
            env.insert(item_bind, VarState::Defined);
            let item_bind_ident = quote::format_ident!("{item_bind}");
            match index_bind {
                Some(index_bind) => {
                    env.insert(index_bind, VarState::Defined);
                    let index_bind_ident = quote::format_ident!("{index_bind}");
                    let body = compile_expr(env, body);
                    quote!(::html_temple::parser::runtime::run::for_each_enumerated(
                            &r#do,
                            #iterable,
                            Default::default(),
                            |r#false, r#true| {
                                #index_bind_ident = r#false;
                                #item_bind_ident = r#true;
                                ::std::result::Result::<::html_temple::parser::value::Value, ::html_temple::parser::runtime::error::Error>::Ok(#body)
                            }
                        )?)
                }
                None => {
                    let body = compile_expr(env, body);
                    quote!(::html_temple::parser::runtime::run::for_each(
                            &r#do,
                            #iterable,
                            Default::default(),
                            |r#true| {
                                #item_bind_ident = r#true;
                                ::std::result::Result::<::html_temple::parser::value::Value, ::html_temple::parser::runtime::error::Error>::Ok(#body)
                            }
                        )?)
                }
            }
        }
        Expr::RangeExclusive(range) => {
            let (start, _, end) = &**range;
            match (start, end) {
                (None, None) => quote!(::html_temple::parser::value::Value::RangeExclusiveOpen(
                    ::std::option::Option::None
                )),
                (None, Some(end)) => {
                    let end = compile_range_integer(env, end);
                    quote!(::html_temple::parser::value::Value::RangeExclusiveClosed(::std::option::Option::None, #end))
                }
                (Some(start), None) => {
                    let start = compile_range_integer(env, start);
                    quote!(::html_temple::parser::value::Value::RangeExclusiveOpen(::std::option::Option::Some(#start)))
                }
                (Some(start), Some(end)) => {
                    let start = compile_range_integer(env, start);
                    let end = compile_range_integer(env, end);
                    quote!(::html_temple::parser::value::Value::RangeExclusiveClosed(::std::option::Option::Some(#start), #end))
                }
            }
        }
        Expr::RangeInclusive(range) => {
            let (start, _, end) = &**range;
            let end = compile_range_integer(env, end);
            match start {
                Some(start) => {
                    let start = compile_range_integer(env, start);
                    quote!(::html_temple::parser::value::Value::RangeInclusive(::std::option::Option::Some(#start), #end))
                }
                None => {
                    quote!(::html_temple::parser::value::Value::RangeInclusive(::std::option::Option::None, #end))
                }
            }
        }
    }
}

fn compile_range_integer<'a>(env: &mut Env, expr: &'a Expr) -> Tokens {
    let expr = compile_expr(env, expr);
    quote!(::html_temple::parser::runtime::run::range_integer(#expr?, Default::default())?)
}
