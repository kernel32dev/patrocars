pub mod arena;
pub mod syntax {
    pub mod error;
    pub mod lexer;
    pub mod parser;
    pub mod source;
}
pub mod runtime {
    pub mod error;
    pub mod eval;
    pub mod run;
    pub mod value;
}
pub mod error;

use arena::{Arena, Liftable};
use error::PathedIoError;
use runtime::value::IntoValues;
use std::{
    cell::RefCell,
    collections::HashMap,
    path::{Path, PathBuf},
};
use syntax::{
    parser::Expr,
    source::{HasSource, Source},
};

pub use runtime::value;

type FileCache<'a> = HashMap<PathBuf, (&'a str, &'a Result<Expr<'a>, syntax::error::Error<'a>>)>;

struct GlobalEnv<'a> {
    file_arena: &'a Arena,
    files: RefCell<FileCache<'a>>,
    root: PathBuf,
}

pub fn eval<'a>(root: &Path, path: &str, inputs: impl IntoValues) -> String {
    match try_eval(root, path, inputs) {
        Ok(value) => value,
        Err(error) => error.to_html(None),
    }
}

pub fn try_eval<'a>(
    root: &Path,
    path: &str,
    inputs: impl IntoValues,
) -> Result<String, PathedIoError> {
    let global_arena = Arena::new();
    let global_env = GlobalEnv {
        file_arena: &global_arena,
        files: RefCell::new(HashMap::new()),
        root: root.to_path_buf(),
    };
    global_env.eval_file(path, inputs)
}

impl<'g> GlobalEnv<'g> {
    fn eval_file<'a>(
        &'g self,
        path: &str,
        inputs: impl IntoValues,
    ) -> Result<String, PathedIoError> {
        match self.load_file(path)?.1 {
            Ok(expr) => {
                let sub_arena = Arena::new();
                let path = path
                    .rsplit_once(|x| matches!(x, '/' | '\\'))
                    .map(|(x, _)| x)
                    .unwrap_or("");
                let mut env = runtime::eval::Env::new(self, &sub_arena, path);
                let result = inputs.into_values(&sub_arena, |k, v| {
                    v.map(|v| {
                        env.set(k.to_string().lift(&sub_arena), v);
                    })
                });
                result.expect("TODO! handle serde_json serialization error");
                let result = runtime::eval::eval_expr(&mut env, expr).map(|x| x.into_html());
                match result {
                    Ok(html) => Ok(html.as_str().to_string()),
                    Err(error) => {
                        let files = self.files.borrow();
                        let sources = |source: Source| {
                            files.iter().find_map(|(file, (code, _))| {
                                source
                                    .infer_range_str(code)
                                    .map(|range| (file, *code, range))
                            })
                        };
                        Ok(error.to_html(sources))
                    }
                }
            }
            Err(error) => {
                let source_range = error.source();
                let files = self.files.borrow();
                let source_entry = files.iter().find_map(|(file, (code, _))| {
                    source_range
                        .infer_range_str(code)
                        .map(|range| (file, *code, range))
                });
                Ok(error.to_html(source_entry))
            }
        }
    }

    fn load_file(
        &self,
        path: &str,
    ) -> Result<(&'g str, &'g Result<Expr<'g>, syntax::error::Error<'g>>), PathedIoError> {
        let path = self.resolve_path(path)?;
        let mut cache = self.files.borrow_mut();
        if let Some(cached) = cache.get(&path) {
            Ok(*cached)
        } else {
            let code = match std::fs::read_to_string(&path) {
                Ok(bytes) => &*bytes.lift(self.file_arena),
                Err(error) => {
                    return Err(PathedIoError {
                        path: path.into_boxed_path(),
                        error,
                    });
                }
            };
            let result = Box::new(syntax::parser::parse(code)).lift(self.file_arena);
            cache.insert(path, (code, result));
            Ok((code, result))
        }
    }

    fn resolve_path(&self, path: &str) -> Result<PathBuf, PathedIoError> {
        let path = path.trim_start_matches(|x| matches!(x, '/' | '\\'));
        if Path::new(path).is_absolute() {
            return Err(PathedIoError {
                path: Path::new(path).to_path_buf().into_boxed_path(),
                error: std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "não é possível usar caminhos absoluto",
                ),
            });
        }
        let path = self.root.join(path);
        path.canonicalize().map_err(|error| PathedIoError {
            path: path.into_boxed_path(),
            error,
        })
    }
}
