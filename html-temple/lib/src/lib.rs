pub use html_temple_parser as parser;

pub fn runtime_eval_template(
    root: impl AsRef<std::path::Path>,
    path: impl AsRef<str>,
    inputs: impl parser::value::IntoValues,
) -> String {
    html_temple_parser::eval(root.as_ref(), path.as_ref(), inputs)
}

#[cfg(not(debug_assertions))]
pub use comptime_use_template as use_template;

pub use html_temple_macros::use_template as comptime_use_template;

#[cfg(debug_assertions)]
pub use runtime_use_template as use_template;

#[macro_export]
#[cfg(debug_assertions)]
macro_rules! runtime_use_template {
    ($e:literal $(,)?) => {
        html_temple::runtime_eval_template(
            ::std::env!("CARGO_MANIFEST_DIR"),
            $e,
            html_temple::parser::value::hlist::HListDone,
        )
    };
    //($e:literal $(, $input:expr)* $(,)?) => {
    ($e:literal, $($input:tt)*) => {
        html_temple::runtime_eval_template(
            ::std::env!("CARGO_MANIFEST_DIR"),
            $e,
            html_temple::runtime_use_template!(@ $($input)*),
        )
    };
    (@) => {
        html_temple::parser::value::hlist::HListDone
    };
    (@ $bind:ident = $input:expr, $($rest:tt)*) => {
        html_temple::parser::value::hlist::HListValues((stringify!($bind), $input), html_temple::runtime_use_template!(@ $($tt)*))
    };
    (@ $bind:ident = $input:expr) => {
        html_temple::parser::value::hlist::HListValues((stringify!($bind), $input), html_temple::parser::value::hlist::HListDone)
    };
    (@ $bind:ident, $($rest:tt)*) => {
        html_temple::parser::value::hlist::HListValues((stringify!($bind), $bind), html_temple::runtime_use_template!(@ $($rest)*))
    };
    (@ $bind:ident) => {
        html_temple::parser::value::hlist::HListValues((stringify!($bind), $bind), html_temple::parser::value::hlist::HListDone)
    };
}