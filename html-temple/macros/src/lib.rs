mod compiler;
mod input;

use proc_macro::TokenStream;

#[proc_macro]
pub fn use_template(input: TokenStream) -> TokenStream {
    let input::UseTemplArgs { path, inputs } = syn::parse_macro_input!(input);
    let inputs = inputs.into_iter().collect::<Vec<_>>();
    compiler::compile_template(&path.value(), &inputs).into()
}


