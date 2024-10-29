use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Expr, Ident, LitStr, Token};

pub struct UseTemplArgs {
    pub path: LitStr,
    pub inputs: Punctuated<Parameter, Token![,]>,
}

pub struct Parameter {
    pub name: Ident,
    pub value: Option<Expr>,
}

impl Parse for UseTemplArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let template: LitStr = input.parse()?;
        let mut parameters = Punctuated::new();

        while !input.is_empty() {
            let lookahead = input.lookahead1();

            if lookahead.peek(Token![,]) {
                // consume the trailing comma if present
                input.parse::<Token![,]>()?;
                continue;
            }

            if lookahead.peek(Ident) {
                let name = input.parse()?;
                let value = if input.peek(Token![=]) {
                    input.parse::<Token![=]>()?;
                    Some(input.parse()?)
                } else {
                    None
                };
                parameters.push(Parameter { name, value });
            } else {
                return Err(syn::Error::new(input.span(), "Expected identifier"));
            }

            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(UseTemplArgs {
            path: template,
            inputs: parameters,
        })
    }
}
