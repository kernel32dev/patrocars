use proc_macro::TokenStream;
use proc_macro2::TokenStream as Tokens;
use quote::quote;
use std::path::Path;
use syn::LitStr;

#[proc_macro]
pub fn use_asset(input: TokenStream) -> TokenStream {
    let path = syn::parse_macro_input!(input as LitStr).value();
    let file = load_asset(path.as_ref()).1;
    TokenStream::from(quote!({
        const ASSET: &::server_utils::asset::FileRecord = #file;
        &ASSET
    }))
}

#[proc_macro]
pub fn use_assets(input: TokenStream) -> TokenStream {
    let path = syn::parse_macro_input!(input as LitStr).value();
    let dir = load_assets(path.as_ref()).1;
    TokenStream::from(quote!({
        const ASSET: &::server_utils::asset::DirRecord = #dir;
        &ASSET
    }))
}

fn load_asset(path: &Path) -> (String, Tokens) {
    let path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join(path)
    };
    let name = path
        .file_name()
        .expect("path has no filename")
        .to_str()
        .expect("filename must be valid utf-8")
        .to_string();
    let mime = match mime_guess::from_path(&path)
        .first()
        .map(|x| x.to_string())
        .as_deref()
    {
        Some(text) => quote!(::std::option::Option::Some(#text)),
        None => quote!(::std::option::Option::None),
    };
    let data = std::fs::read(path).unwrap();
    let tokens = match std::str::from_utf8(&data) {
        Ok(text) => {
            quote! {{
                let text = #text;
                &::server_utils::asset::FileRecord {
                    name: #name,
                    mime: #mime,
                    bytes: text.as_bytes(),
                    text: ::std::option::Option::Some(text)
                }
            }}
        }
        Err(_) => {
            let bytes = proc_macro2::Literal::byte_string(&data);
            quote! {
                &::server_utils::asset::FileRecord {
                    name: #name,
                    mime: #mime,
                    bytes: #bytes,
                    text: ::std::option::Option::None
                }
            }
        }
    };
    (name, tokens)
}

fn load_assets(path: &Path) -> (String, Tokens) {
    let path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join(path)
    };
    let name = path
        .file_name()
        .expect("path has no filename")
        .to_str()
        .expect("filename must be valid utf-8")
        .to_string();
    let mut dirs = Vec::new();
    let mut files = Vec::new();
    for entry in std::fs::read_dir(path).unwrap().map(|x| x.unwrap()) {
        if entry.metadata().unwrap().is_dir() {
            dirs.push(load_assets(&entry.path()))
        } else {
            files.push(load_asset(&entry.path()))
        }
    }
    dirs.sort_unstable_by(|(x, _), (y, _)| x.cmp(y));
    files.sort_unstable_by(|(x, _), (y, _)| x.cmp(y));
    let dirs = dirs.into_iter().map(|x| x.1);
    let files = files.into_iter().map(|x| x.1);
    let tokens = quote! {
        &::server_utils::asset::DirRecord {
            name: #name,
            dirs: &[#(#dirs,)*],
            files: &[#(#files,)*],
        }
    };
    (name, tokens)
}
