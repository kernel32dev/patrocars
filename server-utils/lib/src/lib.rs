pub mod asset;
mod body;
pub mod convert;
pub mod cookie;
mod from;
mod fs;
mod impls;
pub mod log;
mod path;
pub mod server;
pub mod ws;

pub use crate::{
    body::{Body, BodyError},
    path::{PathError, UrlPath},
};
pub use bytes;
pub use hyper::http::{self, StatusCode as Status};
pub use server_utils_macros::{use_asset as comptime_use_asset, use_assets as comptime_use_assets};

#[derive(Debug)]
pub struct Request {
    pub method: http::Method,
    pub uri: http::Uri,
    pub path: path::UrlPath,
    pub version: http::Version,
    pub headers: http::HeaderMap,
    pub extensions: http::Extensions,
    pub body: RequestBody,
}

#[derive(Default, Debug)]
pub struct RequestBody(Option<body::Body>);

#[derive(Default, Debug)]
pub struct Response {
    pub status: http::StatusCode,
    pub version: http::Version,
    pub headers: http::HeaderMap,
    pub extensions: http::Extensions,
    pub body: body::Body,
}

pub trait ReplyError: std::error::Error {
    fn error_status(&self) -> Status {
        Status::INTERNAL_SERVER_ERROR
    }
}
impl<T: ReplyError> From<T> for Response {
    #[cfg(debug_assertions)]
    fn from(value: T) -> Self {
        Response::new()
            .with_status(value.error_status())
            .with_body(format!("{value:#?}"))
    }
    #[cfg(not(debug_assertions))]
    fn from(value: T) -> Self {
        Response::new().with_status(value.error_status())
    }
}

#[macro_export]
#[cfg(debug_assertions)]
macro_rules! use_asset {
    ($e:expr) => {
        $crate::asset::FileRecord::debug_runtime_load(::std::concat!(
            ::std::env!("CARGO_MANIFEST_DIR"),
            "/",
            $e
        ))
    };
}
#[macro_export]
#[cfg(debug_assertions)]
macro_rules! use_assets {
    ($e:expr) => {
        $crate::asset::DirRecord::debug_runtime_load(::std::concat!(
            ::std::env!("CARGO_MANIFEST_DIR"),
            "/",
            $e
        ))
    };
}
#[cfg(not(debug_assertions))]
pub use server_utils_macros::{use_asset, use_assets};

#[macro_export]
#[cfg(debug_assertions)]
macro_rules! use_file {
    ($e:literal) => {
        $crate::Response::file_sync(::std::concat!(::std::env!("CARGO_MANIFEST_DIR"), "/", $e))
    };
}
#[macro_export]
#[cfg(not(debug_assertions))]
macro_rules! use_file {
    ($e:literal) => {{
        $crate::Response::asset($crate::use_asset!($e))
    }};
}
#[macro_export]
#[cfg(debug_assertions)]
macro_rules! use_dir {
    ($root:literal, $segments:expr) => {
        $crate::Response::dir(
            ::std::concat!(::std::env!("CARGO_MANIFEST_DIR"), "/", $root),
            $segments,
        )
    };
}
#[macro_export]
#[cfg(not(debug_assertions))]
macro_rules! use_dir {
    ($root:literal, $segments:expr) => {
        $crate::Response::dir($crate::use_assets!($root), $segments)
    };
}

macro_rules! unit_error_expr {
    ($name:ident $e:expr) => {{
        crate::unit_error_struct! {$name $e};
        $name
    }};
}
pub(crate) use unit_error_expr;

macro_rules! unit_error_struct {
    ($vis:vis $name:ident $e:expr) => {
        $vis struct $name;
        impl ::std::clone::Clone for $name {
            fn clone(&self) -> Self {
                $name
            }
        }
        impl ::std::marker::Copy for $name {}
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.write_str($e)
            }
        }
        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                ::std::fmt::Debug::fmt(self, f)
            }
        }
        impl ::std::error::Error for $name {}
        impl crate::ReplyError for $name {
            fn error_status(&self) -> crate::Status {
                crate::Status::BAD_REQUEST
            }
        }
    };
}
pub(crate) use unit_error_struct;
