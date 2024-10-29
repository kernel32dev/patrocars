use crate::{
    body::{self, Body},
    Response,
};
use hyper::http;
use std::{borrow::Cow, convert::Infallible, ops::ControlFlow};

macro_rules! impl_reply_for_from_body {
    ($(impl $t:ty)*) => {$(
        impl From<$t> for Response {
            fn from(value: $t) -> Self {
                Response::new().with_body(value)
            }
        }
    )*};
}

impl_reply_for_from_body! {
    impl String
    impl Vec<u8>
    impl &'static str
    impl &'static [u8]
    impl Box<[u8]>
    impl Cow<'static, str>
    impl Cow<'static, [u8]>
}

impl<const N: usize> From<&'static [u8; N]> for Response {
    fn from(value: &'static [u8; N]) -> Self {
        Response::new().with_body(value)
    }
}

impl<B> From<http::Response<B>> for Response
where
    B: http_body::Body<Data = bytes::Bytes> + Send + Sync + 'static,
    Box<dyn std::error::Error + Send + Sync + 'static>: From<B::Error>,
{
    fn from(value: http::Response<B>) -> Self {
        let (parts, body) = value.into_parts();
        Response {
            status: parts.status,
            version: parts.version,
            headers: parts.headers,
            extensions: parts.extensions,
            body: Body::from_body(body),
        }
    }
}

impl<L: Into<Response>, R: Into<Response>> From<either::Either<L, R>> for Response {
    fn from(value: either::Either<L, R>) -> Self {
        match value {
            either::Left(left) => left.into(),
            either::Right(right) => right.into(),
        }
    }
}

impl<T: Into<Response>, E: Into<Response>> From<Result<T, E>> for Response {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => value.into(),
            Err(value) => value.into(),
        }
    }
}

impl<B: Into<Response>, C: Into<Response>> From<ControlFlow<B, C>> for Response {
    fn from(value: ControlFlow<B, C>) -> Self {
        match value {
            ControlFlow::Break(value) => value.into(),
            ControlFlow::Continue(value) => value.into(),
        }
    }
}

impl From<()> for Response {
    fn from(_: ()) -> Self {
        Response::new()
    }
}

impl From<Infallible> for Response {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

impl From<Body> for Response {
    fn from(value: Body) -> Self {
        Response::new().with_body(value)
    }
}

impl From<body::BodyError> for Response {
    #[cfg(debug_assertions)]
    fn from(value: body::BodyError) -> Self {
        // TODO! fix this
        Response::internal_server_error().with_body(format!("{value:#?}"))
    }
    #[cfg(not(debug_assertions))]
    fn from(value: body::BodyError) -> Self {
        Response::internal_server_error()
    }
}

impl From<std::io::Error> for Response {
    fn from(value: std::io::Error) -> Self {
        let status = if value.kind() == std::io::ErrorKind::NotFound {
            http::StatusCode::NOT_FOUND
        } else {
            http::StatusCode::INTERNAL_SERVER_ERROR
        };
        #[cfg(debug_assertions)]
        return Response::status(status).with_body(format!("{value:#?}"));
        #[cfg(not(debug_assertions))]
        return Response::status(status);
    }
}

impl From<http::StatusCode> for Response {
    fn from(value: http::StatusCode) -> Self {
        Response::new().with_status(value)
    }
}
