use crate::{
    body::Body, cookie, unit_error_expr, BodyError, ReplyError, Request, RequestBody, Response,
};
use bytes::Bytes;
use futures::FutureExt;
use hyper::{
    header::{self, HeaderName, HeaderValue},
    http, Method, StatusCode,
};
use std::future::Future;

impl Request {
    pub fn is_get(&self) -> bool {
        self.method == Method::GET
    }
    pub fn is_post(&self) -> bool {
        self.method == Method::POST
    }
    pub fn assert_get(&self) -> Result<(), impl ReplyError> {
        if self.is_get() {
            Ok(())
        } else {
            Err(unit_error_expr!(ExpectedGetMethod "expected GET method"))
        }
    }
    pub fn assert_post(&self) -> Result<(), impl ReplyError> {
        if self.is_post() {
            Ok(())
        } else {
            Err(unit_error_expr!(ExpectedPostMethod "expected POST method"))
        }
    }
    pub fn assert_json(&self) -> Result<(), impl ReplyError> {
        if self
            .headers
            .get(http::header::CONTENT_TYPE)
            .is_some_and(|x| x.as_bytes().starts_with(b"application/json"))
        {
            Ok(())
        } else {
            Err(unit_error_expr!(ExpectedJsonContentType "expected application/json content type"))
        }
    }
    pub fn assert_form(&self) -> Result<(), impl ReplyError> {
        if self
            .headers
            .get(http::header::CONTENT_TYPE)
            .is_some_and(|x| {
                x.as_bytes()
                    .starts_with(b"application/x-www-form-urlencoded")
            })
        {
            Ok(())
        } else {
            Err(
                unit_error_expr!(ExpectedFormContentType "expected application/x-www-form-urlencoded content type"),
            )
        }
    }
    #[track_caller]
    pub fn take_json<T: serde::de::DeserializeOwned>(
        &mut self,
    ) -> impl Future<Output = Result<T, BodyError>> + Send + Sync + 'static {
        match self.assert_json() {
            Ok(()) => self.body.take_json().left_future(),
            Err(error) => async { Err(Box::new(error).into()) }.right_future(),
        }
    }
    #[track_caller]
    pub fn take_form<T: serde::de::DeserializeOwned>(
        &mut self,
    ) -> impl Future<Output = Result<T, BodyError>> + Send + Sync + 'static {
        match self.assert_form() {
            Ok(()) => self.body.take_form().left_future(),
            Err(error) => async { Err(Box::new(error).into()) }.right_future(),
        }
    }
}

impl RequestBody {
    pub(crate) fn new(body: Body) -> Self {
        Self(Some(body))
    }
    #[track_caller]
    pub fn take_body(&mut self) -> Body {
        self.0.take().expect("body already taken")
    }
    #[track_caller]
    pub fn take_bytes<T: serde::de::DeserializeOwned>(
        &mut self,
    ) -> impl Future<Output = Result<Bytes, BodyError>> + Send + Sync + 'static {
        self.take_body().into_bytes()
    }
    #[track_caller]
    pub fn take_json<T: serde::de::DeserializeOwned>(
        &mut self,
    ) -> impl Future<Output = Result<T, BodyError>> + Send + Sync + 'static {
        let future = self.take_body().into_bytes();
        async move { serde_json::from_slice::<T>(&future.await?).map_err(From::from) }
    }
    #[track_caller]
    pub fn take_form<T: serde::de::DeserializeOwned>(
        &mut self,
    ) -> impl Future<Output = Result<T, BodyError>> + Send + Sync + 'static {
        let future = self.take_body().into_bytes();
        async move { serde_qs::from_bytes::<T>(&future.await?).map_err(From::from) }
    }
}

impl Response {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn with_status(mut self, status: http::StatusCode) -> Self {
        self.status = status;
        self
    }
    pub fn status(status: http::StatusCode) -> Self {
        Self::new().with_status(status)
    }
    pub fn with_body(mut self, body: impl Into<Body>) -> Self {
        self.body = body.into();
        self
    }
    pub fn body(body: impl Into<Body>) -> Self {
        Self::new().with_body(body)
    }
    pub fn with_header(mut self, header: HeaderName, value: HeaderValue) -> Self {
        self.headers.insert(header, value);
        self
    }
    pub fn header(header: HeaderName, value: HeaderValue) -> Self {
        Self::new().with_header(header, value)
    }
    /// `Set-Cookie: ...`
    pub fn with_cookie(mut self, set_cookie: cookie::SetCookie) -> Self {
        self.headers.append(header::SET_COOKIE, set_cookie.0);
        self
    }
    /// `Set-Cookie: ...`
    pub fn cookie(set_cookie: cookie::SetCookie) -> Self {
        Self::new().with_cookie(set_cookie)
    }

    /// 303 See Other
    #[track_caller]
    pub fn see_other(location: impl AsRef<[u8]>) -> Self {
        Self::status(StatusCode::SEE_OTHER).with_header(
            http::header::LOCATION,
            HeaderValue::from_bytes(location.as_ref()).unwrap(),
        )
    }

    /// 302 Found
    #[track_caller]
    pub fn found(location: impl AsRef<[u8]>) -> Self {
        Self::status(StatusCode::FOUND).with_header(
            http::header::LOCATION,
            HeaderValue::from_bytes(location.as_ref()).unwrap(),
        )
    }

    /// 400 Bad Request
    pub fn bad_request() -> Self {
        Self::status(StatusCode::BAD_REQUEST)
    }

    /// 401 Unauthorized
    pub fn unauthorized() -> Self {
        Self::status(StatusCode::UNAUTHORIZED)
    }

    /// 403 Forbidden
    pub fn forbidden() -> Self {
        Self::status(StatusCode::FORBIDDEN)
    }

    /// 404 Not Found
    pub fn not_found() -> Self {
        Self::status(StatusCode::NOT_FOUND)
    }

    /// 500 Internal Server Error
    pub fn internal_server_error() -> Self {
        Self::status(StatusCode::INTERNAL_SERVER_ERROR)
    }

    /// `text/plain; charset=utf-8`
    pub fn as_text(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/plain; charset=utf-8"),
        )
    }
    /// `text/plain; charset=utf-8`
    pub fn with_text(self, body: impl Into<Body>) -> Self {
        self.as_text().with_body(body.into())
    }
    /// `text/plain; charset=utf-8`
    pub fn text(body: impl Into<Body>) -> Self {
        Self::new().with_text(body)
    }
    /// `application/octet-stream`
    pub fn as_bin(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        )
    }
    /// `application/octet-stream`
    pub fn with_bin(self, body: impl Into<Body>) -> Self {
        self.as_bin().with_body(body.into())
    }
    /// `application/octet-stream`
    pub fn bin(body: impl Into<Body>) -> Self {
        Self::new().with_bin(body)
    }
    /// `text/html; charset=utf-8`
    pub fn as_html(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/html; charset=utf-8"),
        )
    }
    /// `text/html; charset=utf-8`
    pub fn with_html(self, body: impl Into<Body>) -> Self {
        self.as_html().with_body(body)
    }
    /// `text/html; charset=utf-8`
    pub fn html(body: impl Into<Body>) -> Self {
        Self::new().with_html(body)
    }
    /// `application/javascript; charset=utf-8`
    pub fn as_js(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/javascript; charset=utf-8"),
        )
    }
    /// `application/javascript; charset=utf-8`
    pub fn with_js(self, body: impl Into<Body>) -> Self {
        self.as_js().with_body(body)
    }
    /// `application/javascript; charset=utf-8`
    pub fn js(body: impl Into<Body>) -> Self {
        Self::new().with_js(body)
    }
    /// `text/css; charset=utf-8`
    pub fn as_css(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/css; charset=utf-8"),
        )
    }
    /// `text/css; charset=utf-8`
    pub fn css(body: impl Into<Body>) -> Self {
        Self::new().with_css(body)
    }
    /// `text/css; charset=utf-8`
    pub fn with_css(self, body: impl Into<Body>) -> Self {
        self.as_css().with_body(body)
    }
    /// `application/json; charset=utf-8`
    pub fn as_json(self) -> Self {
        self.with_header(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/json; charset=utf-8"),
        )
    }
    /// `application/json; charset=utf-8`
    pub fn with_json<T: serde::Serialize>(self, value: &T) -> Self {
        match serde_json::to_vec(value) {
            Ok(json) => self.as_json().with_body(json),
            Err(_) => self.with_status(http::StatusCode::INTERNAL_SERVER_ERROR),
        }
    }
    /// `application/json; charset=utf-8`
    pub fn json<T: serde::Serialize>(value: &T) -> Self {
        Self::new().with_json(value)
    }
}
