use hyper::{
    header::{HeaderName, HeaderValue},
    http, StatusCode,
};

use crate::{cookie, Body, WithExtra, Response};

impl<T> WithExtra for T
where
    Response: From<T>,
{
    fn with_status(self, status: StatusCode) -> WithStatus<Self> {
        WithStatus {
            inner: self,
            status,
        }
    }
    fn with_body<B>(self, body: B) -> WithBody<Self, B>
    where
        Body: From<B>,
    {
        WithBody { inner: self, body }
    }
    fn with_header(self, header: http::HeaderName, value: http::HeaderValue) -> WithHeader<Self> {
        WithHeader {
            inner: self,
            name: header,
            value,
        }
    }
    fn with_cookie(self, set_cookie: cookie::SetCookie) -> WithCookie<Self> {
        WithCookie {
            inner: self,
            set_cookie,
        }
    }
}

#[derive(Debug, Clone)]
pub struct WithStatus<T> {
    pub(crate) inner: T,
    pub(crate) status: StatusCode,
}

impl<T> From<WithStatus<T>> for Response
where
    Response: From<T>,
{
    fn from(value: WithStatus<T>) -> Self {
        Response::from(value.inner).with_status(value.status)
    }
}

#[derive(Debug, Clone)]
pub struct WithBody<T, B> {
    pub(crate) inner: T,
    pub(crate) body: B,
}

impl<T, B> From<WithBody<T, B>> for Response
where
    Response: From<T>,
    Body: From<B>,
{
    fn from(value: WithBody<T, B>) -> Self {
        Response::from(value.inner).with_body(value.body)
    }
}

#[derive(Debug, Clone)]
pub struct WithHeader<T> {
    pub(crate) inner: T,
    pub(crate) name: HeaderName,
    pub(crate) value: HeaderValue,
}

impl<T> From<WithHeader<T>> for Response
where
    Response: From<T>,
{
    fn from(value: WithHeader<T>) -> Self {
        Response::from(value.inner).with_header(value.name, value.value)
    }
}

#[derive(Debug, Clone)]
pub struct WithCookie<T> {
    pub(crate) inner: T,
    pub(crate) set_cookie: cookie::SetCookie,
}

impl<T> From<WithCookie<T>> for Response
where
    Response: From<T>,
{
    fn from(value: WithCookie<T>) -> Self {
        Response::from(value.inner).with_cookie(value.set_cookie)
    }
}
