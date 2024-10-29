use crate::{unit_error_expr, ReplyError, Request};
use bytes::Bytes;
use hyper::header::HeaderValue;

#[derive(Debug, Clone)]
pub struct SetCookie(pub(crate) HeaderValue);
pub struct SetCookieBuilder<'a> {
    name: &'a [u8],
    value: &'a [u8],
    path: Option<&'a [u8]>,
    domain: Option<&'a [u8]>,
    http_only: bool,
    secure: bool,
    max_age: Option<u32>,
}

pub fn set_cookie<'a>(name: &'a (impl AsRef<[u8]> + ?Sized)) -> SetCookieBuilder<'a> {
    SetCookieBuilder {
        name: name.as_ref(),
        value: b"",
        path: None,
        domain: None,
        http_only: false,
        secure: false,
        max_age: None,
    }
}

impl<'a> SetCookieBuilder<'a> {
    pub fn path(mut self, path: &'a (impl AsRef<[u8]> + ?Sized)) -> Self {
        self.path = Some(path.as_ref());
        self
    }
    pub fn domain(mut self, domain: &'a (impl AsRef<[u8]> + ?Sized)) -> Self {
        self.domain = Some(domain.as_ref());
        self
    }
    pub fn http_only(mut self) -> Self {
        self.http_only = true;
        self
    }
    pub fn secure(mut self) -> Self {
        self.secure = true;
        self
    }
    pub fn max_age(mut self, max_age: u32) -> Self {
        self.max_age = Some(max_age);
        self
    }
    #[track_caller]
    pub fn value(mut self, value: &'a (impl AsRef<[u8]> + ?Sized)) -> SetCookie {
        self.value = value.as_ref();
        if self.value.iter().any(|x| matches!(x, b'=' | b';')) {
            panic!("cookie value has invalid characters")
        }
        self.into_set_cookie()
    }
    #[track_caller]
    pub fn erase(mut self) -> SetCookie {
        self.max_age = Some(0);
        self.into_set_cookie()
    }
    #[track_caller]
    fn into_set_cookie(self) -> SetCookie {
        let mut set_cookie = Vec::new();

        set_cookie.extend_from_slice(self.name);
        set_cookie.push(b'=');
        set_cookie.extend_from_slice(self.value);

        set_cookie.extend_from_slice(b"; SameSite=Strict");

        if let Some(path) = self.path {
            set_cookie.extend_from_slice(b"; Path=");
            set_cookie.extend_from_slice(path);
        }

        if let Some(domain) = self.domain {
            set_cookie.extend_from_slice(b"; Domain=");
            set_cookie.extend_from_slice(domain);
        }

        if self.http_only {
            set_cookie.extend_from_slice(b"; HttpOnly");
        }

        if self.secure {
            set_cookie.extend_from_slice(b"; Secure");
        }

        if let Some(max_age) = self.max_age {
            set_cookie.extend_from_slice(b"; Max-Age=");
            set_cookie.extend_from_slice(max_age.to_string().as_bytes());
        }

        SetCookie(HeaderValue::from_maybe_shared(Bytes::from(set_cookie)).unwrap())
    }
}

impl Request {
    pub fn get_cookie<'a>(
        &'a self,
        name: &(impl AsRef<[u8]> + ?Sized),
    ) -> Result<&'a [u8], impl ReplyError> {
        let name = name.as_ref();
        self.headers
            .get_all(hyper::header::COOKIE)
            .iter()
            .find_map(|x| {
                for i in x.as_bytes().split(|x| *x == b';') {
                    let i = if i.starts_with(b" ") { &i[1..] } else { i };
                    if i.get(name.len()).copied() == Some(b'=') && i.starts_with(name) {
                        return Some(&i[name.len() + 1..]);
                    }
                }
                None
            })
            .ok_or(unit_error_expr!(CookieEsperado "cookie esperado"))
    }
}
