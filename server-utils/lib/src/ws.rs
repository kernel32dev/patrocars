use std::{fmt::Debug, future::Future};

use hyper::StatusCode;
use hyper_tungstenite::tungstenite::error::ProtocolError;

use crate::{Request, Response};

pub use hyper_tungstenite::HyperWebsocket as WebSocket;
pub use hyper_tungstenite::tungstenite::Message as Message;
pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

impl From<ProtocolError> for Response {
    #[cfg(debug_assertions)]
    fn from(value: ProtocolError) -> Self {
        Self::status(StatusCode::BAD_REQUEST).with_text(format!("{value:#?}"))
    }
    #[cfg(not(debug_assertions))]
    fn from(value: ProtocolError) -> Self {
        Self::status(StatusCode::BAD_REQUEST)
    }
}

impl Request {
    pub fn is_web_socket(&self) -> bool {
        header_contains_value(&self.headers, hyper::header::CONNECTION, "Upgrade")
            && header_contains_value(&self.headers, hyper::header::UPGRADE, "websocket")
    }
    pub fn upgrade<T, U, E>(&mut self, handler: T) -> Response
    where
        T: for<'a> FnOnce(WebSocket) -> U,
        U: Future<Output = Result<(), E>> + Send + 'static,
        E: Debug,
    {
        let Self {
            method,
            uri,
            path: _,
            version,
            headers,
            extensions,
            body,
        } = self;
        let mut request = hyper::http::Request::new(std::mem::take(body));
        *request.method_mut() = std::mem::take(method);
        *request.uri_mut() = std::mem::take(uri);
        *request.version_mut() = std::mem::take(version);
        *request.headers_mut() = std::mem::take(headers);
        *request.extensions_mut() = std::mem::take(extensions);
        match hyper_tungstenite::upgrade(request, None) {
            Ok((response, socket)) => {
                let future = handler(socket);
                tokio::task::spawn(async move {
                    match future.await {
                        Ok(()) => {}
                        Err(error) => {
                            log::error!("websocket error: {error:#?}");
                        }
                    }
                });
                Response::from(response)
            }
            Err(error) => Response::from(error),
        }
    }
}

fn header_contains_value(
    headers: &hyper::HeaderMap,
    header: impl hyper::header::AsHeaderName,
    value: impl AsRef<[u8]>,
) -> bool {
    let value = value.as_ref();
    for header in headers.get_all(header) {
        if header
            .as_bytes()
            .split(|&c| c == b',')
            .any(|x| trim(x).eq_ignore_ascii_case(value))
        {
            return true;
        }
    }
    false
}

fn trim(data: &[u8]) -> &[u8] {
    trim_end(trim_start(data))
}

fn trim_start(data: &[u8]) -> &[u8] {
    if let Some(start) = data.iter().position(|x| !x.is_ascii_whitespace()) {
        &data[start..]
    } else {
        b""
    }
}

fn trim_end(data: &[u8]) -> &[u8] {
    if let Some(last) = data.iter().rposition(|x| !x.is_ascii_whitespace()) {
        &data[..last + 1]
    } else {
        b""
    }
}
