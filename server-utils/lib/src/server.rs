use std::borrow::Cow;
use std::future::Future;
use std::net::Ipv4Addr;
use std::task::Poll;

use crate::body::Body;
use crate::log::LOGGER_INITIALIZED;
use crate::Response;
use bytes::Bytes;
use futures::FutureExt;
use hyper::body::Incoming;
use hyper::{http, StatusCode};
use tokio::sync::RwLock;

pub fn setup<F1, F2, F3>(
    logic: F1,
) -> Server<F1, impl tokio::net::ToSocketAddrs + Send, impl std::future::Future<Output = ()>>
where
    F1: Fn(crate::Request) -> F2 + Send + Sync + 'static,
    F2: Future<Output = F3> + Send + 'static,
    Response: From<F3>,
{
    Server {
        logic,
        addr: (Ipv4Addr::LOCALHOST, 8080),
        shutdown: std::future::pending(),
        with_upgrade: false,
    }
}

pub struct Server<F, A, S> {
    logic: F,
    addr: A,
    shutdown: S,
    with_upgrade: bool,
}

impl<F, A, S> Server<F, A, S> {
    pub fn addr<T>(self, addr: T) -> Server<F, T, S> {
        let Self {
            logic,
            addr: _,
            shutdown,
            with_upgrade,
        } = self;
        Server {
            logic,
            addr,
            shutdown,
            with_upgrade,
        }
    }
    pub fn shutdown<T>(self, shutdown: T) -> Server<F, A, T> {
        let Self {
            logic,
            addr,
            shutdown: _,
            with_upgrade,
        } = self;
        Server {
            logic,
            addr,
            shutdown,
            with_upgrade,
        }
    }
    pub fn shutdown_on_ctrl_c(self) -> Server<F, A, impl std::future::Future<Output = ()>> {
        let Self {
            logic,
            addr,
            shutdown: _,
            with_upgrade,
        } = self;
        let shutdown = async {
            match tokio::signal::ctrl_c().await {
                Ok(()) => {}
                Err(_) => std::future::pending().await,
            }
        };
        Server {
            logic,
            addr,
            shutdown,
            with_upgrade,
        }
    }
    pub fn shutdown_on_ctrl_c_or_panic(
        self,
    ) -> Server<F, A, impl std::future::Future<Output = ()>> {
        let Self {
            logic,
            addr,
            shutdown: _,
            with_upgrade,
        } = self;
        let shutdown = async {
            match tokio::signal::ctrl_c().await {
                Ok(()) => {}
                Err(error) => panic!("tokio::signal::ctrl_c() failed: {error:#?}"),
            }
        };
        Server {
            logic,
            addr,
            shutdown,
            with_upgrade,
        }
    }
    pub fn with_upgrade(mut self) -> Self {
        self.with_upgrade = true;
        self
    }
    pub fn enable_websocket(self) -> Self {
        self.with_upgrade()
    }

    pub async fn serve<F2, F3>(self) -> std::io::Result<()>
    where
        F: Fn(crate::Request) -> F2 + Send + Sync + 'static,
        F2: Future<Output = F3> + Send + 'static,
        Response: From<F3>,
        A: tokio::net::ToSocketAddrs + Send,
        S: std::future::Future<Output = ()>,
    {
        use hyper_util::rt::TokioExecutor;
        use hyper_util::server::conn::auto::Builder;
        use std::convert::Infallible;
        use std::sync::Arc;
        use tokio::net::TcpListener;

        let Self {
            logic,
            addr,
            shutdown,
            with_upgrade,
        } = self;

        let listener = TcpListener::bind(addr).await?;
        let builder = Builder::new(TokioExecutor::new());
        let logic = Arc::new(logic);

        let service = hyper::service::service_fn(move |req: http::Request<Incoming>| {
            let Ok(path) = crate::path::UrlPath::new(req.uri().path().as_bytes()) else {
                return async {
                    #[cfg(debug_assertions)]
                    let mut res = http::Response::new(Body::from(
                        "path in the url contains invalid escapes sequences",
                    ));
                    #[cfg(not(debug_assertions))]
                    let mut res = http::Response::default();
                    *res.status_mut() = StatusCode::BAD_REQUEST;
                    Ok::<_, Infallible>(res)
                }
                .left_future();
            };
            let logic = logic.clone();
            let (parts, incoming) = req.into_parts();
            let req = crate::Request {
                method: parts.method,
                uri: parts.uri,
                path,
                version: parts.version,
                headers: parts.headers,
                extensions: parts.extensions,
                body: crate::RequestBody::new(Body::from_body(incoming)),
            };
            let future = (*logic)(req);
            async move {
                let mut future = std::pin::pin!(future);
                let result = if LOGGER_INITIALIZED.load(std::sync::atomic::Ordering::Relaxed) {
                    let mut logs = String::new();
                    std::future::poll_fn(|cx| {
                        crate::log::capture(&mut logs, || {
                            catch_unwind_pool(std::panic::AssertUnwindSafe(|| {
                                future.as_mut().poll(cx)
                            }))
                        })
                    })
                    .await
                    .map(Response::from)
                    .map(|mut x| {
                        x.headers.insert(
                            http::header::HeaderName::from_static("x-log"),
                            http::HeaderValue::from_maybe_shared(Bytes::from(encode_json_string(
                                &logs,
                            )))
                            .expect("json from encode_json_string is always a valid header value"),
                        );
                        x
                    })
                } else {
                    std::future::poll_fn(|cx| {
                        catch_unwind_pool(std::panic::AssertUnwindSafe(|| future.as_mut().poll(cx)))
                    })
                    .await
                    .map(Response::from)
                };
                let res = result.unwrap_or_else(|panic| {
                    if cfg!(debug_assertions) {
                        crate::Response::new()
                            .with_status(StatusCode::INTERNAL_SERVER_ERROR)
                            .with_body(match panic {
                                Cow::Borrowed(borrowed) => Bytes::from(borrowed),
                                Cow::Owned(owned) => Bytes::from(owned),
                            })
                    } else {
                        crate::Response::new().with_status(StatusCode::INTERNAL_SERVER_ERROR)
                    }
                });
                let crate::Response {
                    status,
                    version,
                    headers,
                    extensions,
                    body,
                } = res;
                let mut res = http::Response::new(body);
                *res.status_mut() = status;
                *res.version_mut() = version;
                *res.headers_mut() = headers;
                *res.extensions_mut() = extensions;
                Ok::<_, Infallible>(res)
            }
            .right_future()
        });

        let lock = Arc::new(RwLock::new(()));
        let mut shutdown = std::pin::pin!(shutdown);
        while let Some(Ok((tcp, _))) = tokio::select! {
            x = listener.accept() => Some(x),
            () = &mut shutdown => None,
        } {
            let builder = builder.clone();
            let service = service.clone();
            let lock = lock.clone();
            if with_upgrade {
                tokio::spawn(async move {
                    let lock = lock.try_read().unwrap();
                    let io = hyper_util::rt::TokioIo::new(tcp);
                    let _ = builder.serve_connection_with_upgrades(io, service).await;
                    drop(lock);
                });
            } else {
                tokio::spawn(async move {
                    let lock = lock.try_read().unwrap();
                    let io = hyper_util::rt::TokioIo::new(tcp);
                    let _ = builder.serve_connection(io, service).await;
                    drop(lock);
                });
            }
        }
        // wait for all connections to settle
        drop(lock.write().await);
        Ok(())
    }
}
pub fn catch_unwind_pool<T>(
    f: impl FnOnce() -> Poll<T> + std::panic::UnwindSafe,
) -> Poll<Result<T, Cow<'static, str>>> {
    match std::panic::catch_unwind(f) {
        Ok(Poll::Ready(ready)) => Poll::Ready(Ok(ready)),
        Ok(Poll::Pending) => Poll::Pending,
        Err(error) => Poll::Ready(Err(error
            .downcast::<&'static str>()
            .map(|x| Cow::Borrowed(*x))
            .or_else(|x| x.downcast::<String>().map(|x| Cow::Owned(*x)))
            .or_else(|x| x.downcast::<Cow<'static, str>>().map(|x| *x))
            .unwrap_or_else(|_| Cow::Borrowed("Box<?>")))),
    }
}

fn encode_json_string(s: &str) -> String {
    let mut escaped = String::new();
    escaped.push('"');
    for c in s.chars() {
        match c {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '/' => escaped.push_str("\\/"),
            '\u{08}' => escaped.push_str("\\b"),
            '\u{0C}' => escaped.push_str("\\f"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            c if c.is_ascii_graphic() || c == ' ' => escaped.push(c),
            c if !c.is_ascii() || c.is_ascii_control() || c == '\u{7F}' => {
                if c <= '\u{FFFF}' {
                    escaped.push_str(&format!("\\u{:04x}", c as u32))
                } else {
                    let mut utf16 = [0; 2];
                    c.encode_utf16(&mut utf16);
                    escaped.push_str(&format!("\\u{:04x}\\u{:04x}", utf16[0], utf16[1]));
                }
            }
            _ => escaped.push(c),
        }
    }
    escaped.push('"');
    escaped
}
