use bytes::{BufMut, Bytes};
use std::{
    borrow::Cow,
    pin::Pin,
    task::{Context, Poll},
};

/// concrete body implementation for pact
///
/// used by both requests and responses
#[derive(Default)]
pub struct Body(BodyKind)
where
    Self: Send + Sync;

pub type BodyError = Box<dyn std::error::Error + Send + Sync + 'static>;

type BoxedBody = std::pin::Pin<
    Box<dyn http_body::Body<Data = bytes::Bytes, Error = BodyError> + Send + Sync + 'static>,
>;

#[derive(Default)]
enum BodyKind {
    #[default]
    Empty,
    /// Invariant: bytes is not empty
    Bytes(bytes::Bytes),
    Incoming(hyper::body::Incoming),
    Boxed(BoxedBody),
}

impl From<Bytes> for Body {
    fn from(value: Bytes) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl From<&'static [u8]> for Body {
    fn from(value: &'static [u8]) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl<const N: usize> From<&'static [u8; N]> for Body {
    fn from(value: &'static [u8; N]) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value as &[u8])))
        }
    }
}
impl From<&'static str> for Body {
    fn from(value: &'static str) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl From<Vec<u8>> for Body {
    fn from(value: Vec<u8>) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl From<Box<[u8]>> for Body {
    fn from(value: Box<[u8]>) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl From<String> for Body {
    fn from(value: String) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(BodyKind::Bytes(Bytes::from(value)))
        }
    }
}
impl From<Cow<'static, [u8]>> for Body {
    fn from(value: Cow<'static, [u8]>) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            match value {
                Cow::Borrowed(borrowed) => Self(BodyKind::Bytes(Bytes::from(borrowed))),
                Cow::Owned(owned) => Self(BodyKind::Bytes(Bytes::from(owned))),
            }
        }
    }
}
impl From<Cow<'static, str>> for Body {
    fn from(value: Cow<'static, str>) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            match value {
                Cow::Borrowed(borrowed) => Self(BodyKind::Bytes(Bytes::from(borrowed))),
                Cow::Owned(owned) => Self(BodyKind::Bytes(Bytes::from(owned))),
            }
        }
    }
}

impl Body {
    pub fn from_body<B>(value: B) -> Self
    where
        B: http_body::Body<Data = bytes::Bytes> + Send + Sync + 'static,
        Box<dyn std::error::Error + Send + Sync + 'static>: From<B::Error>,
    {
        let kind = try_safe_transmute(value)
            .or_else(|x| try_safe_transmute(x).map(BodyKind::Incoming))
            .or_else(|x| try_safe_transmute(x).map(BodyKind::Boxed))
            .or_else(|x| try_safe_transmute(x).map(|x: ErasedBody<B>| BodyKind::Boxed(Box::pin(x))))
            .unwrap_or_else(|x| BodyKind::Boxed(Box::pin(ErasedBody(x))));
        Self(kind)
    }
    pub async fn into_bytes(self) -> Result<bytes::Bytes, BodyError> {
        match self.0 {
            BodyKind::Empty => return Ok(bytes::Bytes::new()),
            BodyKind::Bytes(bytes) => return Ok(bytes),
            BodyKind::Incoming(_) | BodyKind::Boxed(_) => {}
        }
        let mut buf = bytes::BytesMut::with_capacity(
            http_body::Body::size_hint(&self).lower().min(1024 * 16) as usize,
        );
        let mut body = std::pin::pin!(self);
        while let Some(result) =
            std::future::poll_fn(|cx| http_body::Body::poll_frame(body.as_mut(), cx)).await
        {
            if let Ok(data) = result?.into_data() {
                buf.put(data);
            }
        }
        Ok(buf.freeze())
    }
    pub async fn next_bytes(&mut self) -> Result<Option<bytes::Bytes>, BodyError> {
        match &mut self.0 {
            BodyKind::Empty => return Ok(None),
            BodyKind::Bytes(bytes) => {
                let bytes = std::mem::take(bytes);
                self.0 = BodyKind::Empty;
                return Ok(Some(bytes));
            }
            BodyKind::Incoming(_) | BodyKind::Boxed(_) => {}
        }
        let mut body = std::pin::Pin::new(self);
        while let Some(result) =
            std::future::poll_fn(|cx| http_body::Body::poll_frame(body.as_mut(), cx)).await
        {
            if let Ok(data) = result?.into_data() {
                if !data.is_empty() {
                    return Ok(Some(data));
                }
            }
        }
        Ok(None)
    }
}

impl http_body::Body for Body {
    type Data = bytes::Bytes;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<http_body::Frame<Self::Data>, Self::Error>>> {
        match &mut self.0 {
            BodyKind::Empty => Poll::Ready(None),
            BodyKind::Bytes(_) => {
                let BodyKind::Bytes(bytes) = std::mem::take(&mut self.0) else {
                    unreachable!()
                };
                Poll::Ready(Some(Ok(http_body::Frame::data(bytes))))
            }
            BodyKind::Incoming(incoming) => Pin::new(incoming).poll_frame(cx).map_err(From::from),
            BodyKind::Boxed(boxed) => boxed.as_mut().poll_frame(cx),
        }
    }

    fn is_end_stream(&self) -> bool {
        match &self.0 {
            BodyKind::Empty => true,
            BodyKind::Bytes(_) => false,
            BodyKind::Incoming(incoming) => incoming.is_end_stream(),
            BodyKind::Boxed(boxed) => boxed.is_end_stream(),
        }
    }

    fn size_hint(&self) -> http_body::SizeHint {
        match &self.0 {
            BodyKind::Empty => http_body::SizeHint::with_exact(0),
            BodyKind::Bytes(bytes) => http_body::SizeHint::with_exact(bytes.len() as u64),
            BodyKind::Incoming(incoming) => incoming.size_hint(),
            BodyKind::Boxed(boxed) => boxed.size_hint(),
        }
    }
}

impl futures::stream::Stream for Body {
    type Item = Result<<Self as http_body::Body>::Data, <Self as http_body::Body>::Error>;
    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        loop {
            match http_body::Body::poll_frame(self.as_mut(), cx) {
                Poll::Ready(Some(Ok(frame))) => {
                    if let Ok(data) = frame.into_data() {
                        return Poll::Ready(Some(Ok(data)));
                    } else {
                        continue;
                    }
                }
                Poll::Ready(Some(Err(error))) => return Poll::Ready(Some(Err(error))),
                Poll::Ready(None) => return Poll::Ready(None),
                Poll::Pending => return Poll::Pending,
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        if http_body::Body::is_end_stream(self) {
            (0, Some(0))
        } else {
            (1, None)
        }
    }
}

impl std::fmt::Debug for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            BodyKind::Empty => f.write_str("Body::Empty"),
            BodyKind::Bytes(bytes) => f.debug_tuple("Body::Bytes").field(bytes).finish(),
            BodyKind::Incoming(incoming) => {
                f.debug_tuple("Body::Incoming").field(incoming).finish()
            }
            BodyKind::Boxed(boxed) => f
                .debug_tuple("Body::Boxed")
                .field(&boxed.size_hint())
                .finish(),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
struct ErasedBody<B>(B);
impl<B> http_body::Body for ErasedBody<B>
where
    B: http_body::Body<Data = bytes::Bytes> + Send + Sync + 'static,
    Box<dyn std::error::Error + Send + Sync + 'static>: From<B::Error>,
{
    type Data = B::Data;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    fn poll_frame(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Result<http_body::Frame<Self::Data>, Self::Error>>> {
        let poll = unsafe {
            std::mem::transmute::<std::pin::Pin<&mut Self>, std::pin::Pin<&mut B>>(self)
                .poll_frame(cx)
        };
        match poll {
            std::task::Poll::Ready(Some(Err(error))) => {
                std::task::Poll::Ready(Some(Err(<Box<
                    dyn std::error::Error + Send + Sync + 'static,
                > as From<B::Error>>::from(error))))
            }
            std::task::Poll::Ready(Some(Ok(frame))) => std::task::Poll::Ready(Some(Ok(frame))),
            std::task::Poll::Ready(None) => std::task::Poll::Ready(None),
            std::task::Poll::Pending => std::task::Poll::Pending,
        }
    }
    fn is_end_stream(&self) -> bool {
        self.0.is_end_stream()
    }
    fn size_hint(&self) -> http_body::SizeHint {
        self.0.size_hint()
    }
}

/// transmutes from `From` to `To` if `From` and `To` are the same type
fn try_safe_transmute<From: 'static, To: 'static>(from: From) -> Result<To, From> {
    if std::any::TypeId::of::<From>() == std::any::TypeId::of::<To>() {
        let transmuted = unsafe { std::mem::transmute_copy::<From, To>(&from) };
        std::mem::forget(from);
        Ok(transmuted)
    } else {
        Err(from)
    }
}
