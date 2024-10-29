use crate::{asset, convert::encode_path, Body, Response};
use bytes::Bytes;
use hyper::header::{self, HeaderValue};
use std::{
    path::Path,
    pin::Pin,
    task::{Context, Poll},
};
use tokio::fs::File as TokioFile;

impl Body {
    pub async fn file(filename: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        let file = tokio::fs::File::open(filename).await?;
        let metadata = file.metadata().await?;
        Ok(Self::from_body(TokioFileBody {
            file: Some(file),
            len: metadata.len(),
        }))
    }
    pub fn file_sync(filename: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        std::fs::read(filename).map(Self::from)
    }
}

pub enum PathOrDir<T> {
    Path(T),
    Dir(asset::Dir),
}
impl<T: AsRef<Path>> From<T> for PathOrDir<T> {
    fn from(value: T) -> Self {
        Self::Path(value)
    }
}
impl From<asset::Dir> for PathOrDir<&Path> {
    fn from(value: asset::Dir) -> Self {
        Self::Dir(value)
    }
}

impl Response {
    pub async fn dir<T: AsRef<Path>>(root: impl Into<PathOrDir<T>>, path: &[&[u8]]) -> Response {
        let root = match root.into() {
            PathOrDir::Dir(asset) => return Response::assets(asset, path),
            PathOrDir::Path(root) => root,
        };
        let segments = path;
        if segments.is_empty() {
            return Response::not_found();
        }
        let needs_index = segments.last().is_some_and(|x| x.is_empty());
        let mut path = root.as_ref().to_path_buf();
        for &segment in segments {
            if segment.is_empty() {
                continue;
            }
            match validate_segment(segment) {
                Some(segment) => path.push(segment),
                None => return Response::bad_request(),
            }
        }
        if needs_index {
            path.push("index.html");
        }
        let metadata = match tokio::fs::metadata(&path).await {
            Ok(metadata) => metadata,
            Err(error) => return error.into(),
        };
        if metadata.is_file() {
            match tokio::fs::File::open(&path).await {
                Ok(file) => Response::body(Body::from_body(TokioFileBody {
                    file: Some(file),
                    len: metadata.len(),
                }))
                .as_file(path),
                Err(error) => return error.into(),
            }
        } else if needs_index {
            Response::not_found()
        } else {
            if let Some(last) = segments.last() {
                Response::found(encode_path(&[*last, b""]))
            } else {
                Response::not_found()
            }
        }
    }
    pub fn dir_sync<T: AsRef<Path>>(root: impl Into<PathOrDir<T>>, segments: &[&[u8]]) -> Response {
        let root = match root.into() {
            PathOrDir::Dir(asset) => return Response::assets(asset, segments),
            PathOrDir::Path(root) => root,
        };
        if segments.is_empty() {
            return Response::not_found();
        }
        let needs_index = segments.last().is_some_and(|x| x.is_empty());
        let mut path = root.as_ref().to_path_buf();
        for &segment in segments {
            if segment.is_empty() {
                continue;
            }
            match validate_segment(segment) {
                Some(segment) => path.push(segment),
                None => return Response::bad_request(),
            }
        }
        if needs_index {
            path.push("index.html");
        }
        let metadata = match std::fs::metadata(&path) {
            Ok(metadata) => metadata,
            Err(error) => return error.into(),
        };
        if metadata.is_file() {
            match std::fs::read(&path) {
                Ok(file) => Response::body(file).as_file(path),
                Err(error) => return error.into(),
            }
        } else if needs_index {
            Response::not_found()
        } else {
            if let Some(last) = segments.last() {
                Response::found(encode_path(&[*last, b""]))
            } else {
                Response::not_found()
            }
        }
    }

    pub fn assets(mut asset: asset::Dir, path: &[&[u8]]) -> Response {
        let segments = path;
        if segments.is_empty() {
            return Response::not_found();
        }
        for &segment in &segments[..segments.len() - 1] {
            if segment.is_empty() {
                continue;
            }
            match validate_segment(segment) {
                Some(segment) => match asset.get_dir(segment) {
                    Some(sub_diretory) => asset = sub_diretory,
                    None => return Response::not_found(),
                },
                None => return Response::bad_request(),
            }
        }
        let filename = match segments.last() {
            Some(&filename) if !filename.is_empty() => match std::str::from_utf8(filename) {
                Ok(filename) => filename,
                Err(_) => return Response::bad_request(),
            },
            _ => "index.html",
        };
        match asset.get_file(filename) {
            Some(file) => Response::asset(file),
            None => return Response::not_found(),
        }
    }

    pub async fn file(filename: impl AsRef<Path>) -> Self {
        Self::new().with_file(filename).await
    }
    pub fn as_file(self, filename: impl AsRef<Path>) -> Self {
        match mime_guess::from_path(filename).first() {
            Some(mime) => self.with_header(
                header::CONTENT_TYPE,
                HeaderValue::from_maybe_shared(Bytes::from(mime.to_string())).unwrap(),
            ),
            None => self,
        }
    }
    pub async fn with_file(self, filename: impl AsRef<Path>) -> Self {
        match Body::file(filename.as_ref()).await {
            Ok(body) => self.as_file(filename).with_body(body),
            Err(error) => error.into(),
        }
    }

    pub fn file_sync(filename: impl AsRef<Path>) -> Self {
        Self::new().with_file_sync(filename)
    }
    pub fn with_file_sync(self, filename: impl AsRef<Path>) -> Self {
        match Body::file_sync(filename.as_ref()) {
            Ok(body) => self.as_file(filename).with_body(body),
            Err(error) => error.into(),
        }
    }

    pub fn asset(asset: asset::File) -> Self {
        Self::new().with_asset(asset)
    }
    pub fn as_asset(self, asset: asset::File) -> Self {
        match asset.mime {
            Some(mime) => self.with_header(header::CONTENT_TYPE, HeaderValue::from_static(mime)),
            None => self,
        }
    }
    pub fn with_asset(self, asset: asset::File) -> Self {
        self.with_body(asset.bytes)
    }
}

struct TokioFileBody {
    file: Option<TokioFile>,
    len: u64,
}

impl http_body::Body for TokioFileBody {
    type Data = bytes::Bytes;
    type Error = std::io::Error;
    fn poll_frame(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<http_body::Frame<Self::Data>, Self::Error>>> {
        if self.len == 0 {
            return Poll::Ready(None);
        }
        let project = unsafe { self.get_unchecked_mut() };
        let Some(file) = &mut project.file else {
            return Poll::Ready(None);
        };
        let mut buf = bytes::BytesMut::with_capacity(1024 * 8);
        let poll = tokio_util::io::poll_read_buf(std::pin::Pin::new(file), cx, &mut buf);
        let readlen = match poll {
            Poll::Ready(Ok(len)) => len as u64,
            Poll::Ready(Err(error)) => return Poll::Ready(Some(Err(error))),
            Poll::Pending => return Poll::Pending,
        };
        if readlen == 0 {
            return Poll::Ready(None);
        }
        let mut chunk = buf.split().freeze();
        if readlen > project.len {
            chunk = chunk.split_to(project.len as usize);
            project.len = 0;
        } else {
            project.len -= readlen;
        }
        Poll::Ready(Some(Ok(http_body::Frame::data(chunk))))
    }
    fn is_end_stream(&self) -> bool {
        self.len == 0
    }
    fn size_hint(&self) -> http_body::SizeHint {
        http_body::SizeHint::with_exact(self.len)
    }
}

fn validate_segment(segment: &[u8]) -> Option<&str> {
    let text = Some(segment)
        .filter(|x| !x.is_empty()) // must not be empty
        .filter(|x| !forbidden_segment_name(x)) // must be not forbidden ".."
        .filter(|x| x.iter().copied().all(safe_path_char)) // must be all safe characters
        .and_then(|x| std::str::from_utf8(x).ok())?; // must be valid utf8

    let path = Path::new(text);
    // must be a relative path
    if !path.is_relative() {
        return None;
    }
    // must have exactly one segment
    if !path.parent().is_some_and(|x| x == Path::new("")) {
        return None;
    }
    Some(text)
}

#[cfg(windows)]
const fn forbidden_segment_name(x: &[u8]) -> bool {
    matches!(
        x,
        b".." | [b'C' | b'c', b'O' | b'o', b'M' | b'm', b'0'..=b'9']
    )
}

#[cfg(windows)]
const fn safe_path_char(c: u8) -> bool {
    !c.is_ascii_control()
        && !matches!(
            c,
            b'\\' | b'/' | b'?' | b'*' | b'\"' | b':' | b'<' | b'>' | b'|'
        )
}

#[cfg(not(windows))]
const fn forbidden_segment_name(x: &[u8]) -> bool {
    matches!(x, b"..")
}

#[cfg(not(windows))]
const fn safe_path_char(c: u8) -> bool {
    !matches!(c, b'\x00' | b'/')
}
