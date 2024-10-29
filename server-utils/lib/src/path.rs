use crate::{convert, unit_error_struct, Request};

#[derive(Clone)]
pub struct UrlPath {
    _decoded: Vec<u8>,
    // invariant, these slices are only valid until DecodedPath._decoded is dropped
    slices: Vec<&'static [u8]>,
}

impl UrlPath {
    pub fn new(path: &[u8]) -> Result<Self, PathError> {
        let mut bytes = match &path[..] {
            [] | [b'/'] => {
                return Ok(Self {
                    _decoded: Vec::new(),
                    slices: vec![b""],
                });
            }
            [b'/', ..] => path[1..].iter().copied(),
            _ => path.iter().copied(),
        };
        let mut decoded = Vec::with_capacity(path.len());
        let mut segments = Vec::with_capacity(path.iter().filter(|x| **x == b'/').count());
        while let Some(c) = bytes.next() {
            match c {
                b'/' => segments.push(decoded.len()),
                b'%' => {
                    let a = match bytes.next() {
                        Some(a @ b'0'..=b'9') => a - b'0',
                        Some(a @ b'A'..=b'F') => a + 10 - b'A',
                        _ => return Err(PathError),
                    };
                    let b = match bytes.next() {
                        Some(b @ b'0'..=b'9') => b - b'0',
                        Some(b @ b'A'..=b'F') => b + 10 - b'A',
                        _ => return Err(PathError),
                    };
                    decoded.push(a * 16 + b);
                }
                b'+' => decoded.push(b' '),
                x => decoded.push(x),
            }
        }
        let slices = std::iter::once(0)
            .chain(segments.iter().copied())
            .zip(
                segments
                    .iter()
                    .copied()
                    .chain(std::iter::once(decoded.len())),
            )
            .map(|(start, end)| {
                let ptr = decoded[start..end].as_ptr();
                // SAFETY: no methods expose the static lifetime of the slices
                // all methods limit the lifetime
                unsafe { std::slice::from_raw_parts::<'static, u8>(ptr, end - start) }
            })
            .collect();
        Ok(Self {
            _decoded: decoded,
            slices,
        })
    }
    pub fn is_empty(&self) -> bool {
        self.slices.is_empty()
    }
    pub fn len(&self) -> usize {
        self.slices.len()
    }
    pub fn get<'a>(&'a self, index: usize) -> Option<&'a [u8]> {
        self.slices.get(index).copied()
    }
    pub fn as_slice<'a>(&'a self) -> &'a [&'a [u8]] {
        &self.slices
    }
}

impl<'a> IntoIterator for &'a UrlPath {
    type IntoIter = std::iter::Copied<std::slice::Iter<'a, &'a [u8]>>;
    type Item = &'a [u8];
    fn into_iter(self) -> Self::IntoIter {
        self.slices.iter().copied()
    }
}

impl std::ops::Index<usize> for UrlPath {
    type Output = [u8];
    fn index(&self, index: usize) -> &Self::Output {
        &self.slices[index]
    }
}

impl std::fmt::Debug for UrlPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            f.write_str("\"/\"")
        } else {
            f.write_str("\"")?;
            for i in self {
                f.write_str("/")?;
                convert::fmt_quoted_bytes(i, f, Some('"'))?;
            }
            f.write_str("\"")
        }
    }
}

unit_error_struct! {pub PathError "error decoding path, invalid escape"}

impl Request {
    /// `req.path.as_slice()`
    pub fn path(&self) -> &[&[u8]] {
        self.path.as_slice()
    }
}
