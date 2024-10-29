use std::{fmt::Debug, ops::Range as StdRange};

#[derive(Clone, Copy)]
pub struct Source {
    from: SourcePtr,
    to: SourcePtr,
}
pub trait HasSource {
    fn source(&self) -> Source;
}
impl HasSource for Source {
    fn source(&self) -> Source {
        *self
    }
}
impl HasSource for [u8] {
    fn source(&self) -> Source {
        let StdRange { start, end } = self.as_ptr_range();
        Source {
            from: SourcePtr::some(start),
            to: SourcePtr::some(end),
        }
    }
}
impl HasSource for str {
    fn source(&self) -> Source {
        self.as_bytes().source()
    }
}
impl<T: HasSource + ?Sized> HasSource for &T {
    fn source(&self) -> Source {
        (*self).source()
    }
}
impl<T: HasSource> HasSource for [T] {
    fn source(&self) -> Source {
        match self {
            [] => Default::default(),
            [one] => one.source(),
            [first, .., last] => first.source() + last.source(),
        }
    }
}
impl<T: HasSource, const N: usize> HasSource for [T; N] {
    fn source(&self) -> Source {
        self[..].source()
    }
}
impl<T: HasSource> HasSource for (T,) {
    fn source(&self) -> Source {
        self.0.source()
    }
}
impl<F: HasSource, L: HasSource> HasSource for (F, L) {
    fn source(&self) -> Source {
        let (first, last) = self;
        first.source() + last.source()
    }
}
impl<F: HasSource, T0, L: HasSource> HasSource for (F, T0, L) {
    fn source(&self) -> Source {
        let (first, .., last) = self;
        first.source() + last.source()
    }
}
impl<F: HasSource, T0, T1, L: HasSource> HasSource for (F, T0, T1, L) {
    fn source(&self) -> Source {
        let (first, .., last) = self;
        first.source() + last.source()
    }
}
impl<F: HasSource, T0, T1, T2, L: HasSource> HasSource for (F, T0, T1, T2, L) {
    fn source(&self) -> Source {
        let (first, .., last) = self;
        first.source() + last.source()
    }
}
impl<F: HasSource, T0, T1, T2, T3, L: HasSource> HasSource for (F, T0, T1, T2, T3, L) {
    fn source(&self) -> Source {
        let (first, .., last) = self;
        first.source() + last.source()
    }
}
impl<F: HasSource, T0, T1, T2, T3, T4, L: HasSource> HasSource for (F, T0, T1, T2, T3, T4, L) {
    fn source(&self) -> Source {
        let (first, .., last) = self;
        first.source() + last.source()
    }
}
impl<T: HasSource> HasSource for Option<T> {
    fn source(&self) -> Source {
        match self {
            Some(x) => x.source(),
            None => Source::nowhere(),
        }
    }
}
impl<T: HasSource, E: HasSource> HasSource for Result<T, E> {
    fn source(&self) -> Source {
        match self {
            Ok(ok) => ok.source(),
            Err(err) => err.source(),
        }
    }
}

impl Source {
    /// creates a range that points nowhere
    ///
    /// when added to any range, it becomes the other range
    pub const fn nowhere() -> Self {
        Self {
            from: SourcePtr::null(),
            to: SourcePtr::null(),
        }
    }
    /// creates a range that points to the end of the file
    pub const fn end() -> Self {
        Self {
            from: SourcePtr::none(),
            to: SourcePtr::none(),
        }
    }
    pub fn infer_range_str(self, whole: &str) -> Option<std::ops::Range<usize>> {
        let range = Self::infer_range_bytes(self, whole.as_bytes())?;
        whole.get(range.clone())?;
        Some(range)
    }
    pub fn infer_range_bytes(self, whole: &[u8]) -> Option<std::ops::Range<usize>> {
        Self::infer_range_ptrs(self, whole.as_ptr_range())
    }
    pub fn infer_range_ptrs(self, whole: std::ops::Range<*const u8>) -> Option<std::ops::Range<usize>> {
        let std::ops::Range { start, end } = whole;
        let Self { from, to } = self;
        let from = from.into_option().unwrap_or(end);
        let to = to.into_option().unwrap_or(end);
        if from.is_null() || from < start || from > end {
            return None;
        }
        let start_index = unsafe { from.offset_from(start) as usize };
        if to.is_null() {
            Some(start_index..start_index)
        } else if to < start || to > end {
            None
        } else if to <= from {
            Some(start_index..start_index)
        } else {
            let end_index = unsafe { to.offset_from(start) as usize };
            Some(start_index..end_index)
        }
    }
}
impl Debug for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Range(...)")
    }
}
impl Default for Source {
    fn default() -> Self {
        Self::nowhere()
    }
}
impl<T: HasSource + ?Sized> From<&T> for Source {
    fn from(value: &T) -> Self {
        value.source()
    }
}
impl std::ops::Add for Source {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            from: if self.from.0.is_null() {
                rhs.from
            } else {
                self.from
            },
            to: if rhs.to.0.is_null() {
                self.to
            } else {
                rhs.to
            },
        }
    }
}

/// a somewhat unsafe and more compact way of storing a `Option<*const u8>`
///
/// stores `None` as `std::ptr::null::<u8>().wrapping_sub(1)`, better known as `0xFFFFFFFFFFFFFFFF`
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SourcePtr(*const u8);
impl Debug for SourcePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.into_option(), f)
    }
}
impl SourcePtr {
    const fn null() -> Self {
        Self(std::ptr::null::<u8>())
    }
    const fn none() -> Self {
        Self(std::ptr::null::<u8>().wrapping_sub(1))
    }
    fn some(ptr: *const u8) -> Self {
        assert_ne!(ptr, std::ptr::null::<u8>().wrapping_sub(1));
        Self(ptr)
    }
    fn into_option(self) -> Option<*const u8> {
        let SourcePtr(ptr) = self;
        if ptr == std::ptr::null::<u8>().wrapping_sub(1) {
            None
        } else {
            Some(ptr)
        }
    }
}
