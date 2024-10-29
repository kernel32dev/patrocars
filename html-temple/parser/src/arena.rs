use std::{
    cell::RefCell,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    pin::Pin,
};

/// an arena that can adopt heap objects, and extend their lifetime to the lifetime of the arena
///
/// it does not have a true byte allocator like a true arena, just extends lifetimes like one would
///
/// does not implement `Sync` on purpose
pub struct Arena {
    boxes: RefCell<Vec<DropBundle>>,
}

pub trait Liftable<'a> {
    type Output: 'a;
    /// lift self, giving ownership to arena, so it lives for as long as the arena
    fn lift(self, arena: &'a Arena) -> Self::Output;
}

struct DropBundle {
    dropper: unsafe fn(*mut [usize; 2]),
    boxed: [usize; 2],
}

impl Arena {
    pub const fn new() -> Self {
        Self {
            boxes: RefCell::new(Vec::new()),
        }
    }
    pub fn boxed<'a, T>(&'a self, value: T) -> &'a mut T {
        Box::new(value).lift(self)
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for DropBundle {
    fn drop(&mut self) {
        unsafe { (self.dropper)(&mut self.boxed) }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        for i in std::mem::take(self.boxes.get_mut()).into_iter().rev() {
            drop(i);
        }
    }
}

impl<'a, T: ?Sized + 'a> Liftable<'a> for Box<T> {
    type Output = &'a mut T;
    fn lift(mut self, arena: &'a Arena) -> Self::Output {
        unsafe {
            let reference = (&mut *self as *mut T).as_mut().unwrap();
            assert!(std::mem::size_of::<Self>() <= std::mem::size_of::<[usize; 2]>());
            let mut cell = std::mem::MaybeUninit::<DropBundle>::uninit();
            cell.assume_init_mut().dropper = std::mem::transmute::<
                unsafe fn(*mut Box<T>),
                unsafe fn(*mut [usize; 2]),
            >(std::ptr::drop_in_place);
            ((&mut cell.assume_init_mut().boxed) as *mut [usize; 2] as *mut Self).write(self);
            arena.boxes.borrow_mut().push(cell.assume_init());
            reference
        }
    }
}

impl<'a, T: ?Sized + 'a> Liftable<'a> for Pin<Box<T>> {
    type Output = Pin<&'a mut T>;
    fn lift(self, arena: &'a Arena) -> Self::Output {
        unsafe { Pin::new_unchecked(Pin::into_inner_unchecked(self).lift(arena)) }
    }
}
impl<'a, T: 'a> Liftable<'a> for Vec<T> {
    type Output = &'a mut [T];
    fn lift(self, arena: &'a Arena) -> Self::Output {
        self.into_boxed_slice().lift(&arena)
    }
}

impl<'a> Liftable<'a> for String {
    type Output = &'a mut str;
    fn lift(self, arena: &'a Arena) -> Self::Output {
        self.into_boxed_str().lift(&arena)
    }
}

impl<'a> Liftable<'a> for PathBuf {
    type Output = &'a mut Path;
    fn lift(self, arena: &'a Arena) -> Self::Output {
        self.into_boxed_path().lift(&arena)
    }
}

impl<'a> Liftable<'a> for OsString {
    type Output = &'a mut OsStr;
    fn lift(self, arena: &'a Arena) -> Self::Output {
        self.into_boxed_os_str().lift(&arena)
    }
}
