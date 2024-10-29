use std::fmt::Debug;

pub type File = &'static FileRecord;
pub type Dir = &'static DirRecord;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileRecord {
    pub name: &'static str,
    pub mime: Option<&'static str>,
    pub bytes: &'static [u8],
    pub text: Option<&'static str>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DirRecord {
    pub name: &'static str,
    pub dirs: &'static [&'static DirRecord],
    pub files: &'static [&'static FileRecord],
}

impl DirRecord {
    pub fn get_file(&self, name: &str) -> Option<File> {
        self.files
            .binary_search_by_key(&name, |x| &x.name)
            .ok()
            .and_then(|x| self.files.get(x).copied())
    }
    pub fn get_dir(&self, name: &str) -> Option<Dir> {
        self.dirs
            .binary_search_by_key(&name, |x| &x.name)
            .ok()
            .and_then(|x| self.dirs.get(x).copied())
    }
}

impl Debug for FileRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.name, f)?;
        if let Some(mime) = self.mime {
            f.write_str(" (")?;
            f.write_str(mime)?;
            f.write_str(")")?;
        }
        if f.alternate() {
            if self.bytes.len() == 1 {
                f.write_str(": 1 byte")
            } else {
                write!(f, ": {} bytes", self.bytes.len())
            }
        } else {
            f.write_str(": ")?;
            if let Some(text) = self.text {
                Debug::fmt(text, f)
            } else {
                f.write_str("\"")?;
                format_bytes(self.bytes, f)?;
                f.write_str("\"")
            }
        }
    }
}
impl Debug for DirRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.name, f)?;
        f.write_str(": ")?;
        f.debug_list()
            .entries(self.files)
            .entries(self.dirs)
            .finish()
    }
}

pub unsafe fn clear_asset_cache() {
    #[cfg(debug_assertions)]
    debug_runtime_load::clear_asset_cache();
}

#[cfg(debug_assertions)]
mod debug_runtime_load {
    use super::{DirRecord, FileRecord};
    use std::{borrow::Cow, cell::RefCell, collections::HashSet, path::Path};

    thread_local! {
        static BIN_CACHE: RefCell<HashSet<Box<[u8]>>> = RefCell::new(HashSet::new());
        static FILE_CACHE: RefCell<HashSet<Box<FileRecord>>> = RefCell::new(HashSet::new());
        static DIR_CACHE: RefCell<HashSet<Box<DirRecord>>> = RefCell::new(HashSet::new());
    }

    impl FileRecord {
        pub fn debug_runtime_load(path: impl AsRef<Path>) -> &'static Self {
            let name = path
                .as_ref()
                .file_name()
                .expect("asset path has no filename")
                .to_str()
                .expect("asset name must be valid unicode");
            let bytes = match std::fs::read(path.as_ref()) {
                Ok(bytes) => bytes,
                Err(error) => panic!(
                    "panic while loading asset \"{}\": {error:#?}",
                    path.as_ref().display()
                ),
            };
            let bytes = make_eternal_bytes(bytes);
            make_eternal_file(Self {
                name: make_eternal_str(name),
                mime: mime_guess::from_path(path)
                    .first()
                    .map(|x| make_eternal_str(x.to_string())),
                bytes,
                text: std::str::from_utf8(bytes).ok(),
            })
        }
    }

    impl DirRecord {
        pub fn debug_runtime_load(path: impl AsRef<Path>) -> &'static Self {
            let name = path
                .as_ref()
                .file_name()
                .expect("asset path has no filename")
                .to_str()
                .expect("asset name must be valid unicode");
            let dir = match std::fs::read_dir(path.as_ref()) {
                Ok(dir) => dir,
                Err(error) => panic!(
                    "panic while loading asset \"{}\": {error:#?}",
                    path.as_ref().display()
                ),
            };
            let mut dirs = Vec::new();
            let mut files = Vec::new();
            for entry in dir {
                let (metadata, entry) = match entry.and_then(|x| Ok((x.metadata()?, x))) {
                    Ok(entry) => entry,
                    Err(error) => panic!(
                        "panic while loading asset directory \"{}\": {error:#?}",
                        path.as_ref().display()
                    ),
                };
                if metadata.is_dir() {
                    dirs.push(DirRecord::debug_runtime_load(entry.path()));
                } else {
                    files.push(FileRecord::debug_runtime_load(entry.path()));
                }
            }
            let name = make_eternal_str(name);
            let dirs = Box::leak(dirs.into_boxed_slice());
            let files = Box::leak(files.into_boxed_slice());
            dirs.sort_unstable_by_key(|x| x.name);
            files.sort_unstable_by_key(|x| x.name);
            make_eternal_dir(Self { name, dirs, files })
        }

        unsafe fn force_drop_slices(self) {
            drop(Box::from_raw(
                self.dirs as *const [&'static DirRecord] as *mut [&'static DirRecord],
            ));
            drop(Box::from_raw(
                self.files as *const [&'static FileRecord] as *mut [&'static FileRecord],
            ));
        }
    }

    fn make_eternal_str<'a>(bytes: impl Into<Cow<'a, str>>) -> &'static str {
        let bytes = make_eternal_bytes(match bytes.into() {
            Cow::Borrowed(bytes) => Cow::Borrowed(bytes.as_bytes()),
            Cow::Owned(bytes) => Cow::Owned(Vec::from(bytes)),
        });
        // SAFETY: as long as the bytes returned by make_eternal_bytes really are the same as passed in
        // this is safe, as the bytes passed in were guaranteed to be valid utf8
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }

    fn make_eternal_bytes<'a>(bytes: impl Into<Cow<'a, [u8]>>) -> &'static [u8] {
        let bytes: Cow<[u8]> = bytes.into();
        BIN_CACHE.with_borrow_mut(|x| {
            if let Some(cached) = x.get(bytes.as_ref()) {
                unsafe { (&cached[..] as *const [u8]).as_ref().unwrap() }
            } else {
                let boxed = bytes.into_owned().into_boxed_slice();
                let reference = unsafe { (&boxed[..] as *const [u8]).as_ref().unwrap() };
                x.insert(boxed);
                reference
            }
        })
    }

    fn make_eternal_file<'a>(file: FileRecord) -> &'static FileRecord {
        FILE_CACHE.with_borrow_mut(|x| {
            if let Some(cached) = x.get(&file) {
                unsafe { (&**cached as *const FileRecord).as_ref().unwrap() }
            } else {
                let boxed = Box::new(file);
                let reference = unsafe { (&*boxed as *const FileRecord).as_ref().unwrap() };
                x.insert(boxed);
                reference
            }
        })
    }

    fn make_eternal_dir<'a>(dir: DirRecord) -> &'static DirRecord {
        DIR_CACHE.with_borrow_mut(|x| {
            if let Some(cached) = x.get(&dir) {
                unsafe { (&**cached as *const DirRecord).as_ref().unwrap() }
            } else {
                let boxed = Box::new(dir);
                let reference = unsafe { (&*boxed as *const DirRecord).as_ref().unwrap() };
                x.insert(boxed);
                reference
            }
        })
    }

    pub unsafe fn clear_asset_cache() {
        BIN_CACHE.with_borrow_mut(std::mem::take);
        FILE_CACHE.with_borrow_mut(std::mem::take);
        let dirs = DIR_CACHE.with_borrow_mut(std::mem::take);
        for i in dirs {
            i.force_drop_slices();
        }
    }
}

fn format_bytes(mut bytes: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    while !bytes.is_empty() {
        let good: &str;
        let bad: &[u8];
        match std::str::from_utf8(bytes) {
            Ok(rest) => {
                good = rest;
                bad = b"";
                bytes = &bytes[0..0];
            }
            Err(error) => {
                let valid_up_to = error.valid_up_to();
                good = unsafe { std::str::from_utf8_unchecked(&bytes[..valid_up_to]) };
                match error.error_len() {
                    Some(error_len) => {
                        bad = &bytes[valid_up_to..valid_up_to + error_len];
                        bytes = &bytes[valid_up_to + error_len..];
                    }
                    None => {
                        bad = &bytes[valid_up_to..];
                        bytes = &bytes[0..0];
                    }
                }
            }
        }
        format_str(good, f)?;
        for i in bad {
            write!(f, "\\x{i:2X}")?;
        }
    }
    Ok(())
}

fn format_str(mut str: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    while let Some((safe, rest)) = str.split_once(|x: char| x.is_control()) {
        f.write_str(safe)?;
        let c = str.chars().next().unwrap();
        str = rest;
        match c {
            '\0' => f.write_str("\\0")?,
            '\t' => f.write_str("\\t")?,
            '\n' => f.write_str("\\n")?,
            '\r' => f.write_str("\\r")?,
            '\"' => f.write_str("\\\"")?,
            //'\'' => f.write_str("\\'")?,
            '\\' => f.write_str("\\\\")?,
            ..='\x7F' => write!(f, "\\x{:2X}", c as u32)?,
            ..='\u{FF}' => write!(f, "\\u{{{:2X}}}", c as u32)?,
            ..='\u{FFF}' => write!(f, "\\u{{{:3X}}}", c as u32)?,
            ..='\u{FFFF}' => write!(f, "\\u{{{:4X}}}", c as u32)?,
            ..='\u{FFFFF}' => write!(f, "\\u{{{:5X}}}", c as u32)?,
            ..='\u{10FFFF}' => write!(f, "\\u{{{:6X}}}", c as u32)?,
        }
    }
    f.write_str(str)
}
