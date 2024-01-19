use std::{
    fmt::{self, Write},
    hash::BuildHasher,
};

use aunty::{make_extensible, Obj, ObjMut, ObjRef, StrongObj};
use hashbrown::hash_map::RawEntryMut;

use super::map::FxHashMap;

// === Interner === //

#[derive(Debug, Default)]
pub struct Interner {
    intern_map: FxHashMap<(usize, u64), ()>,
    offsets: Vec<usize>,
    buffer: String,
}

make_extensible!(pub InternerObj for Interner);

impl Interner {
    pub fn intern(&mut self, str: impl fmt::Display) -> Intern {
        self.build().with(str).commit()
    }

    pub fn build(&mut self) -> InternBuilder<'_> {
        InternBuilder {
            old_len: self.buffer.len(),
            interner: self,
        }
    }

    pub fn decode(&self, intern: Intern) -> &str {
        let start = self.offsets[intern.index];
        let end = self
            .offsets
            .get(intern.index + 1)
            .copied()
            .unwrap_or(self.buffer.len());

        &self.buffer[start..end]
    }
}

impl InternerObj {
    pub fn intern(&self, str: impl fmt::Display) -> Intern {
        self.obj.get_mut().intern(str)
    }
}

#[derive(Debug)]
pub struct InternBuilder<'a> {
    old_len: usize,
    interner: &'a mut Interner,
}

impl InternBuilder<'_> {
    pub fn push(&mut self, ch: char) {
        self.interner.buffer.push(ch);
    }

    pub fn push_str(&mut self, string: &str) {
        self.interner.buffer.push_str(string);
    }

    pub fn with(mut self, f: impl fmt::Display) -> Self {
        let _ = write!(&mut self, "{}", f);
        self
    }

    pub fn as_str(&self) -> &str {
        &self.interner.buffer[self.old_len..]
    }

    pub fn commit(self) -> Intern {
        let hash = self.interner.intern_map.hasher().hash_one(self.as_str());
        let entry =
            self.interner
                .intern_map
                .raw_entry_mut()
                .from_hash(hash, |(idx, entry_hash)| {
                    if *entry_hash != hash {
                        return false;
                    }

                    let start = self.interner.offsets[*idx];
                    let end = self
                        .interner
                        .offsets
                        .get(*idx)
                        .copied()
                        .unwrap_or(self.old_len);

                    self.interner.buffer[start..end] == self.interner.buffer[self.old_len..]
                });

        match entry {
            RawEntryMut::Occupied(entry) => {
                let intern = Intern {
                    index: (entry.get_key_value().0).0,
                };
                drop(self); // Discard the temporary buffer
                intern
            }
            RawEntryMut::Vacant(entry) => {
                let index = self.interner.offsets.len();
                self.interner.offsets.push(self.old_len);
                entry.insert_with_hasher(hash, (index, hash), (), |k| k.1);

                let intern = Intern { index };
                std::mem::forget(self); // Ensure that we don't discard the temporary buffer
                intern
            }
        }
    }
}

impl fmt::Write for InternBuilder<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push(c);
        Ok(())
    }
}

impl Drop for InternBuilder<'_> {
    fn drop(&mut self) {
        self.interner.buffer.truncate(self.old_len);
    }
}

thread_local! {
    static GLOBAL_INTERNER: StrongObj<Interner> = Default::default();
}

pub fn global_interner() -> Obj<Interner> {
    GLOBAL_INTERNER.with(|v| v.downgrade())
}

pub fn global_interner_ref() -> ObjRef<Interner> {
    GLOBAL_INTERNER.with(|v| v.get())
}

pub fn global_interner_mut() -> ObjMut<Interner> {
    GLOBAL_INTERNER.with(|v| v.get_mut())
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Intern {
    index: usize,
}

impl Intern {
    pub fn new(str: impl fmt::Display) -> Self {
        global_interner_mut().intern(str)
    }
}

impl fmt::Debug for Intern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        global_interner_ref().decode(*self).fmt(f)
    }
}

impl fmt::Display for Intern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(global_interner_ref().decode(*self))
    }
}

#[doc(hidden)]
pub mod intern_macro_internals {
    pub use {super::Intern, std::thread_local};
}

macro_rules! intern {
    ($ini:expr) => {{
        $crate::util::intern::intern_macro_internals::thread_local! {
            static __INTERN: $crate::util::intern::intern_macro_internals::Intern =
                $crate::util::intern::intern_macro_internals::Intern::new($ini);
        }

        __INTERN.with(|v| *v)
    }};
}

pub(crate) use intern;
