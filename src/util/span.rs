use aunty::Entity;

use super::parser::{ParseCursor, ParseSequence};

// === Files === //

#[derive(Debug)]
pub struct FileData {
    pub me: Entity,
    pub human_path: String,
    pub data: String,
}

impl FileData {
    pub fn start_loc(&self) -> FileLoc {
        FileLoc {
            file: self.me,
            pos: FilePos { char_index: 0 },
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FileLoc {
    pub file: Entity,
    pub pos: FilePos,
}

impl FileLoc {
    pub fn as_span(self) -> Span {
        Span {
            file: self.file,
            start: self.pos,
            end_excl: self.pos,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialOrd, Ord, PartialEq)]
pub struct FilePos {
    pub char_index: u32,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub file: Entity,
    pub start: FilePos,
    pub end_excl: FilePos,
}

impl Span {
    pub fn new(loc_a: FileLoc, loc_b: FileLoc) -> Self {
        assert_eq!(loc_a.file, loc_b.file);

        let mut pos = [loc_a.pos, loc_b.pos];
        pos.sort();
        let [start, end] = pos;

        Self {
            file: loc_a.file,
            start,
            end_excl: end,
        }
    }

    pub fn start(&self) -> FileLoc {
        FileLoc {
            file: self.file,
            pos: self.start,
        }
    }

    pub fn end(&self) -> FileLoc {
        FileLoc {
            file: self.file,
            pos: self.end_excl,
        }
    }
}

// === FileCursor === //

pub type FileSequence<'a> = ParseSequence<'a, FileCursor<'a>>;

#[derive(Debug, Clone)]
pub struct FileCursor<'a> {
    pub remaining: std::str::Chars<'a>,
    pub loc: FileLoc,
}

impl<'a> FileCursor<'a> {
    pub fn new(file: &'a FileData) -> Self {
        Self {
            remaining: file.data.chars(),
            loc: file.start_loc(),
        }
    }
}

impl Iterator for FileCursor<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let first = self.remaining.next();

        if first.is_some() {
            self.loc.pos.char_index += 1;
        }

        // If we have a carriage return, interpret it as a newline.
        if first == Some('\r') {
            // Consume a whole CRLF if we can.
            self.lookahead(|c| c.next() == Some('\n'));

            // But always interpret it as a newline.
            return Some('\n');
        }

        first
    }
}

impl ParseCursor for FileCursor<'_> {
    fn next_span(&self) -> Span {
        self.loc.as_span()
    }
}
