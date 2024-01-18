use core::fmt;

use aunty::{CyclicCtor, Entity, Obj, ObjRef};

use super::parser::{ForkableCursor, ParseCursor, ParseSequence};

// === Files === //

#[derive(Debug)]
pub struct FileData {
    pub me: Entity,
    pub human_path: String,
    pub data: String,
    pub line_start_offsets: Vec<usize>,
}

impl FileData {
    pub fn new(path: String, data: String) -> impl CyclicCtor<Self> {
        |me, _| {
            let mut line_start_offsets = Vec::new();
            if !data.is_empty() {
                line_start_offsets.push(0);
            }

            let mut reader = NormalizedStrIterator::new(&data);

            while let Some((_, ch)) = reader.next() {
                if ch == '\n' {
                    line_start_offsets.push(reader.next_pos());
                }
            }

            Self {
                me,
                human_path: path,
                data,
                line_start_offsets,
            }
        }
    }

    pub fn start_loc(&self) -> FileLoc {
        FileLoc {
            file: self.me,
            pos: FileOffset { idx: 0 },
        }
    }

    pub fn end_loc(&self) -> FileLoc {
        FileLoc {
            file: self.me,
            pos: FileOffset {
                idx: self.data.len(),
            },
        }
    }

    pub fn file_span(&self) -> Span {
        Span {
            file: self.me,
            start: FileOffset { idx: 0 },
            end_excl: FileOffset {
                idx: self.data.len(),
            },
        }
    }

    pub fn line_start(&self, idx: usize) -> FileLoc {
        FileLoc {
            file: self.me,
            pos: FileOffset {
                idx: self.line_start_offsets[idx],
            },
        }
    }

    fn line_of_pos(&self, pos: FileOffset) -> usize {
        match self.line_start_offsets.binary_search(&pos.idx) {
            Ok(line_idx) => line_idx,
            // This is the index of where we'd have to insert a our element to
            // preserve order. If we're on line zero, the situation would look
            // like this...
            //
            // ```
            // line_start_offsets:
            // 0 10 ...
            //   ^ 4 needs to be inserted here!
            // ```
            //
            // Hence, we need to subtract one from this index to get the index
            // of the line we're on.
            Err(insert_line_idx) => insert_line_idx - 1,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct FileOffset {
    pub idx: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct FileLoc {
    pub file: Entity,
    pub pos: FileOffset,
}

impl FileLoc {
    pub fn file(&self) -> Obj<FileData> {
        self.file.obj()
    }

    pub fn file_ref(&self) -> ObjRef<FileData> {
        self.file.get()
    }

    pub fn with_pos(self, pos: FileOffset) -> Self {
        Self {
            file: self.file,
            pos,
        }
    }

    pub fn line(self) -> usize {
        self.file_ref().line_of_pos(self.pos)
    }

    pub fn line_and_column(self) -> LineAndColumn {
        let file = self.file_ref();
        let line = file.line_of_pos(self.pos);
        let line_start = file.line_start(line);
        let column = self.pos.idx - line_start.pos.idx;

        LineAndColumn { line, column }
    }

    pub fn line_start(self) -> FileLoc {
        let file = self.file_ref();
        file.line_start(file.line_of_pos(self.pos))
    }
}

#[derive(Copy, Clone)]
pub struct Span {
    pub file: Entity,
    pub start: FileOffset,
    pub end_excl: FileOffset,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}",
            self.file_ref().human_path,
            self.start().line_and_column(),
            self.end().line_and_column(),
        )
    }
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

    pub fn file(&self) -> Obj<FileData> {
        self.file.obj()
    }

    pub fn file_ref(&self) -> ObjRef<FileData> {
        self.file.get()
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

    pub fn to(self, other: Self) -> Self {
        Self::new(self.start(), other.end())
    }

    pub fn between(self, other: Self) -> Self {
        Self::new(self.end(), other.start())
    }

    pub fn until(self, other: Self) -> Self {
        Self::new(self.start(), other.start())
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct LineAndColumn {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for LineAndColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

// === NormalizedStrIterator === //

#[derive(Debug, Clone)]
pub struct NormalizedStrIterator<'a> {
    pub iter: std::str::CharIndices<'a>,
    // Needed until `CharIndices::offset` stabilizes.
    pub eof_idx: usize,
}

impl<'a> NormalizedStrIterator<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            iter: str.char_indices(),
            eof_idx: str.len(),
        }
    }

    pub fn next_pos(&self) -> usize {
        match self.peek() {
            Some((idx, _)) => idx,
            None => self.eof_idx,
        }
    }
}

impl Iterator for NormalizedStrIterator<'_> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let first = self.iter.next();

        // If we have a carriage return, interpret it as a newline.
        if let Some((first_idx, '\r')) = first {
            // Consume a whole CRLF if we can.
            let second_idx = self.lookahead(|c| match c.next() {
                Some((second_idx, '\n')) => Some(second_idx),
                _ => None,
            });

            // But always interpret it as a newline.
            return Some((second_idx.unwrap_or(first_idx), '\n'));
        }

        first
    }
}

impl ForkableCursor for NormalizedStrIterator<'_> {}

// === FileCursor === //

pub type FileSequence<'a> = ParseSequence<'a, FileCursor<'a>>;

#[derive(Debug, Clone)]
pub struct FileCursor<'a> {
    pub file: Entity,
    pub iter: NormalizedStrIterator<'a>,
}

impl<'a> FileCursor<'a> {
    pub fn new(file: &'a FileData) -> Self {
        Self {
            file: file.me,
            iter: NormalizedStrIterator::new(&file.data),
        }
    }

    pub fn new_spanned(file: &'a FileData, span: Span) -> Self {
        Self {
            file: file.me,
            iter: NormalizedStrIterator::new(&file.data[span.start.idx..span.end_excl.idx]),
        }
    }
}

impl Iterator for FileCursor<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(_, ch)| ch)
    }
}

impl ForkableCursor for FileCursor<'_> {}

impl ParseCursor for FileCursor<'_> {
    fn next_span(&self) -> Span {
        let mut fork = self.iter.clone();
        let first_pos = fork.next_pos();
        let _ = fork.next();
        let second_pos = fork.next_pos();

        Span {
            file: self.file,
            start: FileOffset { idx: first_pos },
            end_excl: FileOffset { idx: second_pos },
        }
    }
}
