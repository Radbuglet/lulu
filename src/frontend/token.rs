use aunty::Obj;

use crate::util::{
    diag::DiagnosticReporter,
    parser::ParseContext,
    span::{intern, FileCursor, FileData, FileSequence},
};

// === Tokens === //

#[derive(Debug)]
pub enum Token {
    Punct,
    Ident,
    StringLit,
    NumLit,
    Group,
}

// === Tokenizer === //

pub fn tokenize(diag: Obj<DiagnosticReporter>, file: &FileData) {
    let cx = ParseContext::new(diag);

    let cursor = cx.enter(FileCursor::new(file));
    let _guard = cursor.while_parsing(intern!("file"));
}

fn parse_group(c: &mut FileSequence) {}
