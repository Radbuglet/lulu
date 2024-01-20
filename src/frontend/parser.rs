use aunty::Obj;

use crate::util::{diag::DiagnosticReporter, intern::intern, parser::ParseContext};

use super::token::{Token, TokenGroup, TokenSequence};

pub fn parse_file(diag: Obj<DiagnosticReporter>, tokens: &TokenGroup) {
    let cx = ParseContext::new(diag);
    {
        let mut c = cx.enter(tokens.cursor());
        parse_header(&mut c);
    }
}

pub fn parse_header(c: &mut TokenSequence<'_>) {
    let Some(proclamation_kw) = c.expect(intern!("`i_love_this_language`"), |c| {
        c.next()
            .filter(|t| matches!(t, Token::Ident(i) if i.text == intern!("i_love_this_language")))
    }) else {
        c.stuck(|_| ());
        return;
    };
}
