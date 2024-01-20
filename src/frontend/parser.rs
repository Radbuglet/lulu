use crate::util::{
    diag::DiagnosticReporter,
    intern::{intern, Intern},
    parser::{ParseContext, ParseCursor},
    span::Span,
};

use std::fmt;

use aunty::Obj;
use rustc_hash::FxHashMap;

use super::token::{
    punct, GroupDelimiter, PunctChar, Token, TokenGroup, TokenIdent, TokenSequence,
};

// === Keywords === //

macro_rules! define_keywords {
	(
		$(#[$attr:meta])*
		$vis:vis enum $enum_name:ident {
			$($name:ident = $text:expr),*
			$(,)?
		}
	) => {
		$(#[$attr])*
		#[derive(Copy, Clone, Hash, Eq, PartialEq)]
		$vis enum $enum_name {
			$($name),*
		}

		impl $enum_name {
			pub fn from_intern(c: Intern) -> Option<Self> {
				thread_local! {
					static INTERN_MAP: FxHashMap<Intern, $enum_name> = FxHashMap::from_iter([
						$((intern!($text), $enum_name::$name),)*
					]);
				}

				INTERN_MAP.with(|v| v.get(&c).copied())
			}

			pub fn to_intern(self) -> Intern {
				thread_local! {
					static INTERN_MAP: [Intern; 0 $(+ { let _ = $enum_name::$name; 1})*] = [
						$(intern!($text),)*
					];
				}

				INTERN_MAP.with(|v| v[self as usize])
			}

			pub fn to_name_intern(self) -> Intern {
				thread_local! {
					static INTERN_MAP: [Intern; 0 $(+ { let _ = $enum_name::$name; 1})*] = [
						$(intern!(format!("`{}`", $text)),)*
					];
				}

				INTERN_MAP.with(|v| v[self as usize])
			}
		}

		impl fmt::Debug for $enum_name {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				write!(f, "{:?}", self.to_intern())
			}
		}
	};
}

define_keywords! {
    pub enum AstKeyword {
        Namespace = "namespace",
        Fn = "fn",
        Pub = "pub",
        Crate = "crate",
        Super = "super",
        SelfTy = "Self",
        Self_ = "self",
        In = "in",
    }
}

// === Parsing Helpers === //

fn parse_identifier(c: &mut TokenSequence, name: Intern) -> Option<TokenIdent> {
    let ident = c.expect(name, |c| {
        c.next()
            .and_then(Token::as_ident)
            .copied()
            .filter(|ident| ident.is_raw || AstKeyword::from_intern(ident.text).is_none())
    });

    if ident.is_none() {
        c.hint_stuck_if_passes(
			"this identifier has been reserved as a keyword; prefix it with `@` to interpret it as an identifier",
			|c| c.next().is_some_and(|t| t.as_ident().is_some()),
		);
    }

    ident
}

fn parse_keyword(c: &mut TokenSequence, kw: AstKeyword) -> Option<TokenIdent> {
    let kw_text = kw.to_intern();

    c.expect(kw.to_name_intern(), |c| {
        c.next()
            .and_then(Token::as_ident)
            .copied()
            .filter(|ident| !ident.is_raw && ident.text == kw_text)
    })
}

fn parse_puncts(c: &mut TokenSequence, name: Intern, puncts: &[PunctChar]) -> Option<Span> {
    c.expect(name, |c| {
        let start = c.next_span();
        let mut last = start;

        puncts
            .iter()
            .enumerate()
            .all(|(i, expected)| {
                last = c.next_span();
                let Some(Token::Punct(punct)) = c.next() else {
                    return false;
                };

                if i != 0 && !punct.glued {
                    return false;
                }

                punct.char == *expected
            })
            .then(|| start.to(last))
    })
}

fn parse_turbo(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, intern!("`::`"), &[punct!(':'), punct!(':')])
}

fn parse_semi(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, intern!("`;`"), &[punct!(';')])
}

fn parse_asterisk(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, intern!("`*`"), &[punct!('*')])
}

fn parse_group<'a>(c: &mut TokenSequence<'a>, delimiter: GroupDelimiter) -> Option<&'a TokenGroup> {
    let expectation = match delimiter {
        GroupDelimiter::Brace => intern!("`{`"),
        GroupDelimiter::Bracket => intern!("`[`"),
        GroupDelimiter::Paren => intern!("`(`"),
        GroupDelimiter::File => unreachable!(),
    };

    c.expect(expectation, |c| {
        c.next()
            .and_then(Token::as_group)
            .filter(|g| g.delimiter == delimiter)
    })
}

// === Parsers === //

pub fn parse_file(diag: Obj<DiagnosticReporter>, tokens: &TokenGroup) {
    let cx = ParseContext::new(diag);
    let mut c = cx.enter(tokens.cursor());
    parse_module(&mut c)
}

fn parse_module(c: &mut TokenSequence) {
    loop {
        // === Directives without visibility === //

        // Match end-of-file
        if c.expect(intern!("end of file"), |c| c.next().is_none()) {
            break;
        }

        // Match namespace declaration
        if let Some(_namespace) = parse_namespace_decl(c) {
            continue;
        }

        // === Directives with visibility === //

        // Match optional visibility
        let _vis = parse_visibility(c);

        // Match function declaration
        if let Some(_fn) = parse_function_decl(c) {
            continue;
        }

        c.stuck(|c| {
            let _ = c.next();
        });
    }
}

fn parse_namespace_decl(c: &mut TokenSequence) -> Option<Vec<TokenIdent>> {
    let _wp = c.while_parsing(intern!("namespace declaration"));

    // Match namespace keyword
    let _namespace = parse_keyword(c, AstKeyword::Namespace)?;

    // Match path
    let parts = parse_path(c, PathParseKind::Namespace);
    if parts.is_empty() {
        c.stuck(|_| ());
    }

    // Match semicolon
    if parse_semi(c).is_none() {
        c.stuck(|_| ());
    }

    Some(parts)
}

fn parse_function_decl(c: &mut TokenSequence) -> Option<()> {
    let _wp = c.while_parsing(intern!("function declaration"));

    // Match `fn` keyword
    let _fn = parse_keyword(c, AstKeyword::Fn)?;

    // Match function name
    let name = parse_identifier(c, intern!("function name"));
    if name.is_none() {
        c.stuck(|_| ());
    }

    Some(())
}

fn parse_visibility(c: &mut TokenSequence) -> Option<()> {
    let _wp = c.while_parsing(intern!("path qualifier"));

    // Match `pub`
    let _pub = parse_keyword(c, AstKeyword::Pub)?;

    // Match optional path
    if let Some(group) = parse_group(c, GroupDelimiter::Paren) {
        let mut c = c.enter(group.cursor());

        // Match path
        let path = parse_path(&mut c, PathParseKind::Tree);
        if path.is_empty() {
            c.stuck(|_| ());
        }

        // Match EOF
        if !c.expect(intern!(")"), |c| c.next().is_none()) {
            c.stuck(|_| ());
        }
    }

    Some(())
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum PathParseKind {
    Namespace,
    Single,
    Tree,
}

fn parse_path(c: &mut TokenSequence, kind: PathParseKind) -> Vec<TokenIdent> {
    let mut parts = Vec::new();

    // Match prefix
    if kind != PathParseKind::Namespace {
        if let Some(ident) = parse_keyword(c, AstKeyword::Crate)
            .or_else(|| parse_keyword(c, AstKeyword::Super))
            .or_else(|| parse_keyword(c, AstKeyword::Self_))
        {
            parts.push(ident);
        }
    } else {
        c.hint_stuck_if_passes("cannot use path qualifiers in this context", |c| {
            c.next().and_then(Token::as_ident).is_some_and(|c| {
                let kws = [AstKeyword::Crate, AstKeyword::Super, AstKeyword::Self_];
                kws.iter().any(|kw| c.text == kw.to_intern())
            })
        });
    }

    // Match subsequent parts
    let expecting_subsequent = loop {
        // Match turbo if expected for continuation
        if !parts.is_empty() && parse_turbo(c).is_none() {
            break false;
        }

        // Match path part
        if let Some(part) = parse_identifier(c, intern!("path part")).or_else(|| {
            (kind != PathParseKind::Namespace)
                .then(|| parse_keyword(c, AstKeyword::Super))
                .flatten()
        }) {
            parts.push(part);
            continue;
        }

        break true;
    };

    // Match tree part
    if kind == PathParseKind::Tree {
        if let Some(group) = parse_group(c, GroupDelimiter::Brace) {
            let mut c = c.enter(group.cursor());
            todo!();
        } else if parse_asterisk(c).is_some() {
            todo!()
        } else if expecting_subsequent {
            c.stuck(|_| ());
        }
    } else if expecting_subsequent {
        c.stuck(|_| ());
    }

    parts
}
