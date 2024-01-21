use crate::{
    frontend::ast::AstPathPart,
    util::{
        diag::DiagnosticReporter,
        intern::{intern, Intern},
        parser::{ParseContext, ParseCursor},
        span::Span,
    },
};

use std::fmt;

use aunty::Obj;
use rustc_hash::FxHashMap;

use super::{
    ast::{AstMultiPath, AstMultiPathList, AstPath, AstPathPrefix},
    token::{punct, GroupDelimiter, PunctChar, Token, TokenGroup, TokenIdent, TokenSequence},
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

fn parse_asterisk(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, intern!("`*`"), &[punct!('*')])
}

fn parse_comma(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, intern!("`,`"), &[punct!(',')])
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

pub fn parse_file(diag: Obj<DiagnosticReporter>, tokens: &TokenGroup) -> impl fmt::Debug {
    let cx = ParseContext::new(diag);
    let mut c = cx.enter(tokens.cursor());

    let path = AstMultiPath::parse(&mut c);
    if !c.expect(intern!("end of file"), |c| c.next().is_none()) {
        c.stuck(|_| ());
    }

    path
}

// === Paths === //

impl AstPath {
    pub fn parse(c: &mut TokenSequence) -> Self {
        let _wp = c.while_parsing(intern!("path"));
        let (expecting_part, me) = Self::parse_inner(c, true);
        if expecting_part {
            c.stuck(|_| ());
        }

        me
    }

    fn parse_inner(c: &mut TokenSequence, is_root: bool) -> (bool, Self) {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        enum Phase {
            WaitingForTurbo,
            WaitingForPart,
        }

        // Match prefix
        let (prefix, mut phase, mut expecting_something) =
            if is_root && parse_keyword(c, AstKeyword::Crate).is_some() {
                (AstPathPrefix::Crate, Phase::WaitingForTurbo, false)
            } else if is_root && parse_turbo(c).is_some() {
                (AstPathPrefix::Crates, Phase::WaitingForPart, true)
            } else if is_root && parse_keyword(c, AstKeyword::Self_).is_some() {
                (AstPathPrefix::Self_, Phase::WaitingForTurbo, false)
            } else {
                (AstPathPrefix::Self_, Phase::WaitingForPart, false)
            };

        // Match parts
        let mut parts = Vec::new();
        loop {
            match phase {
                Phase::WaitingForTurbo => {
                    if parse_turbo(c).is_some() {
                        phase = Phase::WaitingForPart;
                        expecting_something = true;
                        continue;
                    }
                }
                Phase::WaitingForPart => {
                    if let Some(part) = parse_identifier(c, intern!("path part")).or_else(|| {
                        is_root
                            .then(|| parse_keyword(c, AstKeyword::Super))
                            .flatten()
                    }) {
                        parts.push(AstPathPart(part));
                        phase = Phase::WaitingForTurbo;
                        expecting_something = false;
                        continue;
                    }
                }
            }

            break;
        }

        (
            expecting_something,
            Self {
                prefix,
                parts: Box::from_iter(parts),
            },
        )
    }
}

impl AstMultiPath {
    pub fn parse(c: &mut TokenSequence) -> Self {
        Self::parse_inner(c, true)
    }

    fn parse_inner(c: &mut TokenSequence, is_root: bool) -> Self {
        // Match path base
        let (expecting_part, base) = AstPath::parse_inner(c, is_root);

        // Match tree part
        let imports = if expecting_part {
            let start_span = c.next_span();

            if let Some(group) = parse_group(c, GroupDelimiter::Brace) {
                let _wp = c.context().while_parsing(start_span, intern!("path tree"));
                let mut parts = Vec::new();
                let mut c = c.enter(group.cursor());

                // Match path
                loop {
                    // Match separating comma
                    if !parts.is_empty() && parse_comma(&mut c).is_none() {
                        break;
                    }

                    // Match path
                    let path = AstMultiPath::parse_inner(&mut c, false);
                    if path.is_empty() {
                        break;
                    }
                    parts.push(path);
                }

                // Match EOS
                if !c.expect(intern!("`}`"), |c| c.next().is_none()) {
                    c.stuck(|_| ());
                }

                AstMultiPathList::List(Box::from_iter(parts))
            } else if parse_asterisk(c).is_some() {
                AstMultiPathList::Wildcard
            } else {
                c.stuck(|_| ());
                AstMultiPathList::List(Box::from_iter([]))
            }
        } else {
            AstMultiPathList::List(Box::from_iter([]))
        };

        Self { base, imports }
    }
}
