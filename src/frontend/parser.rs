use crate::{
    frontend::ast::AstPathPart,
    util::{
        diag::DiagnosticReporter,
        parser::{ParseContext, ParseCursor},
        span::Span,
        symbol::Symbol,
    },
};

use std::fmt;

use aunty::{Obj, StrongObj};
use rustc_hash::FxHashMap;

use super::{
    ast::{
        AstBinOpExpr, AstBinOpKind, AstDotExpr, AstExpr, AstMultiPath, AstMultiPathList, AstPath,
        AstPathExpr, AstPathPrefix, AstType, AstTypeKind,
    },
    token::{punct, GroupDelimiter, PunctChar, Token, TokenGroup, TokenIdent, TokenSequence},
};

// === Driver === //

pub fn parse_file(diag: Obj<DiagnosticReporter>, tokens: &TokenGroup) -> impl fmt::Debug {
    let cx = ParseContext::new(diag);
    let mut c = cx.enter(tokens.cursor());

    let expr = AstExpr::parse(&mut c);

    if !c.expect(Symbol!("end of file"), |c| c.next().is_none()) {
        c.stuck(|_| ());
    }

    expr
}

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
			pub fn from_sym(c: Symbol) -> Option<Self> {
				thread_local! {
					static SYM_MAP: FxHashMap<Symbol, $enum_name> = FxHashMap::from_iter([
						$((Symbol!($text), $enum_name::$name),)*
					]);
				}

				SYM_MAP.with(|v| v.get(&c).copied())
			}

			pub fn to_sym(self) -> Symbol {
				const SYM_MAP: [Symbol; 0 $(+ { let _ = $enum_name::$name; 1})*] = [
					$(Symbol!($text),)*
				];

				SYM_MAP[self as usize]
			}

			pub fn to_name_sym(self) -> Symbol {
				const SYM_MAP: [Symbol; 0 $(+ { let _ = $enum_name::$name; 1})*] = [
					$(Symbol!("`" $text "`"),)*
				];

				SYM_MAP[self as usize]
			}
		}

		impl fmt::Debug for $enum_name {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				write!(f, "{:?}", self.to_sym())
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

fn parse_identifier(c: &mut TokenSequence, name: Symbol) -> Option<TokenIdent> {
    let ident = c.expect(name, |c| {
        c.next()
            .and_then(Token::as_ident)
            .copied()
            .filter(|ident| ident.is_raw || AstKeyword::from_sym(ident.text).is_none())
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
    let kw_text = kw.to_sym();

    c.expect(kw.to_name_sym(), |c| {
        c.next()
            .and_then(Token::as_ident)
            .copied()
            .filter(|ident| !ident.is_raw && ident.text == kw_text)
    })
}

fn parse_puncts(c: &mut TokenSequence, name: Symbol, puncts: &[PunctChar]) -> Option<Span> {
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
    parse_puncts(c, Symbol!("`::`"), &[punct!(':'), punct!(':')])
}

fn parse_asterisk(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`*`"), &[punct!('*')])
}

fn parse_comma(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`,`"), &[punct!(',')])
}

fn parse_question(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`?`"), &[punct!('?')])
}

fn parse_lt(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`<`"), &[punct!('<')])
}

fn parse_gt(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`>`"), &[punct!('>')])
}

fn parse_period(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`.`"), &[punct!('.')])
}

fn parse_plus(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`+`"), &[punct!('+')])
}

fn parse_minus(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`-`"), &[punct!('-')])
}

fn parse_group<'a>(c: &mut TokenSequence<'a>, delimiter: GroupDelimiter) -> Option<&'a TokenGroup> {
    let expectation = match delimiter {
        GroupDelimiter::Brace => Symbol!("`{`"),
        GroupDelimiter::Bracket => Symbol!("`[`"),
        GroupDelimiter::Paren => Symbol!("`(`"),
        GroupDelimiter::File => unreachable!(),
    };

    c.expect(expectation, |c| {
        c.next()
            .and_then(Token::as_group)
            .filter(|g| g.delimiter == delimiter)
    })
}

// === Paths === //

impl AstPath {
    pub fn parse(c: &mut TokenSequence) -> Self {
        let _wp = c.while_parsing(Symbol!("path"));
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
                    if let Some(part) = parse_identifier(c, Symbol!("path part")).or_else(|| {
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
                let _wp = c.context().while_parsing(start_span, Symbol!("path tree"));
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
                if !c.expect(Symbol!("`}`"), |c| c.next().is_none()) {
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

// === Types === //

impl AstType {
    pub fn parse(c: &mut TokenSequence) -> Option<Self> {
        // Match base
        let mut base = Self::parse_base(c)?;

        // Match suffixes
        loop {
            // Match option
            if parse_question(c).is_some() {
                base = Self {
                    kind: AstTypeKind::Option,
                    generics: Box::from_iter([base]),
                };
                continue;
            }

            break;
        }

        Some(base)
    }

    fn parse_base(c: &mut TokenSequence) -> Option<Self> {
        let start = c.next_span();

        // Match path
        let path = AstPath::parse(c);
        if !path.is_empty() {
            // Match optional generics
            let mut parts = Vec::new();

            let generic_start = c.next_span();
            if parse_lt(c).is_some() {
                let _wp = c
                    .context()
                    .while_parsing(generic_start, Symbol!("generic list"));

                // Match generic list
                loop {
                    // Match type
                    let Some(path) = AstType::parse(c) else {
                        break;
                    };
                    parts.push(path);

                    // Match comma
                    if parse_comma(c).is_none() {
                        break;
                    }
                }

                // Match close generic
                if parse_gt(c).is_none() {
                    c.stuck(|_| ());
                }
            }

            return Some(Self {
                kind: AstTypeKind::Adt(path),
                generics: Box::from_iter(parts),
            });
        }

        // Match tuple
        if let Some(group) = parse_group(c, GroupDelimiter::Paren) {
            let _wp = c.context().while_parsing(start, Symbol!("tuple type"));
            let mut c = c.enter(group.cursor());
            let mut parts = Vec::new();

            loop {
                // Match type
                let Some(ty) = AstType::parse(&mut c) else {
                    break;
                };
                parts.push(ty);

                // Match comma
                if parse_comma(&mut c).is_none() {
                    break;
                }
            }

            // Match EOS
            if !c.expect(Symbol!(")"), |c| c.next().is_none()) {
                c.stuck(|_| ());
            }

            return Some(Self {
                kind: AstTypeKind::Tuple,
                generics: Box::from_iter(parts),
            });
        }

        None
    }
}

// === Expression === //

impl AstExpr {
    pub fn parse(c: &mut TokenSequence) -> Self {
        // We begin by parsing a bunch of expression fragments.
        let mut frags = Vec::<AstExpr>::new();

        loop {
            if !frags.is_empty()
                && !frags
                    .last()
                    .is_some_and(|v| matches!(v, AstExpr::BinOp(_) | AstExpr::UnaryOp(_)))
            {
                // Match dot expressions
                if parse_period(c).is_some() {
                    let Some(field) = parse_identifier(c, Symbol!("member name")) else {
                        c.stuck(|_| ());
                        continue;
                    };

                    let prev = frags.pop().unwrap();
                    frags.push(
                        StrongObj::new(AstDotExpr {
                            expr: prev,
                            member: field.text,
                        })
                        .into(),
                    );
                }

                // Match binary operators
                let op = if parse_plus(c).is_some() {
                    Some(AstBinOpKind::Add)
                } else if parse_minus(c).is_some() {
                    Some(AstBinOpKind::Sub)
                } else {
                    None
                };

                if let Some(op) = op {
                    frags.push(
                        StrongObj::new(AstBinOpExpr {
                            kind: op,
                            lhs: AstExpr::Placeholder,
                            rhs: AstExpr::Placeholder,
                        })
                        .into(),
                    );
                    continue;
                }
            } else {
                let path = AstPath::parse(c);
                if !path.is_empty() {
                    frags.push(StrongObj::new(AstPathExpr { path }).into());
                    continue;
                }
            }

            break;
        }

        frags.pop().unwrap_or(AstExpr::Placeholder)
    }
}
