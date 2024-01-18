use aunty::Obj;
use unicode_xid::UnicodeXID;

use std::fmt;

use crate::util::{
    diag::{Diagnostic, DiagnosticReporter},
    intern::{intern, Intern},
    parser::{ParseContext, ParseCursor},
    span::{FileCursor, FileData, FileLoc, FileSequence, Span},
};

// === Tokens === //

#[derive(Debug, Clone)]
pub enum Token {
    Group(TokenGroup),
    StringLit(TokenStringLit),
    CharLit(TokenCharLit),
    Ident(TokenIdent),
    Punct(TokenPunct),
}

impl From<TokenGroup> for Token {
    fn from(value: TokenGroup) -> Self {
        Self::Group(value)
    }
}

impl From<TokenStringLit> for Token {
    fn from(value: TokenStringLit) -> Self {
        Self::StringLit(value)
    }
}

impl From<TokenCharLit> for Token {
    fn from(value: TokenCharLit) -> Self {
        Self::CharLit(value)
    }
}

impl From<TokenIdent> for Token {
    fn from(value: TokenIdent) -> Self {
        Self::Ident(value)
    }
}

impl From<TokenPunct> for Token {
    fn from(value: TokenPunct) -> Self {
        Self::Punct(value)
    }
}

// Group
#[derive(Debug, Clone)]
pub struct TokenGroup {
    pub span: Span,
    pub delimiter: GroupDelimiter,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GroupDelimiter {
    Brace,
    Bracket,
    Paren,
    File,
}

impl GroupDelimiter {
    pub fn closer(self) -> &'static str {
        match self {
            GroupDelimiter::Brace => "}",
            GroupDelimiter::Bracket => "]",
            GroupDelimiter::Paren => ")",
            GroupDelimiter::File => "end of file",
        }
    }
}

// StringLit
#[derive(Debug, Clone)]
pub struct TokenStringLit {
    pub span: Span,
    pub inner: Intern,
}

// CharLit
#[derive(Debug, Clone)]
pub struct TokenCharLit {
    pub span: Span,
    pub ch: char,
}

// Ident
#[derive(Debug, Clone)]
pub struct TokenIdent {
    pub span: Span,
    pub text: Intern,
}

// Puncts
#[derive(Debug, Copy, Clone)]
pub struct TokenPunct {
    pub loc: FileLoc,
    pub char: PunctChar,
}

macro_rules! define_puncts {
	(
		$(#[$attr:meta])*
		$vis:vis enum $enum_name:ident {
			$($name:ident = $char:expr),*
			$(,)?
		}
	) => {
		$(#[$attr])*
		#[derive(Copy, Clone, Hash, Eq, PartialEq)]
		$vis enum $enum_name {
			$($name),*
		}

		impl $enum_name {
			pub const fn from_char(c: char) -> Option<Self> {
				match c {
					$($char => Some(Self::$name),)*
					_ => None,
				}
			}

			pub const fn to_char(self) -> char {
				match self {
					$(Self::$name => $char),*
				}
			}
		}

		impl fmt::Debug for $enum_name {
			fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
				write!(f, "{:?}", self.to_char())
			}
		}
	};
}

define_puncts! {
    pub enum PunctChar {
        Equals = '=',
        LessThan = '<',
        GreaterThan = '>',
        Exclamation = '!',
        Tilde = '~',
        Plus = '+',
        Minus = '-',
        Asterisk = '*',
        Slash = '/',
        Percent = '%',
        Caret = '^',
        Ampersand = '&',
        Bar = '|',
        At = '@',
        Point = '.',
        Comma = ',',
        Semicolon = ';',
        Colon = ':',
        Pound = '#',
        Dollar = '$',
        Question = '?',
        BackSlash = '\\',
        BackTick = '`',

    }
}

#[doc(hidden)]
pub mod punct_macro_internals {
    pub use {
        super::PunctChar,
        std::{concat, option::Option::*, panic},
    };
}

macro_rules! punct {
    ($ch:expr) => {{
        const CHAR: $crate::frontend::token::punct_macro_internals::PunctChar =
            match $crate::frontend::token::punct_macro_internals::PunctChar::from_char($ch) {
                $crate::frontend::token::punct_macro_internals::Some(v) => v,
                $crate::frontend::token::punct_macro_internals::None => {
                    $crate::frontend::token::punct_macro_internals::panic!(
                        $crate::frontend::token::punct_macro_internals::concat!(
                            "unknown punct `",
                            $ch,
                            "`"
                        ),
                    );
                }
            };
        CHAR
    }};
}

pub(crate) use punct;

// === Tokenizer === //

pub fn tokenize(diag: Obj<DiagnosticReporter>, file: &FileData) -> TokenGroup {
    let cx = ParseContext::new(diag);

    parse_group(
        &mut cx.enter(FileCursor::new(file)),
        file.start_loc(),
        GroupDelimiter::File,
    )
}

fn parse_group(c: &mut FileSequence, open_loc: FileLoc, delimiter: GroupDelimiter) -> TokenGroup {
    let mut tokens = Vec::new();
    let _wp = c.while_parsing(match delimiter {
        GroupDelimiter::Brace => intern!("braced token group"),
        GroupDelimiter::Bracket => intern!("bracketed token group"),
        GroupDelimiter::Paren => intern!("parenthesized token group"),
        GroupDelimiter::File => intern!("file-spanning token group"),
    });

    loop {
        let next_loc = c.next_loc();

        // Match whitespace
        if c.expect(intern!("whitespace"), |c| {
            c.next().is_some_and(char::is_whitespace)
        }) {
            continue;
        }

        // Match opening delimiters
        if let Some(delimiter) = parse_open_delimiter(c) {
            tokens.push(parse_group(c, next_loc, delimiter).into());
            continue;
        }

        // Match closing delimiters
        if let Some(close_delimiter) = parse_close_delimiter(c) {
            if delimiter != close_delimiter {
                if close_delimiter == GroupDelimiter::File {
                    c.error(
                        Diagnostic::span_err(
                            next_loc.as_span(),
                            format!("unclosed delimiter; expected {}", delimiter.closer()),
                        ),
                        |_| (), // Already caught up
                    );
                } else {
                    c.error(
                        Diagnostic::span_err(
                            next_loc.as_span(),
                            format!(
                                "mismatched delimiter; expected {}, found {}",
                                delimiter.closer(),
                                close_delimiter.closer()
                            ),
                        ),
                        |_| (), // Already caught up
                    );
                }
            }

            break;
        }

        // Match string literals
        if let Some(sl) = parse_string_literal(c) {
            tokens.push(sl.into());
            continue;
        }

        // Match character literals
        if let Some(cl) = parse_char_literal(c) {
            tokens.push(cl.into());
            continue;
        }

        // Match ident
        if let Some(ident) = parse_ident(c) {
            tokens.push(ident.into());
            continue;
        }

        // Match punctuation
        if let Some(char) = parse_punct_char(c) {
            tokens.push(
                TokenPunct {
                    loc: next_loc,
                    char,
                }
                .into(),
            );
            continue;
        }

        // Otherwise, we're stuck.
        c.stuck(|c| {
            // Just ignore the offending character.
            let _ = c.next();
        });
    }

    TokenGroup {
        span: Span::new(open_loc, c.next_loc()),
        delimiter,
        tokens,
    }
}

fn parse_open_delimiter(c: &mut FileSequence) -> Option<GroupDelimiter> {
    if c.expect(intern!("`{`"), |c| c.next() == Some('{')) {
        return Some(GroupDelimiter::Brace);
    }

    if c.expect(intern!("`[`"), |c| c.next() == Some('[')) {
        return Some(GroupDelimiter::Bracket);
    }

    if c.expect(intern!("`(`"), |c| c.next() == Some('(')) {
        return Some(GroupDelimiter::Paren);
    }

    None
}

fn parse_close_delimiter(c: &mut FileSequence) -> Option<GroupDelimiter> {
    if c.expect(intern!("`}`"), |c| c.next() == Some('}')) {
        return Some(GroupDelimiter::Brace);
    }

    if c.expect(intern!("`]`"), |c| c.next() == Some(']')) {
        return Some(GroupDelimiter::Bracket);
    }

    if c.expect(intern!("`)`"), |c| c.next() == Some(')')) {
        return Some(GroupDelimiter::Paren);
    }

    if c.expect(intern!("end of file"), |c| c.next().is_none()) {
        return Some(GroupDelimiter::File);
    }

    None
}

fn parse_punct_char(c: &mut FileSequence) -> Option<PunctChar> {
    c.expect(intern!("punctuation"), |c| {
        c.next().and_then(PunctChar::from_char)
    })
}

fn parse_ident(c: &mut FileSequence) -> Option<TokenIdent> {
    let start = c.next_loc();
    let mut builder = String::new();

    // Match first character
    builder.push(c.expect(intern!("identifier"), |c| {
        c.next().filter(|c| c.is_xid_start())
    })?);

    // Match subsequent characters
    while let Some(ch) = c.expect(intern!("identifier"), |c| {
        c.next().filter(|c| c.is_xid_continue())
    }) {
        builder.push(ch);
    }

    Some(TokenIdent {
        span: Span::new(start, c.next_loc()),
        text: Intern::new(builder),
    })
}

fn parse_string_literal(c: &mut FileSequence) -> Option<TokenStringLit> {
    let start_loc = c.next_loc();
    let _wp = c.while_parsing(intern!("string literal"));

    // Match opening quote
    if !c.expect(intern!("`\"`"), |c| c.next() == Some('"')) {
        return None;
    }

    // Match string contents
    let mut builder = String::new();
    loop {
        // Match character escape
        if c.expect(intern!("`\\`"), |c| c.next() == Some('\\')) {
            if let Some(esc) = parse_char_escape(c, true) {
                builder.push(esc);
            }

            continue;
        }

        // Match closing quote
        if c.expect(intern!("`\"`"), |c| c.next() == Some('"')) {
            break;
        }

        // Match anything but the EOF
        if let Some(char) = c.expect(intern!("string character"), |c| c.next()) {
            builder.push(char);
            continue;
        }

        // Otherwise, we got stuck.
        c.stuck(|_| ());
        break;
    }

    Some(TokenStringLit {
        span: Span::new(start_loc, c.next_loc()),
        inner: Intern::new(builder),
    })
}

fn parse_char_literal(c: &mut FileSequence) -> Option<TokenCharLit> {
    let start_loc = c.next_loc();

    // Match opening quote
    if !c.expect(intern!("`'`"), |c| c.next() == Some('\'')) {
        return None;
    }

    // Match inner character
    let ch = 'parse: {
        // Match character escape
        if c.expect(intern!("`\\`"), |c| c.next() == Some('\\')) {
            break 'parse parse_char_escape(c, false);
        }

        // Match anything but the EOF or a closing quote
        if let Some(char) = c.expect(intern!("string character"), |c| {
            c.next().filter(|c| *c != '\'')
        }) {
            break 'parse Some(char);
        }

        // Otherwise, we got stuck.
        c.stuck(|_| ());
        None
    };

    // Match closing quote
    if !c.expect(intern!("`'`"), |c| c.next() == Some('\'')) {
        // Otherwise, we got stuck.
        c.stuck(|_| ());

        // (fallthrough)
    }

    Some(TokenCharLit {
        span: Span::new(start_loc, c.next_loc()),
        ch: ch.unwrap_or('\0'),
    })
}

fn parse_char_escape(c: &mut FileSequence, allow_multiline: bool) -> Option<char> {
    let _wp = c.while_parsing(intern!("character escape code"));

    // Match multiline escape
    if allow_multiline && c.expect(intern!("newline"), |c| c.next() == Some('\n')) {
        // Match leading whitespace
        while c.expect(intern!("escaped space"), |c| {
            c.next().is_some_and(|c| c.is_whitespace())
        }) {}

        // Match pipe
        let _ = c.expect(intern!("|"), |c| c.next() == Some('|'));

        return None;
    }

    // Match simple escapes
    for (expected, ch, decoded) in [
        // Quotes
        (intern!("`\"`"), '"', '"'),
        (intern!("`'`"), '\'', '\''),
        // ASCII
        (intern!("`n`"), 'n', '\n'),
        (intern!("`r`"), 'r', '\r'),
        (intern!("`t`"), 't', '\t'),
        (intern!("`\\`"), '\\', '\\'),
        (intern!("`0`"), '0', '0'),
    ] {
        if c.expect(expected, |c| c.next() == Some(ch)) {
            return Some(decoded);
        }
    }

    // Match ASCII code escapes
    if c.expect(intern!("`x`"), |c| c.next() == Some('x')) {
        let _wp = c.while_parsing(intern!("ASCII escape code"));
        let hex_start = c.next_loc();

        let Some((a, b)) = c.expect(intern!("two hexadecimal digits"), |c| {
            match (c.next(), c.next()) {
                (Some(a), Some(b)) if a.is_ascii_hexdigit() && b.is_ascii_hexdigit() => {
                    Some((a, b))
                }
                _ => None,
            }
        }) else {
            c.stuck(|c| {
                while c.peek().is_some_and(|v| !v.is_whitespace() && v != '"') {
                    let _ = c.next();
                }
            });
            return None;
        };

        let hex = u8::from_str_radix(&format!("{a}{b}"), 16).unwrap();
        if hex > 0x7F {
            c.error(
                Diagnostic::span_err(
                    Span::new(hex_start, c.next_loc()),
                    "invalid ASCII escape code (must be 0x7F or less)",
                ),
                |_| (),
            );
            return None;
        }

        return Some(hex as char);
    }

    // Match Unicode escapes
    // TODO

    // Otherwise, we're stuck.
    c.stuck(|c| {
        while c.peek().is_some_and(|v| !v.is_whitespace() && v != '"') {
            let _ = c.next();
        }
    });

    None
}
