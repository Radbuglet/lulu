use aunty::Obj;
use unicode_xid::UnicodeXID;

use std::fmt;

use crate::util::{
    diag::{Diagnostic, DiagnosticKind, DiagnosticReporter},
    intern::{intern, Intern},
    parser::{ForkableCursor, ParseContext},
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
    pub is_raw: bool,
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
    {
        let mut c = cx.enter(FileCursor::new(file));
        let open_span = c.next_span();
        parse_group(&mut c, open_span, GroupDelimiter::File)
    }
}

fn parse_group(c: &mut FileSequence, open_span: Span, delimiter: GroupDelimiter) -> TokenGroup {
    let mut tokens = Vec::new();

    let _wp = 'wp_ctor: {
        Some(c.while_parsing(match delimiter {
            GroupDelimiter::Brace => intern!("braced token group"),
            GroupDelimiter::Bracket => intern!("bracketed token group"),
            GroupDelimiter::Paren => intern!("parenthesized token group"),
            GroupDelimiter::File => break 'wp_ctor None,
        }))
    };

    loop {
        let next_span = c.next_span();

        // Match whitespace
        if c.expect(intern!("whitespace"), |c| {
            c.next().is_some_and(char::is_whitespace)
        }) {
            continue;
        }

        // Match comments
        if parse_line_comment(c) {
            continue;
        }

        if parse_block_comment(c) {
            continue;
        }

        // Match opening delimiters
        if let Some(delimiter) = parse_open_delimiter(c) {
            tokens.push(parse_group(c, next_span, delimiter).into());
            continue;
        }

        // Match closing delimiters
        if let Some(close_delimiter) = parse_close_delimiter(c) {
            if delimiter != close_delimiter {
                let prefix = if close_delimiter == GroupDelimiter::File {
                    "unclosed delimiter"
                } else {
                    "mismatched delimiter"
                };

                c.error(
                    {
                        let mut diagnostic = Diagnostic::span_err(
                            next_span,
                            format!("{prefix}; expected {}", delimiter.closer()),
                        );

                        if delimiter != GroupDelimiter::File {
                            diagnostic.subs.push(
                                Diagnostic::new(
                                    DiagnosticKind::Info,
                                    "looking for match to this delimiter",
                                )
                                .with_offending_span(open_span),
                            );
                        }

                        diagnostic
                    },
                    |_| (), // Already caught up
                );
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

        // Match raw identifier or punctuation
        if c.expect(intern!("`@`"), |c| c.next() == Some('@')) {
            // Match as raw identifier
            if let Some(ident) = parse_ident(c, true) {
                tokens.push(ident.into());
                continue;
            }

            // Otherwise, treat as punctuation.
            tokens.push(
                TokenPunct {
                    loc: next_span.start(),
                    char: punct!('@'),
                }
                .into(),
            );
            continue;
        }

        // Match ident
        if let Some(ident) = parse_ident(c, false) {
            tokens.push(ident.into());
            continue;
        }

        // Match punctuation
        if let Some(char) = parse_punct_char(c) {
            tokens.push(
                TokenPunct {
                    loc: next_span.start(),
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
        span: open_span.until(c.next_span()),
        delimiter,
        tokens,
    }
}

fn parse_line_comment(c: &mut FileSequence) -> bool {
    if !c.expect(intern!("`//`"), |c| {
        c.next() == Some('/') && c.next() == Some('/')
    }) {
        return false;
    }

    loop {
        let read = c.irrefutable(|c| c.next());
        if read.is_none() || read == Some('\n') {
            break;
        }
    }

    true
}

fn parse_block_comment(c: &mut FileSequence) -> bool {
    if !c.expect(intern!("`/*`"), |c| {
        c.next() == Some('/') && c.next() == Some('*')
    }) {
        return false;
    }

    let _wp = c.while_parsing(intern!("block comment"));

    loop {
        if c.expect(intern!("`*/`"), |c| {
            c.next() == Some('*') && c.next() == Some('/')
        }) {
            break;
        }

        if parse_block_comment(c) {
            continue;
        }

        if c.expect(intern!("comment character"), |c| c.next().is_some()) {
            continue;
        }

        c.stuck(|_| ()); // Already recovered.
        break;
    }

    true
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

fn parse_ident(c: &mut FileSequence, is_raw: bool) -> Option<TokenIdent> {
    let start = c.next_span();
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
        span: start.until(c.next_span()),
        text: Intern::new(builder),
        is_raw,
    })
}

fn parse_string_literal(c: &mut FileSequence) -> Option<TokenStringLit> {
    let start = c.next_span();
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
        span: start.until(c.next_span()),
        inner: Intern::new(builder),
    })
}

fn parse_char_literal(c: &mut FileSequence) -> Option<TokenCharLit> {
    let start = c.next_span();

    // Match opening quote
    if !c.expect(intern!("`'`"), |c| c.next() == Some('\'')) {
        return None;
    }

    let _wp = c.while_parsing(intern!("character literal"));

    // Match inner character
    let ch = 'parse: {
        // Match character escape
        if c.expect(intern!("`\\`"), |c| c.next() == Some('\\')) {
            break 'parse parse_char_escape(c, false);
        }

        // Match anything but an EOF, a newline, or a closing quote
        if let Some(char) = c.expect(intern!("string character"), |c| {
            c.next().filter(|&c| c != '\'' && c != '\n')
        }) {
            break 'parse Some(char);
        }

        c.hint_stuck_if_passes("try escaping the `'` character using `\\'`", |c| {
            c.next() == Some('\'')
        });

        c.hint_stuck_if_passes(
            "newlines must be written using their escape sequence `\\n`",
            |c| c.next() == Some('\n'),
        );

        // Otherwise, we got stuck.
        c.stuck(|c| {
            if c.peek() == Some('\'') {
                let _ = c.next();
            }
        });
        None
    };

    // Match closing quote
    if !c.expect(intern!("`'`"), |c| c.next() == Some('\'')) {
        // Otherwise, we got stuck.
        c.stuck(|_| ());

        // (fallthrough)
    }

    Some(TokenCharLit {
        span: start.until(c.next_span()),
        ch: ch.unwrap_or('\0'),
    })
}

fn parse_char_escape(c: &mut FileSequence, allow_multiline: bool) -> Option<char> {
    let esc_start = c.next_span();
    let _wp = c.while_parsing(intern!("character escape code"));

    // Match multiline escape
    if allow_multiline {
        if c.expect(intern!("newline"), |c| c.next() == Some('\n')) {
            // Match leading whitespace
            while c.expect(intern!("escaped space"), |c| {
                c.next().is_some_and(|c| c.is_whitespace())
            }) {}

            // Match pipe
            let _ = c.expect(intern!("|"), |c| c.next() == Some('|'));

            return None;
        }
    } else {
        c.hint_stuck_if_passes("you can only escape newlines in string literals", |c| {
            c.next() == Some('\n')
        });
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
        let hex_start = c.next_span();

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
                    hex_start.until(c.next_span()),
                    "invalid ASCII escape code (must be 0x7F or less)",
                ),
                |_| (),
            );
            return None;
        }

        return Some(hex as char);
    }

    // Match Unicode escapes
    if c.expect(intern!("`u`"), |c| c.next() == Some('u')) {
        let _wp = c.while_parsing(intern!("Unicode escape sequence"));

        // Match opening brace
        if !c.expect(intern!("`{`"), |c| c.next() == Some('{')) {
            c.stuck(|_| ());
            return None;
        }

        // Match digits
        let mut digits = String::new();
        while digits.len() < 6 {
            // Match a hexadecimal digit
            if let Some(digit) = c.expect(intern!("hexadecimal digit"), |c| {
                c.next().filter(char::is_ascii_hexdigit)
            }) {
                digits.push(digit);
                continue;
            }

            // Match an underscore
            if c.expect(intern!("`_`"), |c| c.next() == Some('_')) {
                continue;
            }

            break;
        }

        // If we have an insufficient number of digits or fail to match a closing `}`, we're stuck.
        if digits.is_empty() || !c.expect(intern!("`}`"), |c| c.next() == Some('}')) {
            c.hint_stuck_if_passes("expected at least 1 hexadecimal digit", |c| {
                c.next() == Some('}')
            });

            c.hint_stuck_if_passes("expected at most 6 hexadecimal digits", |c| {
                c.next().is_some_and(|c| c.is_ascii_hexdigit() || c == '_')
            });

            c.stuck_lookahead(|c| loop {
                match c.next() {
                    Some('}') => break true,
                    Some('\n') | None => break false,
                    _ => continue,
                }
            });
            return None;
        }

        // Parse hex-code
        let code = u32::from_str_radix(&digits, 16).unwrap();

        // Validate code
        let Some(ch) = char::from_u32(code) else {
            c.error(
                Diagnostic::span_err(
                    esc_start.until(c.next_span()),
                    format!("unicode escape {digits:?} is invalid"),
                ),
                |_| (),
            );
            return None;
        };

        return Some(ch);
    }

    // Otherwise, we're stuck.
    c.stuck(|_| ());

    None
}
