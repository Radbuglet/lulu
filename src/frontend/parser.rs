use crate::{
    frontend::ast::AstPathPart,
    util::{
        diag::DiagnosticReporter,
        parser::{ForkableCursor, ParseContext, ParseCursor},
        span::Span,
        symbol::Symbol,
    },
};

use std::fmt;

use aunty::Obj;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use super::{
    ast::{
        AstBinOpExpr, AstBinOpKind, AstCallExpr, AstCtorExpr, AstDotExpr, AstExpr, AstLiteralExpr,
        AstMultiPath, AstMultiPathList, AstParenExpr, AstPath, AstPathExpr, AstPathPrefix,
        AstTupleExpr, AstType, AstTypeKind, AstUnaryNegExpr,
    },
    token::{
        punct, GroupDelimiter, PunctChar, Token, TokenCharLit, TokenCursor, TokenGroup, TokenIdent,
        TokenNumberLit, TokenSequence, TokenStringLit,
    },
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
        True = "true",
        False = "false",
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

fn match_puncts(c: &mut TokenCursor, puncts: &[PunctChar]) -> Option<Span> {
    c.lookahead(|c| {
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

fn parse_puncts(c: &mut TokenSequence, name: Symbol, puncts: &[PunctChar]) -> Option<Span> {
    c.expect(name, |c| match_puncts(c, puncts))
}

fn parse_turbo(c: &mut TokenSequence) -> Option<Span> {
    parse_puncts(c, Symbol!("`::`"), &[punct!(':'), punct!(':')])
}

fn parse_punct(c: &mut TokenSequence, char: PunctChar) -> Option<Span> {
    c.expect(char.as_char_name(), |c| {
        let span = c.next_span();
        c.next()
            .and_then(Token::as_punct)
            .is_some_and(|c| c.char == char)
            .then_some(span)
    })
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

fn parse_str_lit<'a>(c: &mut TokenSequence<'a>, name: Symbol) -> Option<&'a TokenStringLit> {
    c.expect(name, |c| match c.next() {
        Some(Token::StringLit(lit)) => Some(lit),
        _ => None,
    })
}

fn parse_char_lit<'a>(c: &mut TokenSequence<'a>, name: Symbol) -> Option<&'a TokenCharLit> {
    c.expect(name, |c| match c.next() {
        Some(Token::CharLit(lit)) => Some(lit),
        _ => None,
    })
}

fn parse_number_lit<'a>(c: &mut TokenSequence<'a>, name: Symbol) -> Option<&'a TokenNumberLit> {
    c.expect(name, |c| match c.next() {
        Some(Token::NumberLit(lit)) => Some(lit),
        _ => None,
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
        let imports = if expecting_part || base.is_empty() {
            let start_span = c.next_span();

            if let Some(group) = parse_group(c, GroupDelimiter::Brace) {
                let _wp = c.context().while_parsing(start_span, Symbol!("path tree"));
                let mut parts = Vec::new();
                let mut c = c.enter(group.cursor());

                // Match path
                loop {
                    // Match separating comma
                    if !parts.is_empty() && parse_punct(&mut c, punct!(',')).is_none() {
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
            } else if parse_punct(c, punct!('*')).is_some() {
                AstMultiPathList::Wildcard
            } else {
                if !base.is_empty() {
                    c.stuck(|_| ());
                }
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
            if parse_punct(c, punct!('?')).is_some() {
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
            if parse_punct(c, punct!('<')).is_some() {
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
                    if parse_punct(c, punct!(',')).is_none() {
                        break;
                    }
                }

                // Match close generic
                if parse_punct(c, punct!('>')).is_none() {
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
                if parse_punct(&mut c, punct!(',')).is_none() {
                    break;
                }
            }

            // Match EOS
            if !c.expect(Symbol!("`)`"), |c| c.next().is_none()) {
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
        Self::parse_inner(c, 0)
    }

    // This method implements a Pratt parser to group infix operators in a single pass. The strategy,
    // which was largely derived from [this page on Pratt parsers](matklad-pratt), is as follows:
    //
    // ## Binding Powers
    //
    // As a first approximation, an expression can be thought of as a sequence of atoms and the infix
    // operators between them. In our case, we define atoms as the indivisible pre-existing
    // sub-expressions of an expression such as literals, variables, and, thanks to the way in which
    // we tokenize things, parenthesized groups.
    //
    // Each infix operator has an associated left and right binding power, which is just an integer.
    // A valid parse of an expression must then be parenthesized such that the parentheses adjacent
    // to each atom are all pointing towards the operator with the highest binding power.
    //
    // For example, consider the following expression:
    //
    // ```
    // Items:    1   +   2   *   3
    // Powers:     1   2   3   4
    // ```
    //
    // The following grouping is invalid:
    //
    // ```
    // Items:  ((1   +   2)  *   3)
    // Powers:     1   2   3   4
    // ```
    //
    // ...because the atom `2` should have a right-facing parenthesis pointing towards the `*` with
    // binding power `3` rather than a left-facing parenthesis pointing towards the `+`, which only
    // has binding power `2`.
    //
    // This grouping, meanwhile, is valid:
    //
    // ```
    // Items:   (1   +  (2   *   3))
    // Powers:     1   2   3   4
    // ```
    //
    // We know that this parse is unique because each atom needs at least one corresponding parenthesis
    // to fully group the expression and the direction of that parenthesis is uniquely determined so
    // long as we make ties between binding powers impossible or define them to be biased in some
    // way.
    //
    // In addition to uniquely defining an order of operations, this binding-power scheme can also
    // encode the notion of associativity:
    //
    // Left associative...
    //
    // ```
    // Items:       1   +   2   +   3
    // Powers:        1   2   1   2
    //
    // Parses as: ((1   +   2)  +   3)
    // ```
    //
    // Right associative...
    //
    // ```
    // Items:       1   +   2   +   3
    // Powers:        2   1   2   1
    //
    // Parses as:  (1   +  (2   +   3))
    // ```
    //
    // ...but how do we implement this as a recursive-descent algorithm?
    //
    // ## Basic Pratt Parsing
    //
    // Here's where the description of this method comes in:
    //
    // ```
    // `parse_inner`:
    // Parses an expression while the left binding power of the subsequent operator is higher than
    // the provided `min_bp`, returning an expression respecting operator-precedence.
    // ```
    //
    // We can pretty immediately see that, to have this function parse the entire expression, we can
    // just set `min_bp` to `0`—a binding power lower than the binding powers of all infix operators.
    //
    // But how does that let us handle precedence?
    //
    // Let's say you were in the process of parsing...
    //
    // ```
    // Items:       1   *   2   +   3
    // Powers:    0   2   3   1   2    0
    // ```
    //
    // Here's how we'd do it!
    //
    // - We start at the left with `min_bp = 0` and parse the first atom, which is the literal `1`.
    //
    // - Now, let's see if we can continue parsing this expression by peeking an infix operator.
    //   In this case, we find the `*` operator.
    //
    // - Since the left binding power of `*` is 2`—which is greater than `0`—we know that we can wrap
    //   our starting literal expression in a multiplication expression, with its left hand side being
    //   the literal `1`. But how do we parse the expression's right hand side?
    //
    // - Well, why don't we just ask the `parse_inner` function to parse another expression? That's
    //   its entire job after all. But, we can't just call `parse_inner` with the same `min_bp` of
    //   `0` because that would cause it to parse all the way to the end of the stream. This would
    //   include the `+ 3` part of the expression, which is clearly wrong since the addition should
    //   happen after we compute the result of the multiplication.
    //
    // - So, to get around this problem, we just ask the parser to parse with a `min_bp` of `3`
    //   instead. This ensures that the `+ 3` can be parsed by our call rather than its call since
    //   it will encounter the `+` operator, see that its left binding power is `1`—which is less
    //   than `3`—and stop parsing there, letting us parse it.
    //
    // Here's pseudo-code for this algorithm:
    //
    // ```
    // fn parse_inner(cursor, min_bp: u32) -> Expr {
    //     let mut expr = cursor.parse_atom();
    //
    //     loop {
    //          let op = cursor.try_peek_op();
    //          if !op.is_some_and(|op| left_bp(op) >= min_bp) {
    //              break expr;
    //          }
    //
    //          let lhs = expr;
    //          let op = cursor.parse_op();
    //          let rhs = parse_inner(cursor, right_bp(op));
    //
    //          expr = Expr { lhs, op, rhs };
    //     }
    // }
    // ```
    //
    // It's really that simple!
    //
    // But how do we ensure that the results adhere to the definition of precedence parsing above?
    //
    // ## Proof of Correctness
    //
    // We know that each call to `parse_inner` corresponds to the insertion of a left and right
    // parenthesis where that call starts and ends respectively.
    //
    // Let's start by considering why the algorithm's choice of when to close a parenthesis is always
    // correct. Recall that, to ensure correctness, we need to ensure that we only close a parenthesis
    // when the binding power to the left of a given atom is greater than the binding power to the
    // right of that atom. There are two cases to consider:
    //
    // 1. The atom under which we're terminating is the same atom we consumed at the start of
    //    `parse_inner` before repeating the loop. Because `min_bp` is given as the right binding
    //    power of the operator immediately proceeding it, this tells us that the atom's left binding
    //    power is strictly greater than the binding power of the operator to its right since
    //    `right_bp < (min_bp = left_bp)`.
    //
    // 2. The atom under which we're terminating is the last atom yielded by the recursive call to
    //    `parse_inner` during the previous loop iteration. It's important to note, however, that the
    //    last thing to consume atoms during that prior nested `parse_inner` invocation was a deeply
    //    nested call to `parse_inner` which consumes exactly one atom and never loops. This is because
    //    no other part of the `parse_inner` implementation can consume an atom besides it. Because
    //    this deeply nested call to `parse_inner` falls under the first case, its positional facts—
    //    notably, that `right_bp < (min_bp = left_bp)`—are inherited from it for free, thus implying
    //    that `right_bp < left_bp` by the time we return as well.
    //
    // Now, let's consider why the algorithm's choice of when to open a parenthesis is also always
    // correct. Recall that, to ensure correctness, we need to ensure that we only open a parenthesis
    // when the binding power to the left of a given atom is less than or equal to the binding power
    // to the right of that atom. Because this cannot be satisfied for calls to `parse_inner` which
    // only consume a single atom, we'll exclude those. This is fine because adding a parenthesis
    // there is not necessary.
    //
    // This condition is satisfied somewhat immediately by the fact that `parse_inner` can only parse
    // beyond its first atom if the left binding power of the operator to its right is greater than
    // `min_bp`, which is equal to the right binding power of the operator to the starting atom's
    // left.
    //
    // Hence, Pratt parsers properly follow our intuition for how binding powers specify the direction
    // of parentheses.
    //
    // [matklad-pratt]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn parse_inner(c: &mut TokenSequence, min_bp: u32) -> Self {
        let mut expr = Self::parse_atom(c);

        loop {
            let Some(kind) = c.expect(Symbol!("binary operator"), |c| {
                Self::match_bin_op(c).filter(|&op| Self::bin_binding_power(op).0 >= min_bp)
            }) else {
                break;
            };

            let lhs = expr;
            let rhs = Self::parse_inner(c, Self::bin_binding_power(kind).1);
            expr = AstBinOpExpr { lhs, kind, rhs }.into();
        }

        expr
    }

    fn match_bin_op(c: &mut TokenCursor) -> Option<AstBinOpKind> {
        // Parse multi-puncts
        if match_puncts(c, &[punct!('&'), punct!('&')]).is_some() {
            return Some(AstBinOpKind::ShortAnd);
        }

        if match_puncts(c, &[punct!('|'), punct!('|')]).is_some() {
            return Some(AstBinOpKind::ShortOr);
        }

        // Parse single puncts
        if let Some(ch) = c.lookahead(|c| {
            c.next()
                .and_then(Token::as_punct)
                .and_then(|punct| match punct.char {
                    PunctChar::Plus => Some(AstBinOpKind::Add),
                    PunctChar::Minus => Some(AstBinOpKind::Sub),
                    PunctChar::Asterisk => Some(AstBinOpKind::Mul),
                    PunctChar::Slash => Some(AstBinOpKind::Div),
                    PunctChar::Percent => Some(AstBinOpKind::Rem),
                    PunctChar::Caret => Some(AstBinOpKind::BitXor),
                    PunctChar::Bar => Some(AstBinOpKind::BitOr),
                    PunctChar::Ampersand => Some(AstBinOpKind::BitAnd),
                    _ => None,
                })
        }) {
            return Some(ch);
        }

        None
    }

    fn bin_binding_power(op: AstBinOpKind) -> (u32, u32) {
        use AstBinOpKind::*;

        match op {
            Add | Sub => (1, 2),
            Mul | Div | Rem => (3, 4),
            _ => todo!(),
        }
    }

    fn parse_atom(c: &mut TokenSequence) -> Self {
        if parse_punct(c, punct!('-')).is_some() {
            AstUnaryNegExpr {
                expr: Self::parse_atom_base_with_post(c),
            }
            .into()
        } else {
            Self::parse_atom_base_with_post(c)
        }
    }

    fn parse_atom_base_with_post(c: &mut TokenSequence) -> Self {
        let mut expr = Self::parse_atom_base(c);

        loop {
            // Match member access
            if parse_punct(c, punct!('.')).is_some() {
                let Some(member) = parse_identifier(c, Symbol!("member name")) else {
                    c.stuck(|_| ());
                    continue;
                };

                expr = AstDotExpr {
                    expr,
                    member: member.text,
                }
                .into();
                continue;
            }

            // Match calls
            if let Some(group) = parse_group(c, GroupDelimiter::Paren) {
                let mut c = c.enter(group.cursor());
                let mut args = Vec::new();

                loop {
                    // Match EOS
                    if c.expect(Symbol!("`)`"), |c| c.next().is_none()) {
                        break;
                    }

                    // Match expression
                    args.push(AstExpr::parse(&mut c));

                    // Match comma
                    if parse_punct(&mut c, punct!(',')).is_none() {
                        if !c.expect(Symbol!("`)`"), |c| c.next().is_none()) {
                            c.stuck(|_| ());
                        }

                        break;
                    }

                    continue;
                }

                expr = AstCallExpr {
                    expr,
                    args: Box::from_iter(args),
                }
                .into();

                continue;
            }

            break;
        }

        expr
    }

    fn parse_atom_base(c: &mut TokenSequence) -> Self {
        // Match parenthesized expression
        if let Some(group) = parse_group(c, GroupDelimiter::Paren) {
            // Parse a sequence of expressions
            let mut c = c.enter(group.cursor());
            let mut items = SmallVec::<[AstExpr; 1]>::new();

            loop {
                // Match and EOS
                //
                // If we found an EOS after a comma (or on our first iteration),
                // we know that this is a tuple.
                if c.expect(Symbol!("`)`"), |c| c.next().is_none()) {
                    return AstTupleExpr {
                        items: Box::from_iter(items),
                    }
                    .into();
                }

                // Match an expression
                items.push(AstExpr::parse(&mut c));

                // Match the delimiting comma
                if parse_punct(&mut c, punct!(',')).is_none() {
                    // If it's missing, match the EOS
                    if !c.expect(Symbol!("`)`"), |c| c.next().is_none()) {
                        c.stuck(|_| ());
                    }

                    return if items.len() == 1 {
                        AstParenExpr {
                            expr: items.into_iter().nth(0).unwrap(),
                        }
                        .into()
                    } else {
                        AstTupleExpr {
                            items: Box::from_iter(items),
                        }
                        .into()
                    };
                }
            }
        }

        // Match path
        {
            let path = AstPath::parse(c);
            if !path.is_empty() {
                // Match structure creation
                if let Some(group) = parse_group(c, GroupDelimiter::Brace) {
                    let mut c = c.enter(group.cursor());

                    let mut fields = Vec::new();

                    loop {
                        // Match field name
                        let Some(field) = parse_identifier(&mut c, Symbol!("field name")) else {
                            break;
                        };

                        // Try to match explicit value
                        let value = if parse_punct(&mut c, punct!(':')).is_some() {
                            AstExpr::parse(&mut c)
                        } else {
                            AstPathExpr {
                                path: AstPath::new_local(field),
                            }
                            .into()
                        };

                        fields.push((field, value));

                        // Match comma
                        if parse_punct(&mut c, punct!(',')).is_none() {
                            break;
                        }
                    }

                    // Match EOS
                    if !c.expect(Symbol!("`}`"), |c| c.next().is_none()) {
                        c.stuck(|_| ());
                    }

                    return AstCtorExpr { item: path, fields }.into();
                }

                return AstPathExpr { path }.into();
            }
        }

        // Match literals
        if let Some(lit) = parse_str_lit(c, Symbol!("string literal")) {
            return AstLiteralExpr::String(*lit).into();
        }

        if let Some(lit) = parse_char_lit(c, Symbol!("character literal")) {
            return AstLiteralExpr::Char(*lit).into();
        }

        if let Some(lit) = parse_number_lit(c, Symbol!("numeric literal")) {
            return AstLiteralExpr::Number(*lit).into();
        }

        if let Some(ident) = parse_keyword(c, AstKeyword::True) {
            return AstLiteralExpr::Bool(ident.span, true).into();
        }

        if let Some(ident) = parse_keyword(c, AstKeyword::False) {
            return AstLiteralExpr::Bool(ident.span, false).into();
        }

        c.stuck(|_| ());
        AstExpr::Placeholder
    }
}
