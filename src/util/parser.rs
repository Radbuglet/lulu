use std::{
    cell::{Cell, RefCell},
    thread::panicking,
};

use aunty::Obj;
use rustc_hash::FxHashSet;

use super::{
    diag::{Diagnostic, DiagnosticKind, DiagnosticReporter},
    span::{Intern, Span},
};

// === LookaheadResult === //

pub trait LookaheadResult {
    fn is_truthy(&self) -> bool;
}

impl LookaheadResult for bool {
    fn is_truthy(&self) -> bool {
        *self
    }
}

impl<T> LookaheadResult for Option<T> {
    fn is_truthy(&self) -> bool {
        self.is_some()
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    fn is_truthy(&self) -> bool {
        self.is_ok()
    }
}

// === ParseContext === //

#[derive(Debug, Clone)]
pub struct ParseContext {
    diagnostics: Obj<DiagnosticReporter>,
    while_parsing: RefCell<Vec<Intern>>,
    got_stuck: Cell<bool>,
}

impl ParseContext {
    pub fn new(diagnostics: Obj<DiagnosticReporter>) -> Self {
        Self {
            diagnostics,
            while_parsing: RefCell::default(),
            got_stuck: Cell::new(false),
        }
    }

    pub fn enter<I>(&self, cursor: I) -> ParseSequence<'_, I> {
        ParseSequence {
            cx: self,
            cursor,
            expectations: Vec::new(),
        }
    }

    pub fn while_parsing(&self, what: Intern) -> WhileParsingGuard<'_> {
        self.while_parsing.borrow_mut().push(what);

        WhileParsingGuard {
            cx: self,
            top: what,
        }
    }

    pub fn got_stuck(&self) -> bool {
        self.got_stuck.get()
    }
}

#[derive(Debug, Clone)]
#[must_use]
pub struct WhileParsingGuard<'c> {
    cx: &'c ParseContext,
    top: Intern,
}

impl Drop for WhileParsingGuard<'_> {
    fn drop(&mut self) {
        let popped = self.cx.while_parsing.borrow_mut().pop();
        if !panicking() {
            assert_eq!(popped, Some(self.top));
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseSequence<'cx, I> {
    cx: &'cx ParseContext,
    cursor: I,
    expectations: Vec<Intern>,
}

impl<'cx, I> ParseSequence<'cx, I> {
    pub fn enter<I2>(&self, cursor: I2) -> ParseSequence<'cx, I2> {
        self.cx.enter(cursor)
    }

    pub fn while_parsing(&self, what: Intern) -> WhileParsingGuard<'cx> {
        self.cx.while_parsing(what)
    }

    pub fn expect<R: LookaheadResult>(
        &mut self,
        expectation: Intern,
        f: impl FnOnce(&mut I) -> R,
    ) -> R
    where
        I: ParseCursor,
    {
        let res = self.cursor.lookahead(f);
        if res.is_truthy() {
            self.expectations.clear();
        } else {
            self.expectations.push(expectation);
        }
        res
    }

    pub fn stuck(&mut self, recover: impl FnOnce(&mut I))
    where
        I: ParseCursor,
    {
        // Mark that we got stuck
        self.cx.got_stuck.set(true);

        // Emit the error message
        let span = self.cursor.next_span();

        let expectations = self.expectations.iter().copied().collect::<FxHashSet<_>>();
        let mut expectations = expectations
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        expectations.sort_unstable();

        let expectations = expectations.join(", ");

        let while_parsing = {
            let stack = self.cx.while_parsing.borrow();
            if stack.is_empty() {
                String::new()
            } else {
                format!(
                    " while parsing {}",
                    stack
                        .iter()
                        .rev()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(" in ")
                )
            }
        };

        self.cx.diagnostics.report(
            Diagnostic::new(
                DiagnosticKind::Error,
                format!("expected {expectations}{while_parsing}"),
            )
            .with_offending_span(span),
        );

        // Attempt to get unstuck
        recover(&mut self.cursor);
    }
}

// === ParseCursor === //

pub trait ParseCursor: Iterator + Clone {
    fn peek(&self) -> Option<Self::Item> {
        self.clone().next()
    }

    fn lookahead<R: LookaheadResult>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let mut fork = self.clone();
        let res = f(&mut fork);
        if res.is_truthy() {
            *self = fork;
        }

        res
    }

    fn next_span(&self) -> Span;
}
