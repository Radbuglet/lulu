use std::{
    cell::{Cell, RefCell},
    fmt,
    thread::panicking,
};

use aunty::Obj;
use rustc_hash::FxHashSet;

use super::{
    diag::{Diagnostic, DiagnosticReporter},
    intern::Intern,
    span::{FileLoc, Span},
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
    while_parsing: RefCell<Vec<(FileLoc, Intern)>>,
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
            stuck_hints: Vec::new(),
        }
    }

    pub fn while_parsing(&self, starting_at: FileLoc, what: Intern) -> WhileParsingGuard<'_> {
        self.while_parsing.borrow_mut().push((starting_at, what));

        WhileParsingGuard {
            cx: self,
            top: what,
        }
    }

    pub fn got_stuck(&self) -> bool {
        self.got_stuck.get()
    }

    pub fn diagnostics(&self) -> &Obj<DiagnosticReporter> {
        &self.diagnostics
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
            assert!(matches!(popped, Some((_, v)) if v == self.top));
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseSequence<'cx, I> {
    cx: &'cx ParseContext,
    cursor: I,
    expectations: Vec<Intern>,
    stuck_hints: Vec<String>,
}

impl<'cx, I> ParseSequence<'cx, I> {
    pub fn enter<I2>(&self, cursor: I2) -> ParseSequence<'cx, I2> {
        self.cx.enter(cursor)
    }

    pub fn while_parsing(&self, what: Intern) -> WhileParsingGuard<'cx>
    where
        I: ParseCursor,
    {
        self.cx.while_parsing(self.next_span().start(), what)
    }

    fn moved_forward(&mut self) {
        self.expectations.clear();
        self.stuck_hints.clear();
    }

    pub fn irrefutable<R>(&mut self, f: impl FnOnce(&mut I) -> R) -> R {
        self.moved_forward();
        f(&mut self.cursor)
    }

    pub fn expect<R>(&mut self, expectation: Intern, f: impl FnOnce(&mut I) -> R) -> R
    where
        I: ParseCursor,
        R: LookaheadResult,
    {
        let res = self.cursor.lookahead(f);
        if res.is_truthy() {
            self.moved_forward();
        } else {
            self.expectations.push(expectation);
        }
        res
    }

    pub fn hint_stuck_if_passes<R>(
        &mut self,
        reason: impl fmt::Display,
        f: impl FnOnce(&mut I) -> R,
    ) where
        I: ParseCursor,
        R: LookaheadResult,
    {
        if f(&mut self.cursor.clone()).is_truthy() {
            self.hint_stuck(reason.to_string());
        }
    }

    pub fn hint_stuck(&mut self, reason: impl Into<String>) {
        self.stuck_hints.push(reason.into());
    }

    pub fn stuck_lookahead<R>(&mut self, recover: impl FnOnce(&mut I) -> R)
    where
        I: ParseCursor,
        R: LookaheadResult,
    {
        self.stuck(|c| {
            c.lookahead(recover);
        });
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

        if expectations.is_empty() {
            expectations.push("<nothing?>".to_string());
        }

        if let Some(last) = (expectations.len() > 1)
            .then(|| expectations.last_mut())
            .flatten()
        {
            last.insert_str(0, "or ");
        }
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
                        .map(|(loc, what)| format!("{what} starting at {}", loc.line_and_column()))
                        .collect::<Vec<_>>()
                        .join(" in ")
                )
            }
        };

        self.diagnostics().report({
            let mut diagnostic =
                Diagnostic::span_err(span, format!("expected {expectations}{while_parsing}"));

            for hint in &self.stuck_hints {
                diagnostic.subs.push(Diagnostic::new_note(hint.clone()));
            }

            diagnostic
        });

        // Attempt to get unstuck
        recover(&mut self.cursor);

        // Clear expectation set
        self.moved_forward();
    }

    pub fn error(&mut self, diagnostic: impl Into<Diagnostic>, recover: impl FnOnce(&mut I)) {
        self.cx.got_stuck.set(true);
        self.diagnostics().report(diagnostic.into());
        recover(&mut self.cursor);
    }

    pub fn next_span(&self) -> Span
    where
        I: ParseCursor,
    {
        self.cursor.next_span()
    }

    pub fn context(&self) -> &'cx ParseContext {
        self.cx
    }

    pub fn diagnostics(&self) -> &Obj<DiagnosticReporter> {
        &self.cx.diagnostics
    }
}

// === ParseCursor === //

pub trait ForkableCursor: Iterator + Clone {
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
}

pub trait ParseCursor: ForkableCursor {
    fn next_span(&self) -> Span;
}
