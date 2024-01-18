use crate::util::span::Span;

use aunty::make_extensible;

// === DiagnosticReporter === //

#[derive(Debug, Default)]
pub struct DiagnosticReporter {
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
}

make_extensible!(pub DiagnosticReporterObj for DiagnosticReporter);

impl DiagnosticReporter {
    pub fn report(&mut self, diagnostic: Diagnostic) {
        if diagnostic.kind == DiagnosticKind::Error {
            self.has_errors = true;
        }

        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

impl DiagnosticReporterObj {
    pub fn report(&self, diagnostic: Diagnostic) {
        self.obj.get_mut().report(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.obj.get().has_errors()
    }
}

// === Diagnostic === //

pub const NO_CODE: u32 = u32::MAX;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub code: u32,
    pub message: String,
    pub offending_span: Option<Span>,
    pub windows: Vec<DiagnosticWindow>,
    pub subs: Vec<Diagnostic>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            code: NO_CODE,
            message: message.into(),
            offending_span: None,
            windows: Vec::new(),
            subs: Vec::new(),
        }
    }

    pub fn span_err(span: Span, message: impl Into<String>) -> Self {
        Self::new(DiagnosticKind::Error, message).with_offending_span(span)
    }

    pub fn with_offending_span(mut self, span: Span) -> Self {
        self.offending_span = Some(span);
        self
    }

    pub fn with_code(mut self, code: u32) -> Self {
        self.code = code;
        self
    }

    pub fn with_window(mut self, span: Span, label: Option<impl Into<String>>) -> Self {
        self.windows.push(DiagnosticWindow {
            span,
            label: label.map(Into::into),
        });
        self
    }

    pub fn with_sub(mut self, sub: Diagnostic) -> Self {
        self.subs.push(sub);
        self
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum DiagnosticKind {
    Error,
    Warn,
    Info,
    Note,
    Help,
}

#[derive(Debug, Clone)]
pub struct DiagnosticWindow {
    pub span: Span,
    pub label: Option<String>,
}
