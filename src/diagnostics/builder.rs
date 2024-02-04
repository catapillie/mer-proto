use super::{Diagnostic, DiagnosticKind, Note, NoteSeverity, Severity};
use crate::utils::{Pos, Span};

pub struct DiagnosticBuilder<Ki, Sev, Sp> {
    kind: Ki,
    severity: Sev,
    span: Sp,
    notes: Vec<(Span, Note, NoteSeverity)>,
}

pub fn create_diagnostic() -> DiagnosticBuilder<(), (), ()> {
    DiagnosticBuilder {
        kind: (),
        severity: (),
        span: (),
        notes: Default::default(),
    }
}

impl<Ki, Sev, Sp> DiagnosticBuilder<Ki, Sev, Sp> {
    pub fn annotate_primary(mut self, note: Note, span: Span) -> Self {
        self.notes.push((span, note, NoteSeverity::Default));
        self
    }

    pub fn annotate_secondary(mut self, note: Note, span: Span, severity: NoteSeverity) -> Self {
        self.notes.push((span, note, severity));
        self
    }
}

impl<Sev, Sp> DiagnosticBuilder<(), Sev, Sp> {
    pub fn with_kind(self, kind: DiagnosticKind) -> DiagnosticBuilder<DiagnosticKind, Sev, Sp> {
        DiagnosticBuilder {
            kind,
            severity: self.severity,
            span: self.span,
            notes: self.notes,
        }
    }
}

impl<Ki, Sp> DiagnosticBuilder<Ki, (), Sp> {
    pub fn with_severity(self, severity: Severity) -> DiagnosticBuilder<Ki, Severity, Sp> {
        DiagnosticBuilder {
            kind: self.kind,
            severity,
            span: self.span,
            notes: self.notes,
        }
    }
}

impl<Ki, Sev> DiagnosticBuilder<Ki, Sev, ()> {
    pub fn without_span(self) -> DiagnosticBuilder<Ki, Sev, Option<Span>> {
        DiagnosticBuilder {
            kind: self.kind,
            severity: self.severity,
            span: None,
            notes: self.notes,
        }
    }

    pub fn with_span(self, span: Span) -> DiagnosticBuilder<Ki, Sev, Option<Span>> {
        DiagnosticBuilder {
            kind: self.kind,
            severity: self.severity,
            span: Some(span),
            notes: self.notes,
        }
    }

    pub fn with_pos(self, pos: Pos) -> DiagnosticBuilder<Ki, Sev, Option<Span>> {
        self.with_span(Span::at(pos))
    }
}

impl DiagnosticBuilder<DiagnosticKind, Severity, Option<Span>> {
    pub fn done(self) -> Diagnostic {
        Diagnostic {
            kind: self.kind,
            severity: self.severity,
            span: self.span,
            annotations: self.notes,
        }
    }
}
