use super::{
    span::Span,
    tokens::{Token, TokenKind}, pos::Pos,
};

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn done(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub span: Span,
}

// type-state builder
pub struct NoKind;
pub struct WithKind(DiagnosticKind);
pub struct NoSpan;
pub struct WithSpan(Span);
pub struct DiagnosticBuilder<K, S> {
    kind: K,
    span: S,
}

pub fn create_diagnostic() -> DiagnosticBuilder<NoKind, NoSpan> {
    DiagnosticBuilder {
        kind: NoKind,
        span: NoSpan,
    }
}

impl<S> DiagnosticBuilder<NoKind, S> {
    pub fn with_kind(self, kind: DiagnosticKind) -> DiagnosticBuilder<WithKind, S> {
        DiagnosticBuilder {
            kind: WithKind(kind),
            span: self.span,
        }
    }
}

impl<K> DiagnosticBuilder<K, NoSpan> {
    pub fn with_span(self, span: Span) -> DiagnosticBuilder<K, WithSpan> {
        DiagnosticBuilder {
            kind: self.kind,
            span: WithSpan(span),
        }
    }

    pub fn with_pos(self, pos: Pos) -> DiagnosticBuilder<K, WithSpan> {
        self.with_span(Span::at(pos))
    }
}

impl DiagnosticBuilder<WithKind, WithSpan> {
    pub fn done(self) -> Diagnostic {
        Diagnostic {
            kind: self.kind.0,
            span: self.span.0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    IllegalCharacter(char),
    ExpectedToken { found: Token, expected: TokenKind },
    ExpectedExpression,
    ExpectedStatement,
}

#[rustfmt::skip]
impl DiagnosticKind {
    pub fn msg(&self) -> String {
        match self {
            DiagnosticKind::IllegalCharacter(ill)
                => format!("encountered illegal character {ill:?}"),
            DiagnosticKind::ExpectedToken { found, expected }
                => format!("expected {expected}, but found {found}"),
            DiagnosticKind::ExpectedExpression
                => "expected an expression".to_string(),
            DiagnosticKind::ExpectedStatement
                => "expected a statement".to_string(),
        }
    }
}
