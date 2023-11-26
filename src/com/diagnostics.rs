use colored::Colorize;

use super::{
    abt::TypeAbt,
    pos::Pos,
    span::Span,
    tokens::{Token, TokenKind},
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

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
    pub severity: Severity,
    pub span: Span,
}

// type-state builder
pub struct DiagnosticBuilder<Ki, Sev, Sp> {
    kind: Ki,
    severity: Sev,
    span: Sp,
}

pub fn create_diagnostic() -> DiagnosticBuilder<(), (), ()> {
    DiagnosticBuilder {
        kind: (),
        severity: (),
        span: (),
    }
}

impl<Sev, Sp> DiagnosticBuilder<(), Sev, Sp> {
    pub fn with_kind(self, kind: DiagnosticKind) -> DiagnosticBuilder<DiagnosticKind, Sev, Sp> {
        DiagnosticBuilder {
            kind,
            severity: self.severity,
            span: self.span,
        }
    }
}

impl<Ki, Sp> DiagnosticBuilder<Ki, (), Sp> {
    pub fn with_severity(self, severity: Severity) -> DiagnosticBuilder<Ki, Severity, Sp> {
        DiagnosticBuilder {
            kind: self.kind,
            severity,
            span: self.span,
        }
    }
}

impl<Ki, Sev> DiagnosticBuilder<Ki, Sev, ()> {
    pub fn with_span(self, span: Span) -> DiagnosticBuilder<Ki, Sev, Span> {
        DiagnosticBuilder {
            kind: self.kind,
            severity: self.severity,
            span,
        }
    }

    pub fn with_pos(self, pos: Pos) -> DiagnosticBuilder<Ki, Sev, Span> {
        self.with_span(Span::at(pos))
    }
}

impl DiagnosticBuilder<DiagnosticKind, Severity, Span> {
    pub fn done(self) -> Diagnostic {
        Diagnostic {
            kind: self.kind,
            severity: self.severity,
            span: self.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    IllegalCharacter(char),
    ExpectedToken { found: Token, expected: TokenKind },
    ExpectedExpression,
    ExpectedStatement,
    ExpectedType,

    GuardNotBoolean,

    EmptyThenStatement,
    EmptyElseStatement,
    ThenWithoutIf,
    ElseWithoutIfThen,

    EmptyWhileDoStatement,
    EmptyDoWhileStatement,
    DoWithoutWhile,

    UnknownVariable(String),
    AssigneeMustBeVariable,

    TypeMismatch { found: TypeAbt, expected: TypeAbt },
}

#[rustfmt::skip]
impl DiagnosticKind {
    pub fn msg(&self) -> String {
        match self {
            DiagnosticKind::IllegalCharacter(ill)
                => format!("encountered illegal character {}",
                    format!("{ill:?}").bold()
                ),
            DiagnosticKind::ExpectedToken { found, expected }
                => format!("expected {}, but found {}",
                    expected.to_string().bold(),
                    found.to_string().bold()
                ),
            DiagnosticKind::ExpectedExpression
                => "expected an expression".to_string(),
            DiagnosticKind::ExpectedStatement
                => "expected a statement".to_string(),
            DiagnosticKind::ExpectedType
                => "expected a type expression".to_string(),
            DiagnosticKind::GuardNotBoolean
                => "guard is not a boolean".to_string(),
            DiagnosticKind::EmptyThenStatement
                => "empty then statement".to_string(),
            DiagnosticKind::EmptyElseStatement
                => "empty else statement".to_string(),
            DiagnosticKind::ThenWithoutIf
                => "then statement without if".to_string(),
            DiagnosticKind::ElseWithoutIfThen
                => "else statement without if-then".to_string(),
            DiagnosticKind::EmptyWhileDoStatement
                => "empty while-do statement".to_string(),
            DiagnosticKind::EmptyDoWhileStatement
                => "empty do-while statement".to_string(),
            DiagnosticKind::DoWithoutWhile
                => "do statement without while".to_string(),
            DiagnosticKind::UnknownVariable(name)
                => format!("unknown variable '{}'", name.bold()),
            DiagnosticKind::AssigneeMustBeVariable
                => "assignee must be a variable".to_string(),
            DiagnosticKind::TypeMismatch { found, expected }
                => format!("type mismatch of {} into {}",
                    found.to_string().bold(),
                    expected.to_string().bold()
                ),
        }
    }
}
