use colored::Colorize;

use super::{
    abt::TypeAbt,
    ast::{BinOpAst, UnOpAst},
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
    pub span: Option<Span>,
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
    pub fn without_span(self) -> DiagnosticBuilder<Ki, Sev, Option<Span>> {
        DiagnosticBuilder {
            kind: self.kind,
            severity: self.severity,
            span: None,
        }
    }

    pub fn with_span(self, span: Span) -> DiagnosticBuilder<Ki, Sev, Option<Span>> {
        DiagnosticBuilder {
            kind: self.kind,
            severity: self.severity,
            span: Some(span),
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    IllegalCharacter(char),
    ExpectedToken {
        found: Token,
        expected: TokenKind,
    },
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
    TooManyVariables,

    UnknownFunction(String),
    InvalidParameterCount {
        got: usize,
        expected: usize,
    },

    UnknownType(String),

    TypeMismatch {
        found: TypeAbt,
        expected: TypeAbt,
    },

    InvalidUnaryOperation {
        op: UnOpAst,
        ty: TypeAbt,
    },
    InvalidBinaryOperation {
        op: BinOpAst,
        left: TypeAbt,
        right: TypeAbt,
    },

    MustReturnValue {
        expected: TypeAbt,
    },
    NotAllPathsReturn,
    TopLevelMustReturn,

    UnreachableCode,
}

#[rustfmt::skip]
impl DiagnosticKind {
    pub fn msg(&self) -> String {
        match self {
            DiagnosticKind::IllegalCharacter(ill)
                => format!("encountered illegal character {}",
                    format!("{ill:?}").bold(),
                ),
            DiagnosticKind::ExpectedToken { found, expected }
                => format!("expected '{}', but found '{}'",
                    expected.to_string().bold(),
                    found.to_string().bold(),
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
            DiagnosticKind::TooManyVariables
                => "too many local variables".to_string(),
            DiagnosticKind::UnknownFunction(name)
                => format!("unknown function '{}'", name.bold()),
            DiagnosticKind::InvalidParameterCount { got, expected }
                => format!("function takes in {} but was supplied {} parameters",
                    expected.to_string().bold(),
                    got.to_string().bold()
                ),
            DiagnosticKind::UnknownType(id)
                => format!("unkown type '{}'", id.bold()),
            DiagnosticKind::TypeMismatch { found, expected }
                => format!("type mismatch of '{}' into '{}'",
                    found.to_string().bold(),
                    expected.to_string().bold(),
                ),
            DiagnosticKind::InvalidUnaryOperation { op, ty }
                => format!("invalid unary operation '{}' '{}'",
                    op.to_string().bold(),
                    ty.to_string().bold(),
                ),
            DiagnosticKind::InvalidBinaryOperation { op, left, right }
                => format!("invalid binary operation ('{}' {} '{}')",
                    left.to_string().bold(),
                    op.to_string().bold(),
                    right.to_string().bold(),
                ),
            DiagnosticKind::MustReturnValue { expected }
                => format!("must return a value of type '{}'", expected.to_string().bold()),
            DiagnosticKind::NotAllPathsReturn
                => "not every path is guaranteed to return".to_string(),
            DiagnosticKind::TopLevelMustReturn
                => "the top level program must return unit".to_string(),
            DiagnosticKind::UnreachableCode
                => "this code is unreachable".to_string(),
        }
    }
}
