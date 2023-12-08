use std::num::ParseIntError;

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

    InvalidInteger(ParseIntError),
    InvalidFloat,
    MissingLeadingDigits,
    MissingTrailingDigits,

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
            Self::IllegalCharacter(ill)
                => format!("encountered illegal character {}",
                    format!("{ill:?}").bold(),
                ),
            Self::InvalidInteger(e)
                => format!("invalid integer literal ({e})"),
            Self::InvalidFloat
                => format!("invalid float literal"),
            Self::MissingLeadingDigits
                => format!("float literals must have leading digits"),
            Self::MissingTrailingDigits
                => format!("float literals must have trailing digits"),
            Self::ExpectedToken { found, expected }
                => format!("expected '{}', but found '{}'",
                    expected.to_string().bold(),
                    found.to_string().bold(),
                ),
            Self::ExpectedExpression
                => "expected an expression".to_string(),
            Self::ExpectedStatement
                => "expected a statement".to_string(),
            Self::ExpectedType
                => "expected a type expression".to_string(),
            Self::GuardNotBoolean
                => "guard is not a boolean".to_string(),
            Self::EmptyThenStatement
                => "empty then statement".to_string(),
            Self::EmptyElseStatement
                => "empty else statement".to_string(),
            Self::ThenWithoutIf
                => "then statement without if".to_string(),
            Self::ElseWithoutIfThen
                => "else statement without if-then".to_string(),
            Self::EmptyWhileDoStatement
                => "empty while-do statement".to_string(),
            Self::EmptyDoWhileStatement
                => "empty do-while statement".to_string(),
            Self::DoWithoutWhile
                => "do statement without while".to_string(),
            Self::UnknownVariable(name)
                => format!("unknown variable '{}'", name.bold()),
            Self::AssigneeMustBeVariable
                => "assignee must be a variable".to_string(),
            Self::TooManyVariables
                => "too many local variables".to_string(),
            Self::UnknownFunction(name)
                => format!("unknown function '{}'", name.bold()),
            Self::InvalidParameterCount { got, expected }
                => format!("function takes in {} but was supplied {} parameters",
                    expected.to_string().bold(),
                    got.to_string().bold()
                ),
            Self::UnknownType(id)
                => format!("unkown type '{}'", id.bold()),
            Self::TypeMismatch { found, expected }
                => format!("type mismatch of '{}' into '{}'",
                    found.to_string().bold(),
                    expected.to_string().bold(),
                ),
            Self::InvalidUnaryOperation { op, ty }
                => format!("invalid unary operation '{}' '{}'",
                    op.to_string().bold(),
                    ty.to_string().bold(),
                ),
            Self::InvalidBinaryOperation { op, left, right }
                => format!("invalid binary operation ('{}' {} '{}')",
                    left.to_string().bold(),
                    op.to_string().bold(),
                    right.to_string().bold(),
                ),
            Self::MustReturnValue { expected }
                => format!("must return a value of type '{}'", expected.to_string().bold()),
            Self::NotAllPathsReturn
                => "not every path is guaranteed to return".to_string(),
            Self::TopLevelMustReturn
                => "the top level program must return unit".to_string(),
            Self::UnreachableCode
                => "this code is unreachable".to_string(),
        }
    }
}
