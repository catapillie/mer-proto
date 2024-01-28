use std::num::ParseIntError;

use colored::Colorize;

use crate::utils::{Pos, Span};

use super::{
    abt::TypeAbt,
    syntax::{bin_op::BinOpAst, un_op::UnOpAst},
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

    pub fn is_fatal(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
    }
}

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub severity: Severity,
    pub span: Option<Span>,
    pub annotations: Vec<(Span, Note, NoteSeverity)>,
}

// type-state builder
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
    SingletonTypeSyntax,

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
    TooManyVariables(String, usize),
    TooManyTopLevelVariables(usize),
    UnusedVariable(String),

    UnknownFunction(String),
    InvalidArgCount {
        got: usize,
        expected: usize,
    },
    InvalidCallee,

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

    UnallowedVariableCapture {
        func_name: String,
        var_name: String,
    },

    InvalidDebugExpression(TypeAbt),

    InvalidDereference(TypeAbt),

    InvalidImmediateIndex,
    InvalidTupleIndex {
        len: usize,
        accessed: usize,
    },

    EmptyArray,
    SingletonArray,
    ArrayMismatchingTypes,
    OutOfRangeArrayIndex {
        len: usize,
        index: usize,
    },
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
                => "invalid float literal".to_string(),
            Self::MissingLeadingDigits
                => "float literals must have leading digits".to_string(),
            Self::MissingTrailingDigits
                => "float literals must have trailing digits".to_string(),
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
            Self::SingletonTypeSyntax
                => "singleton type is read as its inner type, so parentheses are unnecessary".to_string(),
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
            Self::TooManyVariables(name, count)
                => format!("function '{}' uses {} variables, which is more than the allowed maximum (255)",
                    name.bold(),
                    count.to_string().bold(),
                ),
            Self::TooManyTopLevelVariables(count)
                => format!("the top level program uses {} variables, which is more than the allowed maximum (255)",
                    count.to_string().bold(),
                ),
            Self::UnusedVariable(name)
                => format!("variable '{}' is never used",
                    name.bold(),
                ),
            Self::UnknownFunction(name)
                => format!("unknown function '{}'", name.bold()),
            Self::InvalidArgCount { got, expected }
                => format!("function takes in {} arguments, but was supplied {}",
                    expected.to_string().bold(),
                    got.to_string().bold()
                ),
            Self::InvalidCallee
                => "callee is not a function, and cannot be called".to_string(),
            Self::UnknownType(id)
                => format!("unknown type '{}'", id.bold()),
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
            Self::UnallowedVariableCapture { func_name, var_name }
                => format!("function '{}' captures variable '{}', which is not (yet) allowed",
                    func_name.bold(),
                    var_name.bold(),
                ),
            Self::InvalidDebugExpression(ty)
                => format!("cannot debug value of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::InvalidDereference(ty)
                => format!("cannot dereference value of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::InvalidImmediateIndex
                => "cannot index a value which is neither a tuple, nor an array of known size".to_string(),
            Self::InvalidTupleIndex { len, accessed }
                => format!("cannot access value {} of a tuple with {} values",
                    accessed.to_string().bold(),
                    len.to_string().bold(),
                ),
            Self::EmptyArray
                => "empty arrays are not allowed".to_string(),
            Self::SingletonArray
                => "singleton arrays are equivalent to their inner value".to_string(),
            Self::ArrayMismatchingTypes
                => "values in array must all be of the same type".to_string(),
            Self::OutOfRangeArrayIndex { len, index }
                => format!("cannot access value {} of an array of size {}",
                    index.to_string().bold(),
                    len.to_string().bold(),
                ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Note {
    Numbered(usize, Box<Note>),
    Then(Box<Note>),
    But(Box<Note>),
    So(Box<Note>),
    DDDotFront(Box<Note>),
    DDDotBack(Box<Note>),
    Quiet,
    Here,
    Unknown,
    LeadingDigits,
    TrailingDigits,
    ExpectedToken(TokenKind),
    CanBeRemoved,
    CanRemoveParentheses,
    FollowsIf,
    FollowsIfThen,
    MissingWhile,
    CannotAssign,
    MustBeOfType(TypeAbt),
    OfType(TypeAbt),
    VariableDeclaration(String),
    VariableType(String, TypeAbt),
    ArgumentType(String, TypeAbt),
    NotFunction(TypeAbt),
    FunctionArgs(String, usize),
    FunctionReturnType(String, TypeAbt),
    FunctionVariableCount(usize),
    ProvidedArgs(usize),
    VariableCapturedBy(String, String),
    TupleValueCount(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum NoteSeverity {
    Default,
    Annotation,
}

#[rustfmt::skip]
impl Note {
    pub fn num(self, num: usize) -> Self {
        Note::Numbered(num, Box::new(self))
    }

    pub fn then(self) -> Self {
        Note::Then(Box::new(self))
    }

    pub fn but(self) -> Self {
        Note::But(Box::new(self))
    }

    pub fn so(self) -> Self {
        Note::So(Box::new(self))
    }

    pub fn dddot_front(self) -> Self {
        Note::DDDotFront(Box::new(self))
    }

    pub fn dddot_back(self) -> Self {
        Note::DDDotBack(Box::new(self))
    }

    pub fn msg(&self) -> String {
        match self {
            Self::Quiet
                => String::new(),
            Self::Numbered(num, note)
                => format!("{} {}", format!("[{num}]").bold(), note.msg()),
            Self::Then(note)
                => format!("then {}", note.msg()),
            Self::But(note)
                => format!("but {}", note.msg()),
            Self::So(note)
                => format!("so {}", note.msg()),
            Self::DDDotFront(note)
                => format!("...{}", note.msg()),
            Self::DDDotBack(note)
                => format!("{}...", note.msg()),
            Self::Here
                => "here".to_string(),
            Self::Unknown
                => "???".to_string(),
            Self::LeadingDigits
                => "needs leading digit".to_string(),
            Self::TrailingDigits
                => "needs trailing digit".to_string(),
            Self::ExpectedToken(expected)
                => format!("expected {}", expected.to_string().bold()),
            Self::CanBeRemoved
                => "this can be removed".to_string(),
            Self::CanRemoveParentheses
                => "parentheses can be removed".to_string(),
            Self::FollowsIf
                => "this should follow an if statement".to_string(),
            Self::FollowsIfThen
                => "this should follow an if-then statement".to_string(),
            Self::MissingWhile
                => "this must be followed by a while guard".to_string(),
            Self::CannotAssign
                => "this cannot be assigned to".to_string(),
            Self::MustBeOfType(ty)
                => format!("this must be of type '{}'",
                    ty.to_string().bold()
                ),
            Self::OfType(ty)
                => format!("this is of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::VariableDeclaration(name)
                => format!("variable '{}' is declared here", name.bold()),
            Self::VariableType(name, ty)
                => format!("variable '{}' has type '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::ArgumentType(name, ty)
                => format!("argument '{}' has type '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::NotFunction(ty)
                => format!("this is not a function, and is of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::FunctionArgs(name, count)
                => format!("function '{}' takes in {} arguments",
                    name.bold(),
                    count.to_string().bold(),
                ),
            Self::FunctionReturnType(name, ty)
                => format!("function '{}' returns '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::FunctionVariableCount(count)
                => format!("uses {} variables",
                    count.to_string().bold(),
                ),
            Self::ProvidedArgs(count)
                => format!("{} were provided", count.to_string().bold()),
            Self::VariableCapturedBy(var_name, func_name)
                => format!("'{}' gets captured by '{}' here",
                    var_name.bold(),
                    func_name.bold(),
                ),
            Self::TupleValueCount(len)
                => format!("this tuple contains {} values",
                    len.to_string().bold(),
                ),
        }
    }
}
