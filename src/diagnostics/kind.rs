use std::num::ParseIntError;

use crate::com::{
    abt, ast,
    tokens::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    IllegalCharacter(char),

    InvalidInteger(ParseIntError),

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

    InvalidArgCount {
        got: usize,
        expected: usize,
    },
    InvalidCallee,

    UnknownType(String),
    TypeMismatch {
        found: abt::Type,
        expected: abt::Type,
    },

    InvalidUnaryOperation {
        op: ast::UnOp,
        ty: abt::Type,
    },
    InvalidBinaryOperation {
        op: ast::BinOp,
        left: abt::Type,
        right: abt::Type,
    },

    MustReturnValue {
        expected: abt::Type,
    },
    NotAllPathsReturn,
    TopLevelMustReturn,

    UnreachableCode,

    UnallowedVariableCapture {
        func_name: String,
        var_name: String,
    },

    InvalidDebugExpression(abt::Type),

    InvalidDereference(abt::Type),

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
    InvalidIndex,
    ArrayIndexMustBeInteger,
    OutOfRangeConstantIndex {
        len: usize,
        index: usize,
    },
    CanBeImmediateIndex,

    MissingOtherwisePath,
    TooManyOtherwisePaths,
    LastCasePathIsNotOtherwise,
    CasePathsTypeMismatch,
    CaseOtherwiseCanBeSimplified,
    CaseThenOtherwiseCanBeSimplified,
}
