use std::num::ParseIntError;

use crate::com::{
    syntax::{BinOpAst, UnOpAst},
    tokens::{Token, TokenKind},
    TypeAbt,
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
}
