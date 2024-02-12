use std::num::ParseIntError;

use super::TypeRepr;
use crate::com::{
    ast,
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
    ExpectedAccess,

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
        found: TypeRepr,
        expected: TypeRepr,
    },

    InvalidUnaryOperation {
        op: ast::UnOp,
        ty: TypeRepr,
    },
    InvalidBinaryOperation {
        op: ast::BinOp,
        left: TypeRepr,
        right: TypeRepr,
    },

    MustReturnValue {
        expected: TypeRepr,
    },
    NotAllPathsReturn,
    TopLevelMustReturn,

    UnreachableCode,

    UnallowedVariableCapture {
        func_name: String,
        var_name: String,
    },

    InvalidDebugExpression(TypeRepr),

    InvalidDereference(TypeRepr),

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

    InvalidDataStructureExpression,
    UnknownDataStructure(String),
    UnknownFieldInDataStructure {
        field_name: String,
        data_name: String,
    },
    FieldSetMoreThanOnce(String),
    FieldsNeverSet(String, Box<[String]>, String),
    InvalidFieldAccess(String),
}
