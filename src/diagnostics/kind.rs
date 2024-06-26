use std::num::ParseIntError;

use super::{PatRepr, TypeRepr};
use crate::com::{
    ast,
    tokens::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    IllegalCharacter(char),
    InvalidInteger(ParseIntError),
    MissingQuote,
    InvalidEscapeSequence,

    ExpectedToken {
        found: Token,
        expected: TokenKind,
    },
    ExpectedExpression,
    ExpectedStatement,
    ExpectedType,
    ExpectedPattern,
    SingletonTypeSyntax,
    ExpectedArraySizeOrAmpersand,
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
    NotVariable(String),
    AssigneeMustBeVariable,
    TooManyVariables(String, usize),
    TooManyTopLevelVariables(usize),
    UnusedVariable(String),

    InvalidArgCount {
        got: usize,
        expected: usize,
    },
    InvalidCallee,
    FunctionRedefinition(String),

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
    InvalidArrayConcatenation {
        inner_left: TypeRepr,
        inner_right: TypeRepr,
    },

    CannotReturnUnit {
        expected: TypeRepr,
    },
    NotAllPathsReturn,
    TopLevelMustReturn,

    UnreachableCode,

    EarlyVariableCapture {
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

    InfiniteDataStructure(String),
    DataStructureRedefinition(String),
    CannotMarkAsOpaque,
    NonOpaqueTypeConstructor(String),
    FieldDeclaredMoreThanOnce(String),
    InvalidDataStructureExpression,
    UnknownDataStructure(String),
    UnknownFieldInDataStructure {
        field_name: String,
        data_name: String,
    },
    FieldSetMoreThanOnce(String),
    FieldsNeverSet(String, Box<[String]>, String),
    InvalidFieldAccess(String),
    DiscardingWithExpression(TypeRepr),
    EmptyWithExpression(TypeRepr),

    AliasRedefinition(String),
    UnknownTypeConstructor(String),

    NonIntegerSize,

    InvalidPrint(TypeRepr),

    PatternMismatch(PatRepr, TypeRepr),
    TuplePatternMismatch(PatRepr, usize),
    ArrayPatternMismatch(PatRepr, usize),
    OpaqueTypeConstructorPatternMismatch(String, String),
    MissingPatternInOpaqueTypeConstructorPattern(PatRepr, String),
    MoreThanOnePatternInOpaqueTypeConstructorPattern(PatRepr, String),
}
