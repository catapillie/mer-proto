use colored::Colorize;
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
                => "only tuples and arrays of known size can be access with an immediate index".to_string(),
            Self::InvalidIndex
                => "cannot index a value which is not an array".to_string(),
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
            Self::ArrayIndexMustBeInteger
                => "array index must be an integer".to_string(),
        }
    }
}
