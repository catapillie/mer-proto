use std::fmt::Display;

use super::tokens::{Token, TokenKind};

#[derive(Debug)]
pub enum ParseError {
    IllegalCharacter(char),
    ExpectedToken(Token, TokenKind),
    ExpectedExpression,
    ExpectedStatement,
}

#[rustfmt::skip]
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::IllegalCharacter(u)
                => write!(f, "encountered illegal character {u:?}"),
            ParseError::ExpectedToken(got, expected)
                => write!(f, "expected {expected}, but found {got}"),
            ParseError::ExpectedExpression
                => write!(f, "expected an expression"),
            ParseError::ExpectedStatement
                => write!(f, "expected a statement"),
        }
    }
}
