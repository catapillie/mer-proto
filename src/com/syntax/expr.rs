use crate::com::{span::Span, tokens::{IntegerValues, DecimalValues}};

use super::{bin_op::BinOpAst, un_op::UnOpAst};

#[derive(Debug)]
pub struct ExprAst {
    pub kind: ExprAstKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprAstKind {
    Bad,
    Integer(Box<IntegerValues>),
    Decimal(Box<DecimalValues>),
    Identifier(String),
    Boolean(bool),
    Parenthesized(Box<ExprAst>),
    BinaryOp(BinOpAst, Box<ExprAst>, Box<ExprAst>),
    UnaryOp(UnOpAst, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
    Debug(Box<ExprAst>),
}

impl ExprAstKind {
    pub fn wrap(self, span: Span) -> ExprAst {
        ExprAst { kind: self, span }
    }
}
