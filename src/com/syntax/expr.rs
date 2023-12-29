use crate::com::span::Span;

use super::{bin_op::BinOpAst, un_op::UnOpAst};

#[derive(Debug)]
pub struct ExprAst {
    pub kind: ExprAstKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprAstKind {
    Bad,
    Integer(i64),
    Decimal(f64),
    Identifier(String),
    Boolean(bool),
    Parenthesized(Box<ExprAst>),
    BinaryOp(BinOpAst, Box<ExprAst>, Box<ExprAst>),
    UnaryOp(UnOpAst, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
    Debug(Box<ExprAst>),
    Ref(Box<ExprAst>),
    Deref(Box<ExprAst>),
}

impl ExprAstKind {
    pub fn wrap(self, span: Span) -> ExprAst {
        ExprAst { kind: self, span }
    }
}
