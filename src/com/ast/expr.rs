use super::{bin_op::BinOp, un_op::UnOp};
use crate::utils::Span;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Bad,
    Unit,
    Integer(i64),
    Decimal(f64),
    Identifier(String),
    Boolean(bool),
    Parenthesized(Box<Expr>),
    Tuple(Box<Expr>, Box<[Expr]>),
    Array(Box<[Expr]>),
    ImmediateIndex(Box<Expr>, u64),
    Index(Box<Expr>, Box<Expr>),
    BinaryOp(BinOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnOp, Box<Expr>),
    Call(Box<Expr>, Box<[Expr]>),
    Debug(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Todo,
    Unreachable,
    Case(Box<[(Option<Expr>, Expr)]>, Span),
}

impl ExprKind {
    pub fn wrap(self, span: Span) -> Expr {
        Expr { kind: self, span }
    }
}
