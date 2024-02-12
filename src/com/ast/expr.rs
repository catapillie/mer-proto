use super::{BinOp, UnOp};
use crate::utils::{Span, Spanned};

pub type Expr = Spanned<ExprKind>;

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
    TernaryCase(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    Case(Box<[(Option<Expr>, Expr)]>, Span),
    DataInit(Spanned<String>, Box<[(Spanned<String>, Expr)]>),
    FieldAccess(Box<Expr>, Spanned<String>),
}

impl ExprKind {
    pub fn wrap(self, span: Span) -> Expr {
        Spanned { value: self, span }
    }
}
