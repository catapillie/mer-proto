use super::{Assignee, BinOp, Type, UnOp};

#[derive(Debug, Clone)]
pub enum Expr {
    Unknown,
    Unit,
    Integer(i64),
    Decimal(f64),
    Boolean(bool),
    Variable(u64),
    Function(u64),
    Tuple(Box<Expr>, Box<[Expr]>),
    TupleImmediateIndex(Box<Expr>, usize),
    Array(Box<[Expr]>),
    ArrayImmediateIndex(Box<Expr>, usize),
    ArrayIndex(Box<Expr>, Box<Expr>),
    Assignment {
        assignee: Assignee,
        var_id: u64,
        expr: Box<Expr>,
    },
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(u64, Box<[Expr]>, Type),
    IndirectCall(Box<Expr>, Box<[Expr]>, Type),
    Debug(Box<Expr>, Type),
    Ref(Box<Expr>),
    VarRef(u64),
    Deref(Box<Expr>),
    VarDeref(u64),
    Todo,
    Unreachable,
    Case(Box<[(Expr, Expr)]>, Box<Expr>, Type),
}
