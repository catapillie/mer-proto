use super::{Expr, Type};

#[derive(Debug, Clone)]
pub enum Assignee {
    Variable,
    VarDeref,
    Deref(Box<Assignee>),
    TupleImmediateIndex(Box<Assignee>, Type, usize),
    ArrayImmediateIndex(Box<Assignee>, Type, usize),
    ArrayIndex(Box<Assignee>, Type, Box<Expr>),
    FieldAccess(Box<Assignee>, u64, usize),
}
