use super::{Expr, Type};

#[derive(Debug, Clone)]
pub enum LValue {
    Variable,
    VarDeref,
    Deref(Box<LValue>),
    TupleImmediateIndex(Box<LValue>, Type, usize),
    ArrayImmediateIndex(Box<LValue>, Type, usize),
    ArrayIndex(Box<LValue>, Type, Box<Expr>),
    PointerIndex(Box<LValue>, Type, Box<Expr>),
    FieldAccess(Box<LValue>, u64, usize),
}
