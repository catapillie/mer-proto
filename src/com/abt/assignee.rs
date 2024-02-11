use super::TypeAbt;

#[derive(Debug)]
pub enum Assignee {
    Variable,
    VarDeref,
    Deref(Box<Assignee>),
    TupleImmediateIndex(Box<Assignee>, TypeAbt, usize),
    ArrayImmediateIndex(Box<Assignee>, TypeAbt, usize),
}
