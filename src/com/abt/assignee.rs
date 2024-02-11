use super::Type;

#[derive(Debug, Clone)]
pub enum Assignee {
    Variable,
    VarDeref,
    Deref(Box<Assignee>),
    TupleImmediateIndex(Box<Assignee>, Type, usize),
    ArrayImmediateIndex(Box<Assignee>, Type, usize),
}
