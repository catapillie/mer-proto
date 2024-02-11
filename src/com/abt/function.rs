use std::collections::BTreeMap;

use super::{Stmt, TypeAbt, VariableUsage};
use crate::utils::Span;

pub struct FunctionInfo {
    pub id: u64,
    pub name: String,
    pub span: Option<Span>,
    pub depth: u16,
    pub args: Vec<(String, TypeAbt)>,
    pub arg_ids: Vec<u64>,
    pub ty: TypeAbt,
    pub ty_span: Option<Span>,
    pub used_variables: BTreeMap<u64, VariableUsage>,
    pub code: Option<Box<Stmt>>,
}
