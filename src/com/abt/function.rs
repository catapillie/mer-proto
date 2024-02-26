use std::collections::BTreeMap;

use super::{Stmt, Type, VariableUsage};
use crate::utils::Span;

pub struct FunctionInfo {
    pub id: u64,
    pub name: String,
    pub name_span: Option<Span>,
    pub depth: u16,
    pub args: Vec<(String, Type)>,
    pub arg_ids: Vec<u64>,
    pub ty: Type,
    pub ty_span: Option<Span>,
    pub used_variables: BTreeMap<u64, VariableUsage>,
    pub code: Option<Box<Stmt>>,
}
