use std::collections::BTreeMap;

use super::{Stmt, Type, VariableUsage};
use crate::utils::OptSpanned;

pub struct FunctionInfo {
    pub id: u64,
    pub name: OptSpanned<String>,
    pub depth: u16,
    pub args: Vec<(String, Type)>,
    pub arg_ids: Vec<u64>,
    pub ty: OptSpanned<Type>,
    pub used_variables: BTreeMap<u64, VariableUsage>,
    pub code: Option<Box<Stmt>>,
    pub was_analysed: bool,
}
