use std::collections::BTreeSet;

use super::{Stmt, Type};
use crate::utils::OptSpanned;

pub struct FunctionInfo {
    pub id: u64,
    pub name: OptSpanned<String>,
    pub depth: u16,
    pub args: Vec<(String, Type)>,
    pub arg_ids: Vec<u64>,
    pub ty: OptSpanned<Type>,
    pub local_variables: BTreeSet<u64>,
    pub captured_variables: BTreeSet<u64>,
    pub code: Option<Box<Stmt>>,
    pub was_analysed: bool,
}
