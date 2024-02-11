use std::collections::HashMap;

use super::{FunctionInfo, VariableInfo};

pub struct Program {
    pub main_fn_id: u64,
    pub functions: HashMap<u64, FunctionInfo>,
    pub variables: HashMap<u64, VariableInfo>,
}
