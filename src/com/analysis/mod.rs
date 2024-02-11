use std::collections::HashMap;

use self::scope::Scope;
use super::abt::{DataInfo, FunctionInfo, VariableInfo};
use crate::diagnostics::DiagnosticList;

mod scope;

mod control_flow;
mod data;
mod expression;
mod program;
mod statement;
mod types;

mod functions;
mod variables;

mod arrays;
mod indexing;
mod operations;
mod reference;
mod tuples;

mod case;
mod if_then;
mod while_do;

pub struct Declaration {
    pub declared: u64,
    pub shadowed: Option<u64>,
}

pub struct Analyser<'d> {
    diagnostics: &'d mut DiagnosticList,
    scope: Scope,
    variables: HashMap<u64, VariableInfo>,
    functions: HashMap<u64, FunctionInfo>,
    datas: HashMap<u64, DataInfo>,
    uid: u64,
}

impl<'d> Analyser<'d> {
    pub fn new(diagnostics: &'d mut DiagnosticList) -> Self {
        Self {
            diagnostics,
            scope: Scope::root(),
            variables: HashMap::default(),
            functions: HashMap::default(),
            datas: HashMap::default(),
            uid: 0,
        }
    }

    pub fn make_unique_id(&mut self) -> u64 {
        self.uid += 1;
        self.uid
    }
}
