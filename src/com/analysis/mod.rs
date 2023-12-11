use std::collections::HashMap;

use super::{abt::TypeAbt, diagnostics::Diagnostics};

mod declarations;
mod scope;

mod expression;
mod program;
mod statement;
mod types;

mod functions;
mod variables;

mod operations;

mod if_then;
mod while_do;

pub struct Analyser<'d> {
    diagnostics: &'d mut Diagnostics,
    variables: HashMap<(String, u64, u64), (TypeAbt, u64)>,
    functions: HashMap<(String, u64, u64), (Vec<TypeAbt>, TypeAbt, u64)>,
    current_depth: u64,
    current_offsets: Vec<u64>,
    current_return_ty: Vec<TypeAbt>,
    uid: u64,
}

impl<'d> Analyser<'d> {
    pub fn new(diagnostics: &'d mut Diagnostics) -> Self {
        Self {
            diagnostics,
            variables: Default::default(),
            functions: Default::default(),
            current_return_ty: vec![TypeAbt::Unit],
            current_depth: 0,
            current_offsets: vec![0],
            uid: 0,
        }
    }

    pub fn make_unique_id(&mut self) -> u64 {
        self.uid += 1;
        self.uid
    }
}
