use std::collections::HashMap;

use super::{diagnostics::Diagnostics, abt::TypeAbt};

mod declarations;

mod types;

mod expression;
mod program;
mod statement;

mod if_then;
mod while_do;

pub struct Analyser<'d> {
    diagnostics: &'d mut Diagnostics,
    functions: HashMap<(String, u64), (Vec<TypeAbt>, TypeAbt)>,
}

impl<'d> Analyser<'d> {
    pub fn new(diagnostics: &'d mut Diagnostics) -> Self {
        Self {
            diagnostics,
            functions: Default::default(),
        }
    }
}
