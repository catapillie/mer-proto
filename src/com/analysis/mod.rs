use std::collections::HashMap;

use super::diagnostics::Diagnostics;

mod declarations;

mod expression;
mod program;
mod statement;

mod if_then;
mod while_do;

pub struct Analyser<'d> {
    diagnostics: &'d mut Diagnostics,
    declarations: HashMap<(String, u64), ()>,
}

impl<'d> Analyser<'d> {
    pub fn new(diagnostics: &'d mut Diagnostics) -> Self {
        Self {
            diagnostics,
            declarations: Default::default(),
        }
    }
}
