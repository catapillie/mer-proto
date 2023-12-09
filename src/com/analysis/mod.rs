use super::diagnostics::Diagnostics;

mod expression;
mod program;
mod statement;

mod if_then;
mod while_do;

pub struct Analyser<'d> {
    diagnostics: &'d mut Diagnostics,
}

impl<'d> Analyser<'d> {
    pub fn new(diagnostics: &'d mut Diagnostics) -> Self {
        Self { diagnostics }
    }
}
