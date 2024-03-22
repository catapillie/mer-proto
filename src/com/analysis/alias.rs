use super::Analyser;
use crate::com::{abt, ast::stmt::AliasDef};

impl<'d> Analyser<'d> {
    pub fn analyse_alias_def(&self, ast: &AliasDef) -> abt::StmtKind {
        abt::StmtKind::Empty
    }
}
