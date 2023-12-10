use crate::com::syntax::stmt::StmtAst;

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_program(&mut self, ast: &StmtAst) {
        self.register_all_declarations(ast);
        self.analyse_statement(ast);
    }
}