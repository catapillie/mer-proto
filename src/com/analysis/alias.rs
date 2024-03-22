use super::Analyser;
use crate::com::{
    abt::{self, AliasInfo},
    ast::stmt::AliasDef,
};

impl<'d> Analyser<'d> {
    pub fn analyse_alias_def(&mut self, ast: &AliasDef) -> abt::StmtKind {
        let ty = self.analyse_type(&ast.ty);

        let id = self.make_unique_id();
        self.scope.bindings.insert(ast.name.value.clone(), id);
        self.program.aliases.insert(
            id,
            AliasInfo {
                name: ast.name.clone(),
                ty,
            },
        );

        abt::StmtKind::Empty
    }
}
