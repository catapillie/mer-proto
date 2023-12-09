use crate::com::syntax::stmt::{StmtAst, StmtAstKind};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn register_all_declarations(&mut self, stmt: &StmtAst) {
        self.register_declarations(stmt, 0);
    }

    // reach all statements and look for declarations
    // nested statements increase depth
    fn register_declarations(&mut self, stmt: &StmtAst, depth: u64) {
        match &stmt.kind {
            StmtAstKind::Block(stmts) => {
                for stmt in stmts {
                    self.register_declarations(stmt, depth + 1)
                }
            }
            StmtAstKind::Func(name, _, body, _) => {
                if let Some(name) = name {
                    let previous = self.declarations.insert((name.clone(), depth), ());
                    if previous.is_some() {
                        panic!("haven't figured out shadowing across two different scopes")
                    }
                }

                self.register_declarations(body, depth);
            }
            StmtAstKind::IfThen(_, body) => self.register_declarations(body, depth + 1),
            StmtAstKind::Then(body) => self.register_declarations(body, depth + 1),
            StmtAstKind::IfThenElse(_, body_then, body_else) => {
                self.register_declarations(body_then, depth + 1);
                self.register_declarations(body_else, depth + 1);
            }
            StmtAstKind::Else(body) => self.register_declarations(body, depth + 1),
            StmtAstKind::WhileDo(_, body) => self.register_declarations(body, depth + 1),
            StmtAstKind::DoWhile(body, _) => self.register_declarations(body, depth + 1),
            StmtAstKind::Do(body) => self.register_declarations(body, depth + 1),
            StmtAstKind::Empty => {}
            StmtAstKind::VarDef(_, _) => {}
            StmtAstKind::Expr(_) => {}
            StmtAstKind::Return => {}
            StmtAstKind::ReturnWith(_) => {}
        }
    }
}
