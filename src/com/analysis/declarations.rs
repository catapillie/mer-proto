use crate::com::syntax::stmt::{StmtAst, StmtAstKind};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn register_all_declarations(&mut self, stmt: &StmtAst) {
        self.register_declarations(stmt, 0, 0);
    }

    // reach all statements and look for declarations
    // nested statements increase depth
    fn register_declarations(&mut self, stmt: &StmtAst, depth: u64, offset: u64) {
        match &stmt.kind {
            StmtAstKind::Block(stmts) => {
                let mut block_offset = offset;
                for stmt in stmts {
                    let is_block = matches!(stmt.kind, StmtAstKind::Block(_));
                    let o = if is_block {
                        block_offset += 1;
                        block_offset
                    } else {
                        offset
                    };
                    self.register_declarations(stmt, depth + 1, o);
                }
            }
            StmtAstKind::Func(name, args, body, ret_ty) => {
                if let Some(name) = name {
                    let args_ty = args.iter().map(|(_, ty)| self.analyse_type(ty)).collect();
                    let ret_ty = self.analyse_type(ret_ty);

                    let id = self.make_unique_id();
                    let signature = (args_ty, ret_ty, id);
                    let index = (name.clone(), depth, offset);

                    let previous = self.functions.insert(index, signature);
                    assert!(previous.is_none());
                }

                self.register_declarations(body, depth + 1, 0);
            }
            StmtAstKind::IfThen(_, body) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::IfThenElse(_, body_then, body_else) => {
                self.register_declarations(body_then, depth + 1, 0);
                self.register_declarations(body_else, depth + 1, 0);
            }
            StmtAstKind::Then(body) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::Else(body) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::WhileDo(_, body) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::DoWhile(body, _) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::Do(body) => self.register_declarations(body, depth + 1, 0),
            StmtAstKind::Empty => {}
            StmtAstKind::VarDef(_, _) => {}
            StmtAstKind::Expr(_) => {}
            StmtAstKind::Return => {}
            StmtAstKind::ReturnWith(_) => {}
        }
    }
}
