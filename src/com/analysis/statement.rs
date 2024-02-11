use crate::com::{abt, ast};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_statement(&mut self, stmt: &ast::Stmt) -> abt::Stmt {
        match &stmt.value {
            ast::StmtKind::Empty
                => abt::StmtKind::Empty,
            ast::StmtKind::VarDef(name, value)
                => self.analyse_variable_definition(name, value, stmt.span),
            ast::StmtKind::Expr(expr)
                => abt::StmtKind::Expr(Box::new(self.analyse_expression(expr))),
            ast::StmtKind::Block(stmts)
                => self.analyse_block_statement(stmts),
            ast::StmtKind::IfThen(guard, body)
                => self.analyse_if_then_statement(guard, body),
            ast::StmtKind::Then(body)
                => self.analyse_then_statement(body),
            ast::StmtKind::IfThenElse(guard, body_then, body_else)
                => self.analyse_if_then_else_statement(guard, body_then, body_else),
            ast::StmtKind::Else(body)
                => self.analyse_else_statement(body),
            ast::StmtKind::WhileDo(guard, body)
                => self.analyse_while_do_statement(guard, body),
            ast::StmtKind::DoWhile(body, guard)
                => self.analyse_do_while_statement(body, guard),
            ast::StmtKind::Do(body)
                => self.analyse_do_statement(body),
            ast::StmtKind::Func(name, args, body, ty)
                => self.analyse_function_definition(name, args, body, ty),
            ast::StmtKind::Return
                => self.analyse_return_statement(stmt.span),
            ast::StmtKind::ReturnWith(expr)
                => self.analyse_return_with_statement(expr),
        }
        .wrap(stmt.span)
    }

    fn analyse_block_statement(&mut self, stmts: &[ast::Stmt]) -> abt::StmtKind {
        self.open_scope();
        let bound_stmts = stmts
            .iter()
            .map(|stmt| self.analyse_statement(stmt))
            .filter(|stmt| !matches!(stmt.value, abt::StmtKind::Empty))
            .collect::<Box<_>>();
        self.close_scope();

        if bound_stmts.is_empty() {
            abt::StmtKind::Empty
        } else {
            abt::StmtKind::Block(bound_stmts)
        }
    }
}
