use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity, TypeRepr},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_statement(&mut self, stmt: &ast::Stmt) -> abt::Stmt {
        match &stmt.value {
            ast::StmtKind::Empty
                => abt::StmtKind::Empty,
            ast::StmtKind::DataDef(name, fields)
                => self.analyse_data_structure_definition(name, fields),
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
            ast::StmtKind::Print(expr)
                => self.analyse_print_statement(expr),
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

    fn analyse_print_statement(&mut self, expr: &ast::Expr) -> abt::StmtKind {
        let bound_expr = self.analyse_expression(expr);
        let bound_ty = self.program.type_of(&bound_expr);
        if let abt::Type::Pointer(inner) = &bound_ty {
            if inner.is(&abt::Type::U8) {
                return abt::StmtKind::Print(Box::new(bound_expr));
            }
        }

        let expected_ty_repr = TypeRepr::Pointer(Box::new(TypeRepr::U8));
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidPrint(
                self.program.type_repr(&bound_ty),
            ))
            .with_severity(Severity::Error)
            .with_span(expr.span)
            .annotate_primary(Note::MustBeOfType(expected_ty_repr), expr.span)
            .done();
        self.diagnostics.push(d);
        abt::StmtKind::Empty
    }
}
