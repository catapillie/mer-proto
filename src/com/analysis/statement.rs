use super::Analyser;
use crate::{
    com::{
        abt,
        ast::{self, stmt::FuncDef},
    },
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_statement(&mut self, stmt: &ast::Stmt) -> abt::Stmt {
        match &stmt.value {
            ast::StmtKind::Empty
                => abt::StmtKind::Empty,
            ast::StmtKind::DataDef(_)
                => abt::StmtKind::Empty,
            ast::StmtKind::AliasDef(_)
                => abt::StmtKind::Empty,
            ast::StmtKind::FuncDef(ast)
                => self.analyse_function_body(ast),
            ast::StmtKind::VarDef(ast)
                => self.analyse_variable_definition(ast),
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
        self.reach_definitions(stmts);
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

    fn reach_definitions(&mut self, stmts: &[ast::Stmt]) {
        let mut funcs = Vec::new();
        let mut datas = Vec::new();
        let mut alias = Vec::new();

        for stmt in stmts {
            match &stmt.value {
                ast::StmtKind::FuncDef(
                    ast @ FuncDef {
                        name: Some(name),
                        args: _,
                        ty,
                        body: _,
                    },
                ) => match self.analyse_function_header(name, ty) {
                    None => continue,
                    Some(id) => funcs.push((id, ast)),
                },
                ast::StmtKind::DataDef(ast) => {
                    match self.analyse_data_structure_header(ast) {
                        None => continue,
                        Some(id) => datas.push((id, ast)),
                    }
                }
                ast::StmtKind::AliasDef(ast) => match self.analyse_alias_header(ast) {
                    None => continue,
                    Some(id) => alias.push((id, ast)),
                },
                _ => (),
            }
        }

        for (id, ast) in &datas {
            self.analyse_data_structure_definition(ast, *id);
        }

        for (id, ast) in &alias {
            self.analyse_alias_definition(ast, *id);
        }

        let data_ids = datas.iter().map(|(id, _)| *id).collect::<Box<_>>();
        self.analyse_data_structure_sizes(&data_ids);

        for (id, ast) in &funcs {
            self.analyse_function_definition(&ast.args, &ast.ty, *id);
        }
    }

    fn analyse_print_statement(&mut self, expr: &ast::Expr) -> abt::StmtKind {
        let mut bound_expr = self.analyse_expression(expr);
        let bound_ty = self.program.type_of(&bound_expr);
        let expected_ty = abt::Type::Pointer(Box::new(abt::Type::U8));
        if self.type_check_coerce(&mut bound_expr, &expected_ty) {
            return abt::StmtKind::Print(Box::new(bound_expr));
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidPrint(
                self.program.type_repr(&bound_ty),
            ))
            .with_severity(Severity::Error)
            .with_span(expr.span)
            .annotate_primary(
                Note::OfTypeButShouldBe(
                    self.program.type_repr(&bound_ty),
                    self.program.type_repr(&expected_ty),
                ),
                expr.span,
            )
            .done();
        self.diagnostics.push(d);
        abt::StmtKind::Empty
    }
}
