use std::collections::HashMap;

use super::Analyser;
use crate::{
    com::{
        abt::{self, DataInfo, FunctionInfo, Size},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{OptSpanned, Spanned},
};

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_statement(&mut self, stmt: &ast::Stmt) -> abt::Stmt {
        match &stmt.value {
            ast::StmtKind::Empty
                => abt::StmtKind::Empty,
            ast::StmtKind::DataDef(_, _)
                => abt::StmtKind::Empty,
            ast::StmtKind::Func(name, args, body, ty)
                => self.analyse_function_definition(name, args, body, ty),
            ast::StmtKind::VarDef(name, value)
                => self.analyse_variable_definition(name, value),
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

        for stmt in stmts {
            match &stmt.value {
                ast::StmtKind::Func(Some(name), _, _, ty) => {
                    if let Some(shadowed) = self.scope.bindings.get(&name.value) {
                        if let Some(info) = self.program.functions.get(shadowed) {
                            let shadowed_span = info.name.span.unwrap();
                            let d = diagnostics::create_diagnostic()
                                .with_kind(DiagnosticKind::FunctionRedefinition(name.value.clone()))
                                .with_span(name.span)
                                .with_severity(Severity::Error)
                                .annotate_secondary(
                                    Note::ShadowedFunction(name.value.clone())
                                        .dddot_back()
                                        .num(1),
                                    shadowed_span,
                                    NoteSeverity::Annotation,
                                )
                                .annotate_primary(
                                    Note::RedefinedFunction.and().dddot_front().num(2),
                                    name.span,
                                )
                                .done();
                            self.diagnostics.push(d);
                            continue;
                        }
                    }

                    let id = self.make_unique_id();
                    self.scope.bindings.insert(name.value.clone(), id);
                    self.program.functions.insert(
                        id,
                        FunctionInfo {
                            id,
                            name: name.clone().into(),
                            depth: self.scope.depth,
                            args: Vec::new(),
                            arg_ids: Vec::new(),
                            ty: OptSpanned {
                                value: abt::Type::Unknown,
                                span: Some(ty.span),
                            },
                            used_variables: Default::default(),
                            code: None,
                            was_analysed: false,
                        },
                    );
                    funcs.push((id, &stmt.value))
                }
                ast::StmtKind::DataDef(name, _) => {
                    if let Some(shadowed) = self.scope.bindings.get(&name.value) {
                        if let Some(info) = self.program.datas.get(shadowed) {
                            let shadowed_span = info.name.span;
                            let d = diagnostics::create_diagnostic()
                                .with_kind(DiagnosticKind::DataStructureRedefinition(
                                    name.value.clone(),
                                ))
                                .with_span(name.span)
                                .with_severity(Severity::Error)
                                .annotate_secondary(
                                    Note::ShadowedDataStructure(name.value.clone())
                                        .dddot_back()
                                        .num(1),
                                    shadowed_span,
                                    NoteSeverity::Annotation,
                                )
                                .annotate_primary(
                                    Note::RedefinedDataStructure.and().dddot_front().num(2),
                                    name.span,
                                )
                                .done();
                            self.diagnostics.push(d);
                            continue;
                        }
                    }

                    let id = self.make_unique_id();
                    self.scope.bindings.insert(name.value.clone(), id);
                    self.program.datas.insert(
                        id,
                        DataInfo {
                            name: name.clone(),
                            id,
                            fields: Vec::new(),
                            size: Size::Infinite,
                        },
                    );
                    datas.push((id, &stmt.value))
                }
                _ => (),
            }
        }

        for (id, stmt) in &datas {
            let ast::StmtKind::DataDef(name, fields) = stmt else {
                unreachable!();
            };

            let mut field_spans = HashMap::new();
            for (field_name, _) in fields.iter() {
                field_spans
                    .entry(field_name.value.as_str())
                    .and_modify(|prev_span| {
                        let d = diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::FieldDeclaredMoreThanOnce(
                                field_name.value.clone(),
                            ))
                            .with_span(field_name.span)
                            .with_severity(Severity::Error)
                            .annotate_secondary(
                                Note::FieldDeclared(field_name.value.clone())
                                    .dddot_back()
                                    .num(1),
                                *prev_span,
                                NoteSeverity::Annotation,
                            )
                            .annotate_primary(
                                Note::FieldDeclaredAgain(field_name.value.clone())
                                    .then()
                                    .dddot_front()
                                    .num(2),
                                field_name.span,
                            )
                            .highlight(name.span)
                            .done();
                        self.diagnostics.push(d);
                    })
                    .or_insert(field_name.span);
            }

            let bound_fields = fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        Spanned {
                            value: self.analyse_type(ty),
                            span: ty.span,
                        },
                    )
                })
                .collect::<Vec<_>>();

            let info = self.program.datas.get_mut(id).unwrap();
            info.fields = bound_fields;
        }

        for (id, stmt) in &funcs {
            let ast::StmtKind::Func(Some(_), args, _, ty) = stmt else {
                unreachable!()
            };

            let bound_args = args
                .iter()
                .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
                .collect();
            let bound_ty = self.analyse_type(ty);

            let info = self.program.functions.get_mut(id).unwrap();
            info.args = bound_args;
            info.ty.value = bound_ty;
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
