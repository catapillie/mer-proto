use super::Analyser;
use crate::{
    com::{
        abt::{self, FunctionInfo, Size},
        ast::{self, stmt::FuncDef},
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{OptSpanned, Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn analyse_function_header(
        &mut self,
        name: &Spanned<String>,
        ty: &ast::Type,
    ) -> Option<u64> {
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
                return None;
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

        Some(id)
    }

    pub fn analyse_function_definition(
        &mut self,
        args: &[(String, ast::Type, Span)],
        ty: &ast::Type,
        id: u64,
    ) {
        let bound_args = args
            .iter()
            .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
            .collect();
        let bound_ty = self.analyse_type(ty);

        let info = self.program.functions.get_mut(&id).unwrap();
        info.args = bound_args;
        info.ty.value = bound_ty;
    }

    pub fn analyse_function_body(&mut self, ast: &FuncDef) -> abt::StmtKind {
        let Some(name) = &ast.name else {
            return abt::StmtKind::Empty;
        };

        let Some(&id) = self.scope.bindings.get(&name.value) else {
            unreachable!()
        };
        let info = self.program.functions.get_mut(&id).unwrap();

        // if this is true, then it is an attempt to analyse a function redefinition
        // which we don't want (an error should have been generated)
        if info.was_analysed {
            return abt::StmtKind::Empty;
        }

        assert_eq!(info.args.len(), ast.args.len());
        let bound_args = info.args.clone();
        let bound_spanned_args = bound_args
            .iter()
            .zip(ast.args.iter().map(|(_, _, span)| span));
        let id = info.id;

        self.open_scope();
        self.scope.current_func_id = id;

        for ((arg_name, arg_ty), &span) in bound_spanned_args {
            let spanned_arg = Spanned {
                value: arg_name.clone(),
                span,
            };
            let decl = self.declare_variable_here(spanned_arg, arg_ty.clone());
            self.program
                .functions
                .get_mut(&id)
                .unwrap()
                .arg_ids
                .push(decl.declared)
        }
        let bound_body = self.analyse_statement(&ast.body);
        if !self.analyse_control_flow(&bound_body) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NotAllPathsReturn)
                .with_severity(Severity::Error)
                .with_span(ast.ty.span)
                .annotate_primary(Note::Quiet, ast.ty.span)
                .done();
            self.diagnostics.push(d);
        }

        self.close_scope();

        let info = self.program.functions.get_mut(&id).unwrap();
        info.code = Some(Box::new(bound_body));

        if let Size::Known(var_count) = self.count_all_variable_sizes(id) {
            if var_count > 255 {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TooManyVariables(
                        name.value.clone(),
                        var_count,
                    ))
                    .with_severity(Severity::Error)
                    .with_span(name.span)
                    .annotate_primary(Note::FunctionVariableCount(var_count), name.span)
                    .done();
                self.diagnostics.push(d);
            }
        }

        self.program.functions.get_mut(&id).unwrap().was_analysed = true;
        abt::StmtKind::Empty
    }

    pub fn analyse_call_expression(
        &mut self,
        callee: &ast::Expr,
        args: &[ast::Expr],
        span: Span,
    ) -> abt::Expr {
        let bound_callee = self.analyse_expression(callee);
        let bound_args = args
            .iter()
            .map(|arg| self.analyse_expression(arg))
            .collect::<Box<_>>();

        if matches!(bound_callee, abt::Expr::Unknown) {
            return abt::Expr::Unknown;
        };

        if let abt::Expr::Function(id) = bound_callee {
            self.analyse_immediate_call(args, bound_args, span, id)
        } else if let abt::Expr::OpaqueConstructor { ctor_id, alias_id } = bound_callee {
            self.analyse_opaque_type_construction(args, bound_args, span, ctor_id, alias_id)
        } else if let abt::Type::Func(func_args, func_return_ty) =
            self.program.type_of(&bound_callee)
        {
            self.analyse_indirect_call(
                args,
                bound_args,
                &func_args,
                *func_return_ty,
                bound_callee,
                span,
            )
        } else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidCallee)
                .with_span(callee.span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::NotFunction(self.program.type_repr(&self.program.type_of(&bound_callee))),
                    callee.span,
                )
                .done();
            self.diagnostics.push(d);
            abt::Expr::Unknown
        }
    }

    fn analyse_immediate_call(
        &mut self,
        args: &[ast::Expr],
        mut bound_args: Box<[abt::Expr]>,
        span: Span,
        id: u64,
    ) -> abt::Expr {
        let info = self.program.functions.get(&id).unwrap();

        let func_name = info.name.clone();
        let func_args = info.args.clone();
        let func_arg_ids = info.arg_ids.clone();
        let ty = info.ty.clone();

        let mut invalid = false;
        for (((i, bound_arg), span), arg_ty) in bound_args
            .iter_mut()
            .enumerate()
            .zip(args.iter().map(|arg| arg.span))
            .zip(func_args.iter().map(|(_, arg_ty)| arg_ty))
        {
            let ty_param = self.program.type_of(bound_arg);
            if self.type_check_coerce(bound_arg, arg_ty) {
                continue;
            }

            let arg_id = func_arg_ids.get(i).unwrap();
            let arg_info = self.program.variables.get(arg_id).unwrap();
            let arg_name = arg_info.name.clone();
            let arg_span = arg_info.name.span;
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&ty_param),
                    expected: self.program.type_repr(arg_ty),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_secondary(
                    Note::ArgumentType(arg_name.value, self.program.type_repr(arg_ty))
                        .dddot_back()
                        .num(1),
                    arg_span,
                    NoteSeverity::Annotation,
                )
                .annotate_primary(
                    Note::OfType(self.program.type_repr(&ty_param))
                        .but()
                        .dddot_front()
                        .num(2),
                    span,
                )
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if bound_args.len() != func_args.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidArgCount {
                    got: bound_args.len(),
                    expected: func_args.len(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(
                    Note::ProvidedArgs(bound_args.len())
                        .but()
                        .dddot_front()
                        .num(2),
                    span,
                )
                .annotate_secondary(
                    Note::FunctionArgs(func_name.value, func_args.len())
                        .dddot_back()
                        .num(1),
                    func_name.span.expect("called functions have a span"),
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            abt::Expr::Unknown
        } else {
            abt::Expr::Call(id, bound_args, ty.value.clone())
        }
    }

    fn analyse_opaque_type_construction(
        &mut self,
        args: &[ast::Expr],
        mut bound_args: Box<[abt::Expr]>,
        span: Span,
        ctor_id: u64,
        alias_id: u64,
    ) -> abt::Expr {
        let func_info = self.program.functions.get(&ctor_id).unwrap();
        let func_args = func_info.args.clone();
        let func_ty = func_info.ty.clone();

        let alias_info = self.program.aliases.get(&alias_id).unwrap();
        let alias_name = alias_info.name.clone();
        let alias_ty = alias_info.ty.clone();

        let mut invalid = false;
        for ((bound_arg, span), arg_ty) in bound_args
            .iter_mut()
            .zip(args.iter().map(|arg| arg.span))
            .zip(func_args.iter().map(|(_, arg_ty)| arg_ty))
        {
            let ty_param = self.program.type_of(bound_arg);
            if self.type_check_coerce(bound_arg, arg_ty) {
                continue;
            }

            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&ty_param),
                    expected: self.program.type_repr(arg_ty),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_secondary(
                    Note::OpaqueAliasType(
                        alias_name.value.clone(),
                        self.program.type_repr(&alias_ty),
                    )
                    .dddot_back()
                    .num(1),
                    alias_name.span,
                    NoteSeverity::Annotation,
                )
                .annotate_primary(
                    Note::OfType(self.program.type_repr(&ty_param))
                        .but()
                        .dddot_front()
                        .num(2),
                    span,
                )
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if bound_args.len() != func_args.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidArgCount {
                    got: bound_args.len(),
                    expected: func_args.len(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(
                    Note::ProvidedArgs(bound_args.len())
                        .but()
                        .dddot_front()
                        .num(2),
                    span,
                )
                .annotate_secondary(
                    Note::OpaqueTypeArgCount(alias_name.value.clone())
                        .dddot_back()
                        .num(1),
                    alias_name.span,
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            abt::Expr::Unknown
        } else {
            abt::Expr::Call(ctor_id, bound_args, func_ty.value.clone())
        }
    }

    fn analyse_indirect_call(
        &mut self,
        args: &[ast::Expr],
        mut bound_args: Box<[abt::Expr]>,
        func_args: &[abt::Type],
        func_return_ty: abt::Type,
        bound_callee: abt::Expr,
        span: Span,
    ) -> abt::Expr {
        let mut invalid = false;
        for ((bound_arg, span), arg_ty) in bound_args
            .iter_mut()
            .zip(args.iter().map(|arg| arg.span))
            .zip(func_args.iter())
        {
            let ty = self.program.type_of(bound_arg);
            if self.type_check_coerce(bound_arg, arg_ty) {
                continue;
            }

            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&ty),
                    expected: self.program.type_repr(arg_ty),
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::MustBeOfType(self.program.type_repr(arg_ty)), span)
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if bound_args.len() != func_args.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidArgCount {
                    got: bound_args.len(),
                    expected: func_args.len(),
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            abt::Expr::Unknown
        } else {
            abt::Expr::IndirectCall(Box::new(bound_callee), bound_args, func_return_ty)
        }
    }

    pub fn analyse_return_statement(&mut self, stmt_span: Span) -> abt::StmtKind {
        let ty = abt::Type::Unit;
        let (return_ty, func_name) = {
            let id = self.scope.current_func_id;
            let info = self.program.functions.get(&id).unwrap();
            (&info.ty, &info.name)
        };

        if !self.type_check(&ty, &return_ty.value) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CannotReturnUnit {
                    expected: self.program.type_repr(&return_ty.value),
                })
                .with_severity(Severity::Error)
                .with_span(stmt_span);

            let d = match (func_name.span, return_ty.span) {
                (Some(span), Some(ty_span)) => d
                    .highlight(ty_span)
                    .annotate_secondary(
                        Note::FunctionReturnType(
                            func_name.value.clone(),
                            self.program.type_repr(&return_ty.value),
                        )
                        .dddot_back()
                        .num(1),
                        span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(Note::ReturnsUnit.but().dddot_front().num(2), stmt_span)
                    .done(),
                _ => d.annotate_primary(Note::ReturnsUnit, stmt_span).done(),
            };
            self.diagnostics.push(d);
            return abt::StmtKind::Return(Box::new(abt::Expr::Unknown));
        }

        abt::StmtKind::Return(Box::new(abt::Expr::Unit))
    }

    pub fn analyse_return_with_statement(&mut self, expr: &ast::Expr) -> abt::StmtKind {
        let mut bound_expr = self.analyse_expression(expr);
        let ty_expr = self.program.type_of(&bound_expr);
        let (return_ty, func_name) = {
            let id = self.scope.current_func_id;
            let info = self.program.functions.get(&id).unwrap();
            (&info.ty, &info.name)
        };

        if !self.type_check_coerce(&mut bound_expr, &return_ty.value) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&ty_expr),
                    expected: self.program.type_repr(&return_ty.value),
                })
                .with_severity(Severity::Error)
                .with_span(expr.span);

            let d = match (func_name.span, return_ty.span) {
                (Some(span), Some(ty_span)) => d
                    .highlight(ty_span)
                    .annotate_secondary(
                        Note::FunctionReturnType(
                            func_name.value.clone(),
                            self.program.type_repr(&return_ty.value),
                        )
                        .dddot_back()
                        .num(1),
                        span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::OfType(self.program.type_repr(&ty_expr))
                            .but()
                            .dddot_front()
                            .num(2),
                        expr.span,
                    )
                    .done(),
                _ => d
                    .annotate_primary(Note::OfType(self.program.type_repr(&ty_expr)), expr.span)
                    .done(),
            };

            self.diagnostics.push(d);
            return abt::StmtKind::Return(Box::new(abt::Expr::Unknown));
        }

        abt::StmtKind::Return(Box::new(bound_expr))
    }

    pub fn analyse_function_variable_usage(&mut self) {
        let id = self.scope.current_func_id;
        let info = self.program.functions.get(&id).unwrap();
        for (var_id, usage) in &info.used_variables {
            if usage.used {
                continue;
            }

            let var_info = self.program.variables.get(var_id).unwrap();
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnusedVariable(var_info.name.value.clone()))
                .with_span(var_info.name.span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::Here, var_info.name.span)
                .done();
            self.diagnostics.push(d);
        }
    }

    pub fn count_all_variable_sizes(&mut self, func_id: u64) -> Size {
        self.program
            .functions
            .get(&func_id)
            .unwrap()
            .used_variables
            .keys()
            .map(|var_id| self.program.variables.get(var_id).unwrap())
            .map(|var_info| self.program.size_of(&var_info.ty))
            .sum::<Size>()
    }
}
