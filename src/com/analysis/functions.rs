use crate::{
    com::{
        abt::{self, FunctionInfo},
        analysis::Declaration,
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{spanned::OptSpanned, Span, Spanned},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn declare_function_here(
        &mut self,
        name: OptSpanned<String>,
        args: Vec<(String, abt::Type)>,
        ty: OptSpanned<abt::Type>,
    ) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.value.to_string(), declared);

        let info = FunctionInfo {
            id: declared,
            name,
            depth: self.scope.depth,
            args,
            arg_ids: Default::default(),
            ty,
            used_variables: Default::default(),
            code: None,
        };
        let prev = self.program.functions.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        Declaration { declared, shadowed }
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.scope.search(|scope| match scope.bindings.get(name) {
            Some(id) => self.program.functions.get(id),
            None => None,
        })
    }

    pub fn analyse_function_definition(
        &mut self,
        name: &Option<Spanned<String>>,
        args: &[(String, ast::Type, Span)],
        body: &ast::Stmt,
        ty: &ast::Type,
    ) -> abt::StmtKind {
        let Some(name) = name else {
            return abt::StmtKind::Empty;
        };

        let info = match self.get_function(&name.value) {
            Some(info) => info, // already declared
            None => {
                // must be declared now
                let bound_args = args
                    .iter()
                    .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
                    .collect();
                let bound_ty = OptSpanned {
                    value: self.analyse_type(ty),
                    span: Some(ty.span),
                };
                let decl = self.declare_function_here(name.clone().into(), bound_args, bound_ty);
                self.program.functions.get(&decl.declared).unwrap() // was just declared, cannot be none
            }
        };

        assert_eq!(info.args.len(), args.len());
        let bound_args = info.args.clone();
        let bound_spanned_args = bound_args.iter().zip(args.iter().map(|(_, _, span)| span));
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
        let bound_body = self.analyse_statement(body);
        if !self.analyse_control_flow(&bound_body) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NotAllPathsReturn)
                .with_severity(Severity::Error)
                .with_span(ty.span)
                .annotate_primary(Note::Quiet, ty.span)
                .done();
            self.diagnostics.push(d);
        }

        self.close_scope();

        let info = self.program.functions.get_mut(&id).unwrap();
        info.code = Some(Box::new(bound_body));

        let var_count = self.count_all_variable_sizes(id);
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
                    Note::MustBeOfType(self.program.type_repr(arg_ty))
                        .so()
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

        if !ty.is(&return_ty.value) {
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

    pub fn count_all_variable_sizes(&mut self, func_id: u64) -> usize {
        self.program
            .functions
            .get(&func_id)
            .unwrap()
            .used_variables
            .keys()
            .map(|var_id| self.program.variables.get(var_id).unwrap())
            .map(|var_info| self.program.size_of(&var_info.ty))
            .sum::<usize>()
    }
}
