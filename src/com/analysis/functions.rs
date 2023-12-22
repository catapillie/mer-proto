use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    analysis::Declaration,
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    span::Span,
    syntax::{expr::ExprAst, stmt::StmtAst, types::TypeAst},
};

use super::Analyser;

pub struct FunctionInfo {
    pub id: u64,
    pub name: String,
    pub span: Span,
    pub depth: u16,
    pub args: Vec<(String, TypeAbt)>,
    pub arg_ids: Vec<u64>,
    pub ty: TypeAbt,
}

impl<'d> Analyser<'d> {
    pub fn declare_function_here(
        &mut self,
        name: &str,
        span: Span,
        args: Vec<(String, TypeAbt)>,
        ty: TypeAbt,
    ) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.to_string(), declared);

        let info = FunctionInfo {
            id: declared,
            name: name.to_string(),
            span,
            depth: self.scope.depth,
            args,
            arg_ids: Default::default(),
            ty,
        };
        let prev = self.functions.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        Declaration { declared, shadowed }
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.scope.search(|scope| match scope.bindings.get(name) {
            Some(id) => self.functions.get(id),
            None => None,
        })
    }

    pub fn analyse_function_definition(
        &mut self,
        name: &Option<(String, Span)>,
        args: &[(String, TypeAst, Span)],
        body: &StmtAst,
        ty: &TypeAst,
    ) -> StmtAbtKind {
        let Some((name, span)) = name else {
            return StmtAbtKind::Empty;
        };

        let info = match self.get_function(name) {
            Some(info) => info, // already declared
            None => {
                // must be declared now
                let bound_args = args
                    .iter()
                    .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
                    .collect();
                let bound_ty = self.analyse_type(ty);
                let decl = self.declare_function_here(name, *span, bound_args, bound_ty);
                self.functions.get(&decl.declared).unwrap() // was just declared, cannot be none
            }
        };

        assert_eq!(info.args.len(), args.len());
        let bound_args = info.args.clone();
        let bound_spanned_args = bound_args.iter().zip(args.iter().map(|(_, _, span)| span));
        let id = info.id;

        self.open_scope();
        self.scope.current_func_id = Some(id);

        for ((arg_name, arg_ty), &span) in bound_spanned_args {
            let decl = self.declare_variable_here(arg_name.as_str(), arg_ty.clone(), span);
            self.functions
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

        StmtAbtKind::Empty
    }

    pub fn analyse_call_expression(
        &mut self,
        callee: &str,
        args: &[ExprAst],
        span: Span,
    ) -> ExprAbt {
        let bound_args = args
            .iter()
            .map(|arg| self.analyse_expression(arg))
            .collect::<Vec<_>>();

        let name = callee;
        let Some(info) = self.get_function(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFunction(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let func_span = info.span;
        let func_name = info.name.clone();
        let func_args = info.args.clone();
        let func_arg_ids = info.arg_ids.clone();
        let id = info.id;
        let ty = info.ty.clone();

        let mut invalid = false;
        for (((i, bound_arg), span), arg_ty) in bound_args
            .iter()
            .enumerate()
            .zip(args.iter().map(|arg| arg.span))
            .zip(func_args.iter().map(|(_, arg_ty)| arg_ty))
        {
            let ty_param = self.type_of(bound_arg);
            if !ty_param.is(arg_ty) {
                let arg_id = func_arg_ids.get(i).unwrap();
                let arg_info = self.variables.get(&arg_id).unwrap();
                let arg_name = arg_info.name.clone();
                let arg_span = arg_info.declaration_span;
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: ty_param.clone(),
                        expected: arg_ty.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_secondary(
                        Note::ArgumentType(arg_name, arg_ty.clone())
                            .dddot_back()
                            .num(1),
                        arg_span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::MustBeOfType(arg_ty.clone()).so().dddot_front().num(2),
                        span,
                    )
                    .done();
                self.diagnostics.push(d);
                invalid = true;
            }
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
                    Note::FunctionArgs(func_name, func_args.len())
                        .dddot_back()
                        .num(1),
                    func_span,
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            ExprAbt::Unknown
        } else {
            ExprAbt::Call(id, bound_args, ty.clone())
        }
    }

    pub fn analyse_return_statement(&mut self, span: Span) -> StmtAbtKind {
        let ty = TypeAbt::Unit;
        let return_ty = match self.scope.current_func_id {
            Some(id) => self.functions.get(&id).unwrap().ty.clone(),
            None => TypeAbt::Unit,
        };

        if !ty.is(&return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MustReturnValue {
                    expected: return_ty.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::MustBeOfType(return_ty.clone()), span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(ExprAbt::Unit))
    }

    pub fn analyse_return_with_statement(&mut self, expr: &ExprAst) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);
        let ty_expr = self.type_of(&bound_expr);
        let return_ty = match self.scope.current_func_id {
            Some(id) => self.functions.get(&id).unwrap().ty.clone(),
            None => TypeAbt::Unit,
        };

        if !ty_expr.is(&return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: ty_expr,
                    expected: return_ty.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(expr.span)
                .annotate_primary(Note::MustBeOfType(return_ty.clone()), expr.span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(bound_expr))
    }
}
