use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
    syntax::{expr::ExprAst, stmt::StmtAst, types::TypeAst},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_function_definition(
        &mut self,
        name: &Option<String>,
        args: &[(String, TypeAst)],
        body: &StmtAst,
        _ty: &TypeAst,
    ) -> StmtAbtKind {
        let Some(name) = name else {
            return StmtAbtKind::Empty;
        };

        let depth = self.current_depth;
        let offset = self.get_block_offset();
        let entry = (name.clone(), depth, offset);
        let (bound_args_ty, _, _) = self
            .functions
            .get(&entry)
            .cloned()
            .expect("declaration must have been reached");

        assert_eq!(args.len(), bound_args_ty.len());

        self.open_scope();
        let bound_args = args.iter().map(|(name, _)| name).zip(bound_args_ty);
        for (arg_name, arg_ty) in bound_args {
            self.declare_variable(arg_name, arg_ty);
        }
        let _ = self.analyse_statement(body);
        self.close_scope();

        StmtAbtKind::Empty
    }

    pub fn analyse_call_expression(
        &mut self,
        callee: &str,
        args: &[ExprAst],
        span: Span,
    ) -> ExprAbt {
        let bound_params = args
            .iter()
            .map(|arg| self.analyse_expression(arg))
            .collect::<Vec<_>>();

        let name = callee.to_string();
        let depth = self.current_depth;
        let offset = self.get_block_offset();
        let entry = (name.clone(), depth, offset);
        let Some(func) = self.functions.get(&entry) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFunction(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let (arg_types, return_type, id) = func;

        let mut invalid = false;
        for ((bound_param, param), expected_ty) in
            bound_params.iter().zip(args).zip(arg_types)
        {
            let ty_param = bound_param.ty();
            if !ty_param.is(expected_ty) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: ty_param.clone(),
                        expected: expected_ty.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(param.span)
                    .done();
                self.diagnostics.push(d);
                invalid = true;
            }
        }

        if bound_params.len() != arg_types.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidParameterCount {
                    got: bound_params.len(),
                    expected: bound_params.len(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            ExprAbt::Unknown
        } else {
            ExprAbt::Call(*id, bound_params, return_type.clone())
        }
    }

    pub fn analyse_return_statement(&mut self, span: Span) -> StmtAbtKind {
        let ty = TypeAbt::Unit;
        let return_ty = self.get_return_type();

        if !ty.is(&return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MustReturnValue {
                    expected: return_ty,
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(ExprAbt::Unit))
    }

    pub fn analyse_return_with_statement(&mut self, expr: &ExprAst) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);
        let ty_expr = bound_expr.ty();
        let return_ty = self.get_return_type();

        if !ty_expr.is(&return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: ty_expr,
                    expected: return_ty,
                })
                .with_severity(Severity::Error)
                .with_span(expr.span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(bound_expr))
    }
}
