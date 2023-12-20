use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    analysis::Declaration,
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
    syntax::{expr::ExprAst, stmt::StmtAst, types::TypeAst},
};

use super::Analyser;

pub struct FunctionInfo {
    pub id: u64,
    pub name: String,
    pub args: Vec<(String, TypeAbt)>,
    pub ty: TypeAbt,
}

impl<'d> Analyser<'d> {
    pub fn declare_function_here(
        &mut self,
        name: &str,
        args: Vec<(String, TypeAbt)>,
        ty: TypeAbt,
    ) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.to_string(), declared);

        let info = FunctionInfo {
            id: declared,
            name: name.to_string(),
            args,
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
        name: &Option<String>,
        args: &[(String, TypeAst)],
        body: &StmtAst,
        ty: &TypeAst,
    ) -> StmtAbtKind {
        let Some(name) = name else {
            return StmtAbtKind::Empty;
        };

        let info = match self.get_function(name) {
            Some(info) => info, // already declared
            None => {
                // must be declared now
                let bound_args = args
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.analyse_type(ty)))
                    .collect();
                let bound_ty = self.analyse_type(ty);
                let decl = self.declare_function_here(name, bound_args, bound_ty);
                self.functions.get(&decl.declared).unwrap() // was just declared, cannot be none
            }
        };

        let ty = info.ty.clone();
        let args = info.args.clone();

        self.open_scope();
        self.scope.return_type = ty;

        for (arg_name, arg_ty) in args {
            self.declare_variable(arg_name.as_str(), arg_ty);
        }
        let _body = self.analyse_statement(body);

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

        let name = callee;
        let Some(info) = self.get_function(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFunction(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let bound_args = info.args.clone();
        let id = info.id;
        let ty = info.ty.clone();

        let mut invalid = false;
        for ((bound_arg, span), arg_ty) in bound_params
            .iter()
            .zip(args.iter().map(|arg| arg.span))
            .zip(bound_args.iter().map(|(_, arg_ty)| arg_ty))
        {
            let ty_param = self.type_of(bound_arg);
            if !ty_param.is(arg_ty) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: ty_param.clone(),
                        expected: arg_ty.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
                invalid = true;
            }
        }

        if bound_params.len() != bound_args.len() {
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
            ExprAbt::Call(id, bound_params, ty.clone())
        }
    }

    pub fn analyse_return_statement(&mut self, span: Span) -> StmtAbtKind {
        let ty = TypeAbt::Unit;
        let return_ty = &self.scope.return_type;

        if !ty.is(return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MustReturnValue {
                    expected: return_ty.clone(),
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
        let ty_expr = self.type_of(&bound_expr);
        let return_ty = &self.scope.return_type;

        if !ty_expr.is(return_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: ty_expr,
                    expected: return_ty.clone(),
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
