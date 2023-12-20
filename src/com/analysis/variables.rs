use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
    syntax::expr::ExprAst,
};

use super::{Analyser, Declaration};

pub struct VariableInfo {
    pub id: u64,
    pub name: String,
    pub depth: u16,
    pub ty: TypeAbt,
}

impl<'d> Analyser<'d> {
    pub fn declare_variable_here(&mut self, name: &str, ty: TypeAbt) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.to_string(), declared);

        let info = VariableInfo {
            id: declared,
            name: name.to_string(),
            depth: self.scope.depth,
            ty,
        };
        let prev = self.variables.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        Declaration { declared, shadowed }
    }

    pub fn get_variable(&self, name: &str) -> Option<&VariableInfo> {
        self.scope.search(|scope| match scope.bindings.get(name) {
            Some(id) => self.variables.get(id),
            None => None,
        })
    }

    pub fn analyse_variable_definition(
        &mut self,
        id: &Option<(String, Span)>,
        expr: &ExprAst,
    ) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);

        let Some((name, _)) = id else {
            return StmtAbtKind::Empty;
        };
        let decl = self.declare_variable_here(name, self.type_of(&bound_expr));

        // variable definitions are just (the first) assignment
        StmtAbtKind::Expr(Box::new(ExprAbt::Assignment(
            decl.declared,
            Box::new(bound_expr),
        )))
    }

    pub fn analyse_variable_expression(&mut self, name: &str, span: Span) -> ExprAbt {
        let Some(info) = self.get_variable(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownVariable(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let id = info.id;

        if let Some(func_id) = self.scope.current_func_id {
            let func_info = self.functions.get(&func_id).unwrap();
            if info.depth <= func_info.depth {
                let func_name = func_info.name.clone();
                let var_name = info.name.clone();
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnallowedVariableCapture {
                        func_name,
                        var_name,
                    })
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
            }
        }

        ExprAbt::Variable(id)
    }
}
