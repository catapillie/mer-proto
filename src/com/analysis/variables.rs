use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    span::Span,
    syntax::expr::ExprAst,
};

use super::{functions::VariableUsage, Analyser, Declaration};

pub struct VariableInfo {
    pub id: u64,
    pub name: String,
    pub depth: u16,
    pub ty: TypeAbt,
    pub declaration_span: Span,
}

impl<'d> Analyser<'d> {
    pub fn declare_variable_here(&mut self, name: &str, ty: TypeAbt, span: Span) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.to_string(), declared);

        let info = VariableInfo {
            id: declared,
            name: name.to_string(),
            depth: self.scope.depth,
            ty,
            declaration_span: span,
        };
        let prev = self.variables.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        if let Some(func_id) = self.scope.current_func_id {
            let func_info = self.functions.get_mut(&func_id).unwrap();
            func_info
                .used_variables
                .insert(declared, VariableUsage { captured: false });
        }

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
        span: Span,
    ) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);

        let Some((name, _)) = id else {
            return StmtAbtKind::Empty;
        };
        let decl = self.declare_variable_here(name, self.type_of(&bound_expr), span);

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
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let id = info.id;
        let depth = info.depth;
        let declaration_span = info.declaration_span;

        if let Some(func_id) = self.scope.current_func_id {
            let func_info = self.functions.get_mut(&func_id).unwrap();
            let captured = depth <= func_info.depth;

            func_info
                .used_variables
                .entry(id)
                .and_modify(|u| u.captured = captured)
                .or_insert(VariableUsage { captured });

            if captured {
                let func_name = func_info.name.clone();
                let var_name = name.to_string();
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnallowedVariableCapture {
                        func_name,
                        var_name,
                    })
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_primary(
                        Note::VariableCapturedBy(name.to_string(), func_info.name.to_string())
                            .then()
                            .dddot_front()
                            .num(2),
                        span,
                    )
                    .annotate_secondary(
                        Note::VariableDeclaration(name.to_string())
                            .dddot_back()
                            .num(1),
                        declaration_span,
                        NoteSeverity::Annotation,
                    )
                    .done();
                self.diagnostics.push(d);
            }
        }

        ExprAbt::Variable(id)
    }
}
