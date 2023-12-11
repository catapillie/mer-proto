use crate::com::{
    abt::{ExprAbt, StmtAbtKind, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
    syntax::expr::ExprAst,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn get_variable_by_id(&self, var_id: u64) -> Option<&TypeAbt> {
        let ids = self
            .variables
            .values()
            .filter(|(_, id)| id == &var_id)
            .collect::<Vec<_>>();
        assert_eq!(ids.len(), 1, "variable ids must be unique");
        ids.first().map(|(ty, _)| ty)
    }

    pub fn get_variable(&self, name: &str) -> Option<&(TypeAbt, u64)> {
        let depth = self.current_depth;
        let indices = self.current_offsets.iter().rev().enumerate();
        for (i, &offset) in indices {
            let depth = depth - i as u64 + 1;
            let entry = (name.to_string(), depth, offset);

            let var = self.variables.get(&entry);
            if var.is_some() {
                return var;
            }
        }
        None
    }

    pub fn declare_variable(&mut self, name: &str, ty: TypeAbt) -> u64 {
        let id = self.make_unique_id();
        let depth = self.current_depth;
        let offset = self.get_block_offset();

        let entry = (name.to_string(), depth, offset);
        let previous = self.variables.insert(entry, (ty, id));
        assert!(previous.is_none());

        id
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
        let id = self.declare_variable(name, self.type_of(&bound_expr));

        // variable definitions are just (the first) assignment
        StmtAbtKind::Expr(Box::new(ExprAbt::Assignment(id, Box::new(bound_expr))))
    }

    pub fn analyse_variable_expression(&mut self, name: &str, span: Span) -> ExprAbt {
        match self.get_variable(name) {
            Some(&(_, id)) => ExprAbt::Variable(id),
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownVariable(name.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }
}
