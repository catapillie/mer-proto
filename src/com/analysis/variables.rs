use crate::com::{
    abt::{ExprAbt, StmtAbtKind},
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
    syntax::expr::ExprAst,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_variable_definition(
        &mut self,
        id: &Option<(String, Span)>,
        expr: &ExprAst,
    ) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);

        let Some((name, _)) = id else {
            return StmtAbtKind::Empty;
        };

        let id = self.make_unique_id();
        let depth = self.current_depth;
        let offset = self.get_block_offset();
        let entry = (name.to_string(), depth, offset);
        let previous = self.variables.insert(entry, (bound_expr.ty(), id));
        assert!(previous.is_none());

        // variable definitions are just (the first) assignment
        StmtAbtKind::Expr(Box::new(ExprAbt::Assignment(id, Box::new(bound_expr))))
    }

    pub fn analyse_variable_expression(&mut self, id: &str, span: Span) -> ExprAbt {
        let depth = self.current_depth;
        let offset = self.get_block_offset();
        let entry = (id.to_string(), depth, offset);
        match self.variables.get(&entry) {
            Some(&(_, id)) => ExprAbt::Variable(id),
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownVariable(id.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }
}
