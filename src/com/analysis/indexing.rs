use diagnostics::{DiagnosticKind, Note, Severity};

use crate::{
    com::{abt::ExprAbt, diagnostics, syntax::expr::ExprAst, TypeAbt},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_immediate_index(&mut self, expr: &ExprAst, index: u64, span: Span) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        if let TypeAbt::Tuple(_, tail) = ty {
            self.analyse_tuple_immediate_index(expr, bound_expr, &tail, index, span)
        } else if let TypeAbt::Array(_, size) = ty {
            self.analyse_array_immediate_index(expr, bound_expr, index, size, span)
        } else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidImmediateIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(ty), expr.span)
                .done();
            self.diagnostics.push(d);
            ExprAbt::Unknown
        }
    }
}
