use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Spanned,
};

impl<'d> Analyser<'d> {
    pub fn analyse_pattern(&mut self, pattern: &ast::Pattern) -> abt::Pattern {
        self.analyse_pattern_kind(pattern).wrap(pattern.span)
    }

    fn analyse_pattern_kind(&mut self, pat: &ast::Pattern) -> abt::PatternKind {
        match &pat.value {
            ast::PatternKind::Bad => abt::PatternKind::Discard,
            ast::PatternKind::Discard => abt::PatternKind::Discard,
            ast::PatternKind::Binding(name) => abt::PatternKind::Binding(name.clone()),
            ast::PatternKind::Unit => abt::PatternKind::Unit,
            ast::PatternKind::Parenthesized(pat) => self.analyse_pattern_kind(pat),
            ast::PatternKind::Tuple(head, tail) => abt::PatternKind::Tuple(
                Box::new(self.analyse_pattern(head)),
                tail.iter().map(|p| self.analyse_pattern(p)).collect(),
            ),
        }
    }

    pub fn declare_pattern_bindings(&mut self, pattern: &abt::Pattern, ty: &abt::Type) {
        use abt::PatternKind as Pat;
        use abt::Type as Ty;
        match (&pattern.value, ty) {
            (Pat::Discard, _) => (),
            (Pat::Binding(name), ty) => {
                self.declare_variable_here(
                    Spanned {
                        span: pattern.span,
                        value: name.clone(),
                    },
                    ty.clone(),
                );
            }
            (Pat::Unit, Ty::Unit) => (),
            (Pat::Tuple(pat_hd, pat_tl), Ty::Tuple(ty_hd, ty_tl)) => {
                self.declare_pattern_bindings(pat_hd, ty_hd);
                for (pat, ty) in pat_tl.iter().zip(ty_tl.iter()) {
                    self.declare_pattern_bindings(pat, ty);
                }
                if pat_tl.len() != ty_tl.len() {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let ty_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::TuplePatternMismatch(
                            pat_repr,
                            1 + ty_tl.len(),
                        ))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span)
                        .done();
                    self.diagnostics.push(d);
                }
            }
            _ => {
                let pat_repr = self.program.pat_repr(&pattern.value);
                let ty_repr = self.program.type_repr(ty);
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::PatternMismatch(pat_repr, ty_repr.clone()))
                    .with_severity(Severity::Error)
                    .with_span(pattern.span)
                    .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span)
                    .done();
                self.diagnostics.push(d);
            }
        }
    }
}
