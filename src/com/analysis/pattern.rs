use super::Analyser;
use crate::{
    com::{
        abt::{self, BoundPattern},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
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
            ast::PatternKind::Constructor(name, patterns) => {
                let bound_patterns = patterns.iter().map(|p| self.analyse_pattern(p)).collect();

                let Some(info) = self.get_type_alias(&name.value) else {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::UnknownTypeConstructor(name.value.clone()))
                        .with_severity(Severity::Error)
                        .with_span(name.span)
                        .annotate_primary(Note::Unknown, name.span)
                        .done();
                    self.diagnostics.push(d);
                    return abt::PatternKind::Discard;
                };

                if !info.is_opaque {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::NonOpaqueTypeConstructor(
                            name.value.to_string(),
                        ))
                        .with_severity(Severity::Error)
                        .with_span(name.span)
                        .annotate_secondary(
                            Note::MarkedAsOpaque(name.value.to_string())
                                .dddot_back()
                                .num(1),
                            info.name.span,
                            NoteSeverity::Annotation,
                        )
                        .annotate_primary(
                            Note::DoesNotHaveConstructor(name.value.to_string())
                                .so()
                                .dddot_front()
                                .num(2),
                            name.span,
                        )
                        .done();
                    self.diagnostics.push(d);
                    return abt::PatternKind::Discard;
                }

                abt::PatternKind::OpaqueTypeConstructor(info.id, bound_patterns)
            }
            ast::PatternKind::Unit => abt::PatternKind::Unit,
            ast::PatternKind::Parenthesized(pat) => self.analyse_pattern_kind(pat),
            ast::PatternKind::Tuple(head, tail) => abt::PatternKind::Tuple(
                Box::new(self.analyse_pattern(head)),
                tail.iter().map(|p| self.analyse_pattern(p)).collect(),
            ),
            ast::PatternKind::Array(pats) => {
                abt::PatternKind::Array(pats.iter().map(|p| self.analyse_pattern(p)).collect())
            }
            ast::PatternKind::Ref(pat) => {
                abt::PatternKind::Ref(Box::new(self.analyse_pattern(pat)))
            }
        }
    }

    pub fn declare_pattern_bindings(
        &mut self,
        pattern: &abt::Pattern,
        ty: &abt::Type,
    ) -> BoundPattern {
        use abt::BoundPattern as B;
        use abt::PatternKind as Pat;
        use abt::Type as Ty;
        match (&pattern.value, ty) {
            (Pat::Discard, _) => B::Discard {
                len: self.program.size_of(ty),
            },
            (Pat::Binding(name), _) => {
                let decl = self.declare_variable_here(
                    Spanned {
                        span: pattern.span,
                        value: name.clone(),
                    },
                    ty.clone(),
                );
                B::Loc { id: decl.declared }
            }
            (Pat::Unit, Ty::Unit) => B::Discard {
                len: self.program.size_of(&Ty::Unit),
            },
            (Pat::Tuple(pat_hd, pat_tl), Ty::Tuple(ty_hd, ty_tl)) => {
                let mut bound_patterns = Vec::new();
                bound_patterns.push(self.declare_pattern_bindings(pat_hd, ty_hd));
                for (pat, ty) in pat_tl.iter().zip(ty_tl.iter()) {
                    bound_patterns.push(self.declare_pattern_bindings(pat, ty));
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

                B::Seq(bound_patterns.into())
            }
            (Pat::Array(pats), Ty::Array(inner, size)) => {
                let bound_patterns = pats
                    .iter()
                    .map(|p| self.declare_pattern_bindings(p, inner))
                    .collect();

                if pats.len() != *size {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let ty_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ArrayPatternMismatch(pat_repr, *size))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span)
                        .done();
                    self.diagnostics.push(d);
                }

                B::Seq(bound_patterns)
            }
            (Pat::Ref(pat), Ty::Ref(inner)) => B::Ref {
                pat: Box::new(self.declare_pattern_bindings(pat, inner)),
                len: self.program.size_of(inner),
            },
            (Pat::OpaqueTypeConstructor(ctor_id, pats), Ty::Alias(alias_id)) => {
                assert_eq!(ctor_id, alias_id, "matching against different opaque types");

                let Some((head, tail)) = pats.split_first() else {
                    todo!("missing pattern in constructor")
                };

                if !tail.is_empty() {
                    todo!("more than one pattern in opaque type constructor")
                }

                let inner = self.program.aliases.get(alias_id).unwrap().ty.clone();
                self.declare_pattern_bindings(head, &inner)
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
                B::Bad
            }
        }
    }
}
