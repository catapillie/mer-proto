use super::Analyser;
use crate::{
    com::{
        abt::{self, BoundPattern},
        ast,
    },
    diagnostics::{self, builder::DiagnosticBuilder, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{Span, Spanned},
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
    pub fn declare_pattern_bindings_by_expr(
        &mut self,
        pattern: &abt::Pattern,
        expr: &abt::Expr,
    ) -> BoundPattern {
        use abt::BoundPattern as B;
        use abt::ExprKind as K;
        use abt::PatternKind as Pat;
        use abt::Type as Ty;
        let ty = &expr.value.ty;
        match (&pattern.value, &expr.value.kind) {
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
            (Pat::Unit, K::Unit) => B::Discard {
                len: self.program.size_of(&Ty::Unit),
            },
            (Pat::Tuple(pat_hd, pat_tl), K::Tuple(hd, tl)) => {
                let mut bound_patterns = Vec::new();
                bound_patterns.push(self.declare_pattern_bindings_by_expr(pat_hd, hd));
                for (pat, expr) in pat_tl.iter().zip(tl.iter()) {
                    bound_patterns.push(self.declare_pattern_bindings_by_expr(pat, expr));
                }

                if pat_tl.len() != tl.len() {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let ty_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::TuplePatternMismatch(pat_repr, 1 + tl.len()))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span)
                        .annotate_secondary(
                            Note::TupleValueCount(tl.len() + 1),
                            expr.span,
                            NoteSeverity::Annotation,
                        )
                        .done();
                    self.diagnostics.push(d);
                }

                B::Seq(bound_patterns.into())
            }
            (Pat::Array(pats), K::Array(exprs)) => {
                let bound_patterns = pats
                    .iter()
                    .zip(exprs.iter())
                    .map(|(p, e)| self.declare_pattern_bindings_by_expr(p, e))
                    .collect();

                let size = exprs.len();
                if pats.len() != size {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let ty_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ArrayPatternMismatch(pat_repr, size))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span)
                        .annotate_secondary(
                            Note::ArrayLength(size),
                            expr.span,
                            NoteSeverity::Annotation,
                        )
                        .done();
                    self.diagnostics.push(d);
                }

                B::Seq(bound_patterns)
            }
            (Pat::Ref(pat), K::Heap(inner)) => B::Ref {
                pat: Box::new(self.declare_pattern_bindings_by_expr(pat, inner)),
                len: self.program.size_of(&inner.value.ty),
            },
            _ => self.declare_pattern_bindings_by_type(pattern, ty, Some((ty, expr.span))),
        }
    }

    pub fn declare_pattern_bindings_by_type(
        &mut self,
        pattern: &abt::Pattern,
        ty: &abt::Type,
        full_ty: Option<(&abt::Type, Span)>,
    ) -> BoundPattern {
        use abt::BoundPattern as B;
        use abt::PatternKind as Pat;
        use abt::Type as Ty;

        let extra = full_ty.map(|(full_ty, span)| {
            let highlight = match full_ty {
                Ty::Alias(id) => Some(self.program.aliases.get(id).unwrap().name.span),
                _ => None,
            };
            (self.program.type_repr(full_ty), span, highlight)
        });
        let add_extra_ann =
            |d: DiagnosticBuilder<DiagnosticKind, Severity, Option<Span>>| match extra {
                Some((full_ty_repr, span, Some(highlight))) => d
                    .highlight(highlight)
                    .annotate_secondary(Note::Type(full_ty_repr), span, NoteSeverity::Annotation),
                Some((full_ty_repr, span, None)) => {
                    d.annotate_secondary(Note::Type(full_ty_repr), span, NoteSeverity::Annotation)
                }
                None => d,
            };

        match (&pattern.value, self.program.dealias_type(ty).clone()) {
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
                bound_patterns.push(self.declare_pattern_bindings_by_type(pat_hd, &ty_hd, full_ty));
                for (pat, ty) in pat_tl.iter().zip(ty_tl.iter()) {
                    bound_patterns.push(self.declare_pattern_bindings_by_type(pat, ty, full_ty));
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
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span);
                    let d = add_extra_ann(d).done();
                    self.diagnostics.push(d);
                }

                B::Seq(bound_patterns.into())
            }
            (Pat::Array(pats), Ty::Array(inner, size)) => {
                let bound_patterns = pats
                    .iter()
                    .map(|p| self.declare_pattern_bindings_by_type(p, &inner, full_ty))
                    .collect();

                if pats.len() != size {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let ty_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ArrayPatternMismatch(pat_repr, size))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span);
                    let d = add_extra_ann(d).done();
                    self.diagnostics.push(d);
                }

                B::Seq(bound_patterns)
            }
            (Pat::Ref(pat), Ty::Ref(inner)) => B::Ref {
                pat: Box::new(self.declare_pattern_bindings_by_type(pat, &inner, full_ty)),
                len: self.program.size_of(&inner),
            },
            (Pat::OpaqueTypeConstructor(ctor_id, pats), Ty::Alias(alias_id)) => {
                let ctor_info = self.program.aliases.get(ctor_id).unwrap();
                let ctor_name = &ctor_info.name;

                let alias_info = self.program.aliases.get(&alias_id).unwrap();
                let alias_inner = alias_info.ty.clone();
                let alias_name = &alias_info.name;

                if *ctor_id != alias_id {
                    let type_repr = self.program.type_repr(ty);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::OpaqueTypeConstructorPatternMismatch(
                            ctor_name.value.clone(),
                            alias_name.value.clone(),
                        ))
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::PatternMustDescribe(type_repr), pattern.span);
                    let d = add_extra_ann(d).done();
                    self.diagnostics.push(d);
                    return B::Bad;
                }

                let Some((head, tail)) = pats.split_first() else {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(
                            DiagnosticKind::MissingPatternInOpaqueTypeConstructorPattern(
                                pat_repr,
                                alias_name.value.clone(),
                            ),
                        )
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::Here, pattern.span)
                        .done();
                    self.diagnostics.push(d);
                    return B::Bad;
                };

                let is_error = if !tail.is_empty() {
                    let pat_repr = self.program.pat_repr(&pattern.value);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(
                            DiagnosticKind::MoreThanOnePatternInOpaqueTypeConstructorPattern(
                                pat_repr,
                                alias_name.value.clone(),
                            ),
                        )
                        .with_severity(Severity::Error)
                        .with_span(pattern.span)
                        .annotate_primary(Note::Here, pattern.span)
                        .done();
                    self.diagnostics.push(d);
                    true
                } else {
                    false
                };

                let b = self.declare_pattern_bindings_by_type(head, &alias_inner, full_ty);
                if is_error {
                    B::Bad
                } else {
                    b
                }
            }
            _ => {
                let pat_repr = self.program.pat_repr(&pattern.value);
                let ty_repr = self.program.type_repr(ty);
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::PatternMismatch(pat_repr, ty_repr.clone()))
                    .with_severity(Severity::Error)
                    .with_span(pattern.span)
                    .annotate_primary(Note::PatternMustDescribe(ty_repr), pattern.span);
                let d = add_extra_ann(d).done();
                self.diagnostics.push(d);
                B::Bad
            }
        }
    }
}
