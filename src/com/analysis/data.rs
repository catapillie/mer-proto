use itertools::Itertools;
use std::collections::BTreeMap;

use super::{Analyser, Declaration};
use crate::{
    com::{
        abt::{self, DataInfo},
        ast,
    },
    diagnostics::{self, DiagnosticKind, DiagnosticList, Note, NoteSeverity, Severity},
    utils::Spanned,
};

impl<'d> Analyser<'d> {
    pub fn declare_data_structure_here(
        &mut self,
        name: &Spanned<String>,
        fields: Vec<(Spanned<String>, Spanned<abt::Type>)>,
    ) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.value.clone(), declared);

        let size = fields
            .iter()
            .map(|(_, ty)| self.program.size_of(&ty.value))
            .sum();

        let info = DataInfo {
            name: name.clone(),
            id: declared,
            fields,
            size,
        };
        let prev = self.program.datas.insert(declared, info);
        assert!(prev.is_none(), "id must be unique");

        Declaration { declared, shadowed }
    }

    pub fn analyse_data_structure_definition(
        &mut self,
        name: &Spanned<String>,
        fields: &[(Spanned<String>, ast::Type)],
    ) -> abt::StmtKind {
        if self.get_data_structure(&name.value).is_some() {
            return abt::StmtKind::Empty;
        }

        // data structure has already not been declared yet, let's do it now
        let bound_fields = fields
            .iter()
            .map(|(name, ty)| {
                (
                    name.clone(),
                    Spanned {
                        value: self.analyse_type(ty),
                        span: ty.span,
                    },
                )
            })
            .collect::<Vec<_>>();

        self.declare_data_structure_here(name, bound_fields);
        abt::StmtKind::Empty
    }

    pub fn get_data_structure(&self, name: &str) -> Option<&DataInfo> {
        self.scope.search(|scope| match scope.bindings.get(name) {
            Some(id) => self.program.datas.get(id),
            None => None,
        })
    }

    pub fn analyse_data_init_expression(
        &mut self,
        name: &Spanned<String>,
        fields: &[(Spanned<String>, ast::Expr)],
    ) -> abt::Expr {
        let bound_fields = fields
            .iter()
            .map(|(name, expr)| (name, self.analyse_expression(expr)))
            .collect::<Vec<_>>();

        let Some(info) = self.get_data_structure(&name.value) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownDataStructure(name.value.clone()))
                .with_severity(Severity::Error)
                .with_span(name.span)
                .annotate_primary(Note::Unknown, name.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        // hashmap: name -> (span, type, id, set_span)
        let mut required_fields = info
            .fields
            .iter()
            .enumerate()
            .map(|(i, (field_name, field_type))| {
                (
                    field_name.value.clone(),
                    (field_name.span, field_type, i, None),
                )
            })
            .collect::<BTreeMap<_, _>>();

        let mut diagnostics = DiagnosticList::new();
        for ((id, bound_expr), (_, expr)) in bound_fields.iter().zip(fields.iter()) {
            let Some(req) = required_fields.get_mut(&id.value) else {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownFieldInDataStructure {
                        field_name: id.value.clone(),
                        data_name: info.name.value.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(id.span)
                    .annotate_primary(Note::Unknown, id.span)
                    .done();
                diagnostics.push(d);
                continue;
            };

            match req.3 {
                None => req.3 = Some(id.span),
                Some(prev_span) => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::FieldSetMoreThanOnce(id.value.clone()))
                        .with_severity(Severity::Error)
                        .with_span(id.span)
                        .annotate_secondary(
                            Note::FieldSet(id.value.clone()).dddot_back().num(1),
                            prev_span,
                            NoteSeverity::Annotation,
                        )
                        .annotate_primary(
                            Note::FieldSetAgain(id.value.clone())
                                .then()
                                .dddot_front()
                                .num(2),
                            id.span,
                        )
                        .done();
                    diagnostics.push(d);
                }
            }

            let expected_ty = req.1;
            let expr_ty = self.program.type_of(bound_expr);
            if !expr_ty.is(&expected_ty.value) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: self.program.type_repr(&expr_ty),
                        expected: self.program.type_repr(&expected_ty.value),
                    })
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(
                        Note::MustBeOfType(self.program.type_repr(&expected_ty.value))
                            .so()
                            .dddot_front()
                            .num(2),
                        expr.span,
                    )
                    .annotate_secondary(
                        Note::FieldType(
                            id.value.clone(),
                            self.program.type_repr(&expected_ty.value),
                        )
                        .dddot_back()
                        .num(1),
                        req.0.join(expected_ty.span),
                        NoteSeverity::Annotation,
                    )
                    .highlight(info.name.span)
                    .done();
                diagnostics.push(d);
            }
        }

        let mut missing_fields = required_fields
            .iter()
            .filter(|(_, (_, _, _, s))| s.is_none())
            .map(|(name, (_, _, _, _))| name.clone())
            .collect::<Vec<_>>();
        if !missing_fields.is_empty() {
            let last_field = missing_fields.pop().unwrap();
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::FieldsNeverSet(
                    info.name.value.clone(),
                    missing_fields.clone().into(),
                    last_field.clone(),
                ))
                .with_severity(Severity::Error)
                .with_span(name.span)
                .annotate_primary(
                    Note::MissingFields(missing_fields.into(), last_field),
                    name.span,
                )
                .done();
            diagnostics.push(d);
        }

        if !diagnostics.is_empty() {
            self.diagnostics.extend(diagnostics);
            return abt::Expr::Unknown;
        }

        let bound_data_struct = bound_fields
            .into_iter()
            .sorted_by_cached_key(|(name, _)| required_fields.get(&name.value).unwrap().2)
            .map(|(_, e)| e)
            .collect();

        abt::Expr::Data(info.id, bound_data_struct)
    }
}
