use std::collections::BTreeMap;

use super::Analyser;
use crate::{
    com::{
        abt::{self, DataInfo},
        ast,
    },
    diagnostics::{self, DiagnosticKind, DiagnosticList, Note, NoteSeverity, Severity},
    utils::Spanned,
};

impl<'d> Analyser<'d> {
    pub fn analyse_data_structure_definition(
        &mut self,
        name: &Spanned<String>,
        fields: &[(Spanned<String>, ast::Type)],
    ) -> abt::StmtKind {
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

        let size = bound_fields
            .iter()
            .map(|(_, ty)| self.program.size_of(&ty.value))
            .sum();

        let declared = self.make_unique_id();
        self.scope.bindings.insert(name.value.clone(), declared);

        let info = DataInfo {
            name: name.clone(),
            id: declared,
            fields: bound_fields,
            size,
        };
        let previous = self.program.datas.insert(declared, info);
        assert!(previous.is_none(), "id must be unique");

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
        ty_expr: &ast::Expr,
        fields: &[(Spanned<String>, ast::Expr)],
    ) -> abt::Expr {
        let ast::ExprKind::Identifier(id) = &ty_expr.value else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidDataStructureExpression)
                .with_severity(Severity::Error)
                .with_span(ty_expr.span)
                .annotate_primary(Note::Unknown, ty_expr.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        let bound_fields = fields
            .iter()
            .map(|(name, expr)| (name, self.analyse_expression(expr)))
            .collect::<Vec<_>>();

        let Some(info) = self.get_data_structure(id) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownDataStructure(id.clone()))
                .with_severity(Severity::Error)
                .with_span(ty_expr.span)
                .annotate_primary(Note::Unknown, ty_expr.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        // hashmap: name -> (span, type, is_set)
        let mut required_fields = info
            .fields
            .iter()
            .map(|(field_name, field_type)| {
                (
                    field_name.value.clone(),
                    (field_name.span, field_type, None),
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

            match req.2 {
                None => req.2 = Some(id.span),
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
            .filter(|(_, (_, _, s))| s.is_none())
            .map(|(name, (_, _, _))| name.clone())
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
                .with_span(ty_expr.span)
                .annotate_primary(
                    Note::MissingFields(missing_fields.into(), last_field),
                    ty_expr.span,
                )
                .done();
            diagnostics.push(d);
        }

        if !diagnostics.is_empty() {
            self.diagnostics.extend(diagnostics);
            return abt::Expr::Unknown;
        }

        abt::Expr::Unknown
    }
}
