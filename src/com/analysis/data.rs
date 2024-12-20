use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use super::Analyser;
use crate::{
    com::{
        abt::{self, DataInfo, Size},
        ast::{self, stmt::DataDef},
    },
    diagnostics::{self, DiagnosticKind, DiagnosticList, Note, NoteSeverity, Severity},
    utils::{Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn get_data_structure(&self, name: &str) -> Option<&DataInfo> {
        let id = self.scope.search_id(name)?;

        if let Some(info) = self.program.datas.get(&id) {
            return Some(info);
        };

        let alias_info = self.program.aliases.get(&id)?;
        if alias_info.is_opaque {
            return None;
        }

        match self.program.dealias_type(&abt::Type::Alias(id)) {
            abt::Type::Data(id) => Some(self.program.datas.get(id)?),
            _ => None,
        }
    }

    pub fn analyse_data_structure_header(&mut self, ast: &DataDef) -> Option<u64> {
        if ast.is_opaque {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CannotMarkAsOpaque)
                .with_severity(Severity::Error)
                .with_span(ast.name.span)
                .annotate_primary(
                    Note::DataStructureMarkedOpaque(ast.name.value.clone()),
                    ast.name.span,
                )
                .done();
            self.diagnostics.push(d);
        }

        if let Some(shadowed) = self.scope.bindings.get(&ast.name.value) {
            if let Some(info) = self.program.datas.get(shadowed) {
                let shadowed_span = info.name.span;
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::DataStructureRedefinition(
                        ast.name.value.clone(),
                    ))
                    .with_span(ast.name.span)
                    .with_severity(Severity::Error)
                    .annotate_secondary(
                        Note::ShadowedDataStructure(ast.name.value.clone())
                            .dddot_back()
                            .num(1),
                        shadowed_span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::RedefinedDataStructure.and().dddot_front().num(2),
                        ast.name.span,
                    )
                    .done();
                self.diagnostics.push(d);
                return None;
            }
        }

        let id = self.make_unique_id();
        self.scope.bindings.insert(ast.name.value.clone(), id);
        self.program.datas.insert(
            id,
            DataInfo {
                name: ast.name.clone(),
                id,
                fields: Vec::new(),
                size: Size::Infinite,
            },
        );

        Some(id)
    }

    pub fn analyse_data_structure_definition(&mut self, ast: &DataDef, id: u64) {
        let mut field_spans = HashMap::new();
        for (field_name, _) in ast.fields.iter() {
            field_spans
                .entry(field_name.value.as_str())
                .and_modify(|prev_span| {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::FieldDeclaredMoreThanOnce(
                            field_name.value.clone(),
                        ))
                        .with_span(field_name.span)
                        .with_severity(Severity::Error)
                        .annotate_secondary(
                            Note::FieldDeclared(field_name.value.clone())
                                .dddot_back()
                                .num(1),
                            *prev_span,
                            NoteSeverity::Annotation,
                        )
                        .annotate_primary(
                            Note::FieldDeclaredAgain(field_name.value.clone())
                                .then()
                                .dddot_front()
                                .num(2),
                            field_name.span,
                        )
                        .highlight(ast.name.span)
                        .done();
                    self.diagnostics.push(d);
                })
                .or_insert(field_name.span);
        }

        let bound_fields = ast
            .fields
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

        let info = self.program.datas.get_mut(&id).unwrap();
        info.fields = bound_fields;
    }

    pub fn analyse_data_structure_sizes(&mut self, data_ids: &[u64]) {
        let mut map = HashMap::new();

        for &id in data_ids.iter() {
            let size = self.calculate_ty_size(&mut map, &abt::Type::Data(id));
            let info = self.program.datas.get_mut(&id).unwrap();
            match size {
                Size::Known(_) => info.size = size,
                Size::Infinite => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::InfiniteDataStructure(
                            info.name.value.clone(),
                        ))
                        .with_span(info.name.span)
                        .with_severity(Severity::Error)
                        .annotate_primary(
                            Note::DataInfiniteSize(info.name.value.clone()),
                            info.name.span,
                        )
                        .done();
                    self.diagnostics.push(d);
                }
            }
        }
    }

    fn calculate_ty_size(&self, map: &mut HashMap<u64, Option<Size>>, ty: &abt::Type) -> Size {
        let abt::Type::Data(id) = ty else {
            return self.program.size_of(ty);
        };

        match map.get(id) {
            Some(Some(size)) => *size,
            Some(None) => Size::Infinite,
            None => {
                map.insert(*id, None);
                let info = self.program.datas.get(id).unwrap();
                let size = info
                    .fields
                    .iter()
                    .map(|(_, ty)| self.calculate_ty_size(map, &ty.value))
                    .sum::<Size>();
                map.insert(*id, Some(size));
                size
            }
        }
    }

    pub fn analyse_data_init_expression(
        &mut self,
        name: &Spanned<String>,
        fields: &[(Spanned<String>, ast::Expr)],
    ) -> abt::TypedExpr {
        let mut bound_fields = fields
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
            return abt::TypedExpr::unknown();
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
        for ((id, bound_expr), (_, expr)) in bound_fields.iter_mut().zip(fields.iter()) {
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
            let expr_ty = bound_expr.value.ty.clone();
            if self.type_check_coerce(bound_expr, &expected_ty.value).is_err() {
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
            return abt::TypedExpr::unknown();
        }

        let bound_data_struct = bound_fields
            .into_iter()
            .sorted_by_cached_key(|(name, _)| required_fields.get(&name.value).unwrap().2)
            .map(|(_, e)| e)
            .collect();

        abt::TypedExpr {
            kind: abt::ExprKind::Data(info.id, bound_data_struct),
            ty: abt::Type::Data(info.id),
        }
    }

    pub fn analyse_data_with_expression(
        &mut self,
        expr: &ast::Expr,
        fields: &[(Spanned<String>, ast::Expr)],
        span: Span,
    ) -> abt::TypedExpr {
        let mut bound_expr = self.analyse_expression(expr);
        let bound_ty = bound_expr.value.ty.clone();
        let mut bound_fields = fields
            .iter()
            .map(|(s, expr)| (s, self.analyse_expression(expr)))
            .collect::<Vec<_>>();

        if !bound_ty.is_known() {
            return abt::TypedExpr::unknown();
        }

        let Some(id) = self.try_coerce_data_structure(&mut bound_expr) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidDataStructureExpression)
                .with_span(expr.span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::NotDataStructure(self.program.type_repr(&bound_ty)),
                    expr.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        let info = self.program.datas.get(&id).unwrap();

        // hashmap: name -> (span, type, id, set_span)
        let mut available_fields = info
            .fields
            .iter()
            .enumerate()
            .map(|(i, (name, ty))| (&name.value, (name.span, ty, i, None)))
            .collect::<BTreeMap<_, _>>();

        for ((id, bound_field), (_, expr)) in bound_fields.iter_mut().zip(fields) {
            let Some((field_span, field_ty, _, set_span)) = available_fields.get_mut(&id.value)
            else {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownFieldInDataStructure {
                        field_name: id.value.clone(),
                        data_name: info.name.value.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(id.span)
                    .annotate_primary(Note::Unknown, id.span)
                    .done();
                self.diagnostics.push(d);
                continue;
            };

            match set_span {
                None => *set_span = Some(id.span),
                Some(prev_span) => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::FieldSetMoreThanOnce(id.value.clone()))
                        .with_severity(Severity::Error)
                        .with_span(id.span)
                        .annotate_secondary(
                            Note::FieldSet(id.value.clone()).dddot_back().num(1),
                            *prev_span,
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
                    self.diagnostics.push(d);
                }
            }

            let expected_ty = field_ty;
            let expr_ty = bound_field.value.ty.clone();
            if self.type_check_coerce(bound_field, &expected_ty.value).is_err() {
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
                        field_span.join(expected_ty.span),
                        NoteSeverity::Annotation,
                    )
                    .highlight(info.name.span)
                    .done();
                self.diagnostics.push(d);
            }
        }

        let data_ty = abt::Type::Data(id);

        let total_field_count = available_fields.len();
        let reset_field_count = available_fields
            .values()
            .filter(|(_, _, _, set)| set.is_some())
            .count();

        if reset_field_count == total_field_count && total_field_count > 0 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::DiscardingWithExpression(
                    self.program.type_repr(&data_ty),
                ))
                .with_span(span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::DiscardedDataStructure, expr.span)
                .done();
            self.diagnostics.push(d);
        }

        if reset_field_count == 0 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyWithExpression(
                    self.program.type_repr(&data_ty),
                ))
                .with_span(span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::UnmodifiedDataStructure, expr.span)
                .done();
            self.diagnostics.push(d);
        }

        let bound_with_fields = bound_fields
            .into_iter()
            .filter_map(|(field_name, field_expr)| {
                available_fields
                    .get(&field_name.value)
                    .map(|t| (t.2, field_expr))
            })
            .sorted_by_cached_key(|&(id, _)| id)
            .collect::<Box<_>>();

        abt::TypedExpr {
            kind: abt::ExprKind::DataWith(id, Box::new(bound_expr), bound_with_fields),
            ty: abt::Type::Data(id),
        }
    }

    fn try_coerce_data_structure(&self, expr: &mut abt::Expr) -> Option<u64> {
        let mut ty = &expr.value.ty;
        ty = self.program.dealias_type(ty);

        let mut deref_count = 0;
        let id = loop {
            match ty {
                abt::Type::Data(id) => break *id,
                abt::Type::Ref(inner) => {
                    ty = &**inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        let span = expr.span;
        for _ in 0..deref_count {
            let inner = std::mem::replace(&mut expr.value, abt::TypedExpr::unknown());
            let abt::Type::Ref(ty) = inner.ty.clone() else {
                unreachable!()
            };

            *expr = abt::TypedExpr {
                kind: abt::ExprKind::Deref(Box::new(inner.wrap(span))),
                ty: *ty,
            }
            .wrap(span);
        }

        Some(id)
    }

    pub fn analyse_data_field_access(
        &mut self,
        expr: &ast::Expr,
        name: &Spanned<String>,
    ) -> abt::TypedExpr {
        let mut bound_expr = self.analyse_expression(expr);

        let expr_ty = bound_expr.value.ty.clone();
        if !expr_ty.is_known() {
            return abt::TypedExpr::unknown();
        }

        let Some(data_id) = self.try_coerce_data_structure(&mut bound_expr) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidFieldAccess(name.value.clone()))
                .with_severity(Severity::Error)
                .with_span(expr.span)
                .annotate_primary(
                    Note::NotDataStructure(self.program.type_repr(&expr_ty)),
                    expr.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        let info = self.program.datas.get(&data_id).unwrap();
        let Some((field_id, (_, field_ty))) = info
            .fields
            .iter()
            .enumerate()
            .find(|(_, (field_name, _))| field_name.value == name.value)
        else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFieldInDataStructure {
                    field_name: name.value.clone(),
                    data_name: info.name.value.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(name.span)
                .annotate_primary(Note::Unknown, name.span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        abt::TypedExpr {
            kind: abt::ExprKind::FieldAccess {
                expr: Box::new(bound_expr),
                data_id,
                field_id,
            },
            ty: field_ty.value.clone(),
        }
    }
}
