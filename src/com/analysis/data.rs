use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

use super::{Analyser, Declaration};
use crate::{
    com::{
        abt::{self, DataInfo},
        ast,
    },
    diagnostics::{self, DiagnosticKind, DiagnosticList, Note, NoteSeverity, Severity},
    utils::{Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn declare_data_structure_here(&mut self, name: &Spanned<String>) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.value.clone(), declared);
        let info = DataInfo {
            name: name.clone(),
            id: declared,
            fields: vec![],
            size: 0,
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
        let decl = self.declare_data_structure_here(name);
        let id = decl.declared;

        let mut field_spans = HashMap::new();
        for (field_name, _) in fields.iter() {
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
                        .highlight(name.span)
                        .done();
                    self.diagnostics.push(d);
                })
                .or_insert(field_name.span);
        }

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
            .map(|f| self.data_structure_size(&f.1.value, id))
            .fold(Some(0), |a, b| Some(a? + b?));
        let size = match size {
            Some(size) => size,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InfiniteDataStructure(name.value.clone()))
                    .with_span(name.span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::DataInfiniteSize(name.value.clone()), name.span)
                    .done();
                self.diagnostics.push(d);
                0
            }
        };

        let info = self
            .program
            .datas
            .get_mut(&id)
            .expect("data structure was just declared");

        info.size = size;
        info.fields = bound_fields;

        abt::StmtKind::Empty
    }

    pub fn data_structure_size(&self, ty: &abt::Type, data_id: u64) -> Option<usize> {
        use abt::Type as Ty;
        match ty {
            Ty::Unknown
            | Ty::Never
            | Ty::Unit
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::F32
            | Ty::F64
            | Ty::Bool => Some(1),
            Ty::Ref(_) => Some(1),
            Ty::Func(_, _) => Some(1),
            Ty::Pointer(_) => Some(2),
            Ty::Tuple(head, tail) => tail
                .iter()
                .map(|ty| self.data_structure_size(ty, data_id))
                .fold(self.data_structure_size(head, data_id), |a, b| {
                    Some(a? + b?)
                }),
            Ty::Array(ty, size) => Some(self.data_structure_size(ty, data_id)? * size),
            Ty::Data(id) => {
                if *id == data_id {
                    None
                } else {
                    Some(self.program.datas.get(id).unwrap().size)
                }
            }
        }
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
            let expr_ty = self.program.type_of(bound_expr);
            if !self.type_check_coerce(bound_expr, &expected_ty.value) {
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

    pub fn analyse_data_with_expression(
        &mut self,
        expr: &ast::Expr,
        fields: &[(Spanned<String>, ast::Expr)],
        span: Span,
    ) -> abt::Expr {
        let mut bound_expr = self.analyse_expression(expr);
        let bound_ty = self.program.type_of(&bound_expr);
        let mut bound_fields = fields
            .iter()
            .map(|(s, expr)| (s, self.analyse_expression(expr)))
            .collect::<Vec<_>>();

        if !bound_ty.is_known() {
            return abt::Expr::Unknown;
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
            return abt::Expr::Unknown;
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
            let expr_ty = self.program.type_of(bound_field);
            if !self.type_check_coerce(bound_field, &expected_ty.value) {
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

        abt::Expr::DataWith(id, Box::new(bound_expr), bound_with_fields)
    }

    fn try_coerce_data_structure(&self, expr: &mut abt::Expr) -> Option<u64> {
        let mut ty = self.program.type_of(expr);
        let mut deref_count = 0;
        let id = loop {
            match ty {
                abt::Type::Data(id) => break id,
                abt::Type::Ref(inner) => {
                    ty = *inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        for _ in 0..deref_count {
            let inner = std::mem::replace(expr, abt::Expr::Unknown);
            *expr = abt::Expr::Deref(Box::new(inner));
        }

        Some(id)
    }

    pub fn analyse_data_field_access(
        &mut self,
        expr: &ast::Expr,
        name: &Spanned<String>,
    ) -> abt::Expr {
        let mut bound_expr = self.analyse_expression(expr);

        let expr_ty = self.program.type_of(&bound_expr);
        if !expr_ty.is_known() {
            return abt::Expr::Unknown;
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
            return abt::Expr::Unknown;
        };

        let info = self.program.datas.get(&data_id).unwrap();
        let Some((field_id, _)) = info
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
            return abt::Expr::Unknown;
        };

        abt::Expr::FieldAccess {
            expr: Box::new(bound_expr),
            data_id,
            field_id,
        }
    }
}
