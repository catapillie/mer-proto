use crate::com::{
    abt::{ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    syntax::types::{TypeAst, TypeAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_type(&mut self, ty: &TypeAst) -> TypeAbt {
        match &ty.kind {
            TypeAstKind::Bad => TypeAbt::Unknown,
            TypeAstKind::Unit => TypeAbt::Unit,
            TypeAstKind::Declared(id) => {
                match id.as_str() {
                    "u8" => return TypeAbt::U8,
                    "u16" => return TypeAbt::U16,
                    "u32" => return TypeAbt::U32,
                    "u64" => return TypeAbt::U64,
                    "i8" => return TypeAbt::I8,
                    "i16" => return TypeAbt::I16,
                    "i32" => return TypeAbt::I32,
                    "i64" => return TypeAbt::I64,
                    "f32" => return TypeAbt::F32,
                    "f64" => return TypeAbt::F64,
                    "bool" => return TypeAbt::Bool,
                    _ => {}
                };

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownType(id.clone()))
                    .with_severity(Severity::Error)
                    .with_span(ty.span)
                    .done();
                self.diagnostics.push(d);

                TypeAbt::Unknown
            }
        }
    }

    pub fn type_of(&self, expr: &ExprAbt) -> TypeAbt {
        match expr {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Unit => TypeAbt::Unit,
            ExprAbt::Integer(_) => TypeAbt::I64,
            ExprAbt::Decimal(_) => TypeAbt::F64,
            ExprAbt::Boolean(_) => TypeAbt::Bool,

            ExprAbt::Variable(var_id) => self.variables.get(var_id).unwrap().ty.clone(),
            ExprAbt::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty.clone(),
            ExprAbt::Assignment(id, _) => self.type_of(&ExprAbt::Variable(*id)),

            ExprAbt::Binary(op, _, _) => op.out_ty.clone(),
            ExprAbt::Unary(op, _) => op.ty.clone(),
            ExprAbt::Debug(inner) => self.type_of(inner),
        }
    }
}
