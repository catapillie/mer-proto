use crate::com::{
    abt::{ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    syntax::types::{TypeAst, TypeAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_type(&mut self, ty: &TypeAst) -> TypeAbt {
        match &ty.kind {
            TypeAstKind::Bad => TypeAbt::Unknown,
            TypeAstKind::Unit => TypeAbt::Unit,
            TypeAstKind::Ref(ty) => TypeAbt::Ref(Box::new(self.analyse_type(ty))),
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
                    .annotate_primary(Note::Unknown, ty.span)
                    .done();
                self.diagnostics.push(d);

                TypeAbt::Unknown
            }
        }
    }

    pub fn type_of(&self, expr: &ExprAbt) -> TypeAbt {
        use ExprAbt as E;
        use TypeAbt as Ty;
        match expr {
            E::Unknown => Ty::Unknown,
            E::Unit => Ty::Unit,
            E::Integer(_) => Ty::I64,
            E::Decimal(_) => Ty::F64,
            E::Boolean(_) => Ty::Bool,

            E::Variable(var_id) => self.variables.get(var_id).unwrap().ty.clone(),
            E::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty.clone(),
            E::Assignment {
                var_id,
                deref_count,
                expr: _,
            } => {
                let mut result_ty = self.type_of(&E::Variable(*var_id));
                for _ in 0..(*deref_count) {
                    result_ty = match result_ty {
                        TypeAbt::Ref(ty) => *ty,
                        _ => unreachable!(),
                    }
                }
                result_ty
            }

            E::Binary(op, _, _) => op.out_ty.clone(),
            E::Unary(op, _) => op.ty.clone(),
            E::Debug(_, ty) => ty.clone(),

            E::Ref(inner) => Ty::Ref(Box::new(self.type_of(inner))),
            E::VarRef(var_id) => Ty::Ref(Box::new(self.type_of(&E::Variable(*var_id)))),
            E::Deref(inner) => match self.type_of(inner) {
                Ty::Ref(ty) => *ty,
                _ => unreachable!(),
            },
        }
    }
}
