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
            TypeAstKind::Tuple(head, tail) => TypeAbt::Tuple(
                Box::new(self.analyse_type(head)),
                tail.iter().map(|ty| self.analyse_type(ty)).collect(),
            ),
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
            TypeAstKind::Func(arg_tys, ret_ty) => {
                let bound_arg_tys = arg_tys.iter().map(|ty| self.analyse_type(ty)).collect();
                let bound_ret_ty = self.analyse_type(ret_ty);
                TypeAbt::Func(bound_arg_tys, Box::new(bound_ret_ty))
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

            E::Tuple(head, tail) => Ty::Tuple(
                Box::new(self.type_of(head)),
                tail.iter().map(|e| self.type_of(e)).collect(),
            ),

            E::Variable(var_id) => self.variables.get(var_id).unwrap().ty.clone(),
            E::Function(func_id) => {
                let info = self.functions.get(func_id).unwrap();
                let args = info
                    .args
                    .iter()
                    .map(|(_, ty)| ty.clone())
                    .collect::<Box<_>>();
                Ty::Func(args, Box::new(info.ty.clone()))
            }

            E::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty.clone(),
            E::IndirectCall(_, _, ty) => ty.clone(),

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

            E::Todo => Ty::Never,
            E::Unreachable => Ty::Never,
        }
    }

    pub fn size_of(ty: &TypeAbt) -> usize {
        match ty {
            TypeAbt::Unknown => 1,
            TypeAbt::Never => 1,
            TypeAbt::Unit => 1,
            TypeAbt::U8 => 1,
            TypeAbt::U16 => 1,
            TypeAbt::U32 => 1,
            TypeAbt::U64 => 1,
            TypeAbt::I8 => 1,
            TypeAbt::I16 => 1,
            TypeAbt::I32 => 1,
            TypeAbt::I64 => 1,
            TypeAbt::F32 => 1,
            TypeAbt::F64 => 1,
            TypeAbt::Bool => 1,
            TypeAbt::Tuple(head, tail) => {
                Self::size_of(head) + tail.iter().map(Self::size_of).sum::<usize>()
            }
            TypeAbt::Ref(_) => 1,
            TypeAbt::Func(_, _) => 1,
        }
    }
}
