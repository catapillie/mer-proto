use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

impl<'d> Analyser<'d> {
    pub fn analyse_type(&mut self, ty: &ast::Type) -> abt::Type {
        match &ty.value {
            ast::TypeKind::Bad => abt::Type::Unknown,
            ast::TypeKind::Unit => abt::Type::Unit,
            ast::TypeKind::Tuple(head, tail) => abt::Type::Tuple(
                Box::new(self.analyse_type(head)),
                tail.iter().map(|ty| self.analyse_type(ty)).collect(),
            ),
            ast::TypeKind::Array(inner, size) => {
                abt::Type::Array(Box::new(self.analyse_type(inner)), *size)
            }
            ast::TypeKind::Pointer(inner) => abt::Type::Pointer(Box::new(self.analyse_type(inner))),
            ast::TypeKind::Ref(ty) => abt::Type::Ref(Box::new(self.analyse_type(ty))),
            ast::TypeKind::Declared(name) => {
                match name.as_str() {
                    "u8" => return abt::Type::U8,
                    "u16" => return abt::Type::U16,
                    "u32" => return abt::Type::U32,
                    "u64" => return abt::Type::U64,
                    "i8" => return abt::Type::I8,
                    "i16" => return abt::Type::I16,
                    "i32" => return abt::Type::I32,
                    "i64" => return abt::Type::I64,
                    "f32" => return abt::Type::F32,
                    "f64" => return abt::Type::F64,
                    "bool" => return abt::Type::Bool,
                    _ => {}
                };

                let Some(id) = self.scope.search_id(name) else {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::UnknownType(name.clone()))
                        .with_severity(Severity::Error)
                        .with_span(ty.span)
                        .annotate_primary(Note::Unknown, ty.span)
                        .done();
                    self.diagnostics.push(d);
                    return abt::Type::Unknown;
                };

                if self.program.datas.contains_key(&id) {
                    abt::Type::Data(id)
                } else if self.program.aliases.contains_key(&id) {
                    abt::Type::Alias(id)
                } else {
                    unreachable!()
                }
            }
            ast::TypeKind::Func(arg_tys, ret_ty) => {
                let bound_arg_tys = arg_tys.iter().map(|ty| self.analyse_type(ty)).collect();
                let bound_ret_ty = self.analyse_type(ret_ty);
                abt::Type::Func(bound_arg_tys, Box::new(bound_ret_ty))
            }
        }
    }

    #[rustfmt::skip]
    pub fn type_check(&self, left: &abt::Type, right: &abt::Type) -> bool {
        use abt::Type::*;
        match (left, right) {
            (_, Unknown) => true,
            (Unknown, _) => true,
            (Never, _) => true,
            (Unit, Unit) => true,
            (U8, U8) => true,
            (U16, U16) => true,
            (U32, U32) => true,
            (U64, U64) => true,
            (I8, I8) => true,
            (I16, I16) => true,
            (I32, I32) => true,
            (I64, I64) => true,
            (F32, F32) => true,
            (F64, F64) => true,
            (Bool, Bool) => true,
            (Data(id_left), Data(id_right))
                => id_left == id_right,
            (Alias(id_left), Alias(id_right))
                => id_left == id_right,
            (Tuple(head_left, tail_left), Tuple(head_right, tail_right))
                => self.type_check(head_left, head_right)
                && tail_left.len() == tail_right.len()
                && tail_left.iter().zip(tail_right.iter())
                    .all(|(left, right)| self.type_check(left, right)),
            (Array(ty_left, size_left), Array(ty_right, size_right))
                => self.type_check(ty_left, ty_right)
                && size_left == size_right,
            (Pointer(ty_left), Pointer(ty_right))
                => self.type_check(ty_left, ty_right),
            (Ref(ty_left), Ref(ty_right))
                => self.type_check(ty_left, ty_right),
            (Func(args_left, ty_left), Func(args_right, ty_right))
                => self.type_check(ty_left, ty_right)
                && args_left.len() == args_right.len()
                && args_left.iter().zip(args_right.iter())
                    .all(|(left, right)| self.type_check(right, left)),
            _ => false,
        }
    }

    pub fn type_check_coerce(&self, expr: &mut abt::Expr, ty: &abt::Type) -> bool {
        let expr_ty = self.program.type_of(expr);

        if let (abt::Type::Ref(ref_inner), abt::Type::Pointer(pointer_ty)) = (&expr_ty, ty) {
            if let abt::Type::Array(inner_ty, _) = &**ref_inner {
                if self.type_check(inner_ty, pointer_ty) {
                    let prev_expr = std::mem::replace(expr, abt::Expr::Unknown);
                    *expr = abt::Expr::ToPointer(Box::new(prev_expr));
                    return true;
                }
            }
        }

        self.type_check(&expr_ty, ty)
    }
}
