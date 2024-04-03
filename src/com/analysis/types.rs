use super::Analyser;
use crate::{
    com::{abt, analysis::tc::Tc, ast},
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

    pub fn type_check<'a>(&'a self, left: &'a abt::Type, right: &'a abt::Type) -> Tc {
        use abt::Type::*;
        match (
            self.program.dealias_type(left),
            self.program.dealias_type(right),
        ) {
            (_, Unknown) => Tc::Ok,
            (Unknown, _) => Tc::Ok,
            (Never, _) => Tc::Ok,
            (Unit, Unit) => Tc::Ok,
            (U8, U8) => Tc::Ok,
            (U16, U16) => Tc::Ok,
            (U32, U32) => Tc::Ok,
            (U64, U64) => Tc::Ok,
            (I8, I8) => Tc::Ok,
            (I16, I16) => Tc::Ok,
            (I32, I32) => Tc::Ok,
            (I64, I64) => Tc::Ok,
            (F32, F32) => Tc::Ok,
            (F64, F64) => Tc::Ok,
            (Bool, Bool) => Tc::Ok,
            (Data(id_left), Data(id_right)) => match id_left == id_right {
                true => Tc::Ok,
                false => Tc::Mismatch {
                    left: left.clone(),
                    right: left.clone(),
                },
            },
            (Alias(id_left), Alias(id_right)) => match id_left == id_right {
                true => Tc::Ok,
                false => Tc::Mismatch {
                    left: left.clone(),
                    right: left.clone(),
                },
            },
            (Tuple(head_left, tail_left), Tuple(head_right, tail_right)) => {
                let size_check = match tail_left.len() == tail_right.len() {
                    true => Tc::Ok,
                    false => Tc::TupleSizeMismatch {
                        left: tail_left.len() + 1,
                        right: tail_right.len() + 1,
                    },
                };

                let head_check = self.type_check(head_left, head_right);
                let tail_check = tail_left
                    .iter()
                    .zip(tail_right.iter())
                    .map(|(left, right)| self.type_check(left, right))
                    .collect::<Vec<_>>();
                let tc = [size_check, head_check]
                    .into_iter()
                    .chain(tail_check)
                    .collect();
                println!("{tc:#?}");
                Tc::Seq(tc)
            }
            (Array(ty_left, size_left), Array(ty_right, size_right)) => {
                let size_check = match size_left == size_right {
                    true => Tc::Ok,
                    false => Tc::ArraySizeMismatch {
                        left: *size_left,
                        right: *size_right,
                    },
                };
                let ty_check = self.type_check(ty_left, ty_right);
                Tc::Seq(Box::new([size_check, ty_check]))
            }
            (Pointer(ty_left), Pointer(ty_right)) => self.type_check(ty_left, ty_right),
            (Ref(ty_left), Ref(ty_right)) => self.type_check(ty_left, ty_right),
            (Func(args_left, ty_left), Func(args_right, ty_right)) => {
                let arg_count_check = match args_left.len() == args_right.len() {
                    true => Tc::Ok,
                    false => Tc::ArgCountMismatch {
                        left: args_left.len(),
                        right: args_right.len(),
                    },
                };

                let ty_check = self.type_check(ty_left, ty_right);
                let args_check = args_left
                    .iter()
                    .zip(args_right.iter())
                    .map(|(left, right)| self.type_check(right, left))
                    .collect::<Vec<_>>();
                let tc = [arg_count_check, ty_check]
                    .into_iter()
                    .chain(args_check)
                    .collect();

                Tc::Seq(tc)
            }
            _ => Tc::Mismatch {
                left: left.clone(),
                right: left.clone(),
            },
        }
    }

    pub fn type_check_coerce<'a>(&'a self, expr: &mut abt::Expr, mut ty: &'a abt::Type) -> Tc {
        ty = self.program.dealias_type(ty);
        let expr_ty = expr.value.ty.clone();
        let span = expr.span;

        if let (abt::Type::Ref(ref_inner), abt::Type::Pointer(pointer_ty)) = (&expr_ty, ty) {
            if let abt::Type::Array(inner_ty, _) = &**ref_inner {
                match self.type_check(inner_ty, pointer_ty) {
                    Tc::Ok => {
                        let prev_expr =
                            std::mem::replace(&mut expr.value, abt::TypedExpr::unknown());
                        *expr = abt::TypedExpr {
                            kind: abt::ExprKind::ToPointer(Box::new(prev_expr.wrap(span))),
                            ty: abt::Type::Pointer(pointer_ty.clone()),
                        }
                        .wrap(span);
                        return Tc::Ok;
                    }
                    tc => return tc,
                }
            }
        }

        self.type_check(&expr_ty, ty)
    }
}
