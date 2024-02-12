use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

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
            ast::TypeKind::Ref(ty) => abt::Type::Ref(Box::new(self.analyse_type(ty))),
            ast::TypeKind::Declared(id) => {
                match id.as_str() {
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

                if let Some(info) = self.get_data_structure(id) {
                    return abt::Type::Data(info.id);
                }

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownType(id.clone()))
                    .with_severity(Severity::Error)
                    .with_span(ty.span)
                    .annotate_primary(Note::Unknown, ty.span)
                    .done();
                self.diagnostics.push(d);
                abt::Type::Unknown
            }
            ast::TypeKind::Func(arg_tys, ret_ty) => {
                let bound_arg_tys = arg_tys.iter().map(|ty| self.analyse_type(ty)).collect();
                let bound_ret_ty = self.analyse_type(ret_ty);
                abt::Type::Func(bound_arg_tys, Box::new(bound_ret_ty))
            }
        }
    }
}
