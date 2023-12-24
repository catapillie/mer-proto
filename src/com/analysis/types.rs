use std::collections::HashSet;

use crate::com::{
    abt::{ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    syntax::types::{TypeAst, TypeAstKind},
};

use super::Analyser;

#[derive(Debug)]
pub enum TypeConstraint {
    Exactly(u64),
    OneOf(HashSet<TypeAbt>),
    Unknown,
}

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
                    .annotate_primary(Note::Unknown, ty.span)
                    .done();
                self.diagnostics.push(d);

                TypeAbt::Unknown
            }
        }
    }

    pub fn type_of(&mut self, expr: &ExprAbt) -> TypeAbt {
        match expr {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Unit => TypeAbt::Unit,
            
            // generate a type variable, and store all its possible values
            #[rustfmt::skip]
            ExprAbt::Integer(values) => {
                let id = self.make_unique_id();

                let func_id = self.scope.current_func_id;
                let info = self.functions.get_mut(&func_id).unwrap();

                let mut int_types = HashSet::new();
                if values.u8.is_some() { int_types.insert(TypeAbt::U8); }
                if values.u16.is_some() { int_types.insert(TypeAbt::U16); }
                if values.u32.is_some() { int_types.insert(TypeAbt::U32); }
                if values.u64.is_some() { int_types.insert(TypeAbt::U64); }
                if values.i8.is_some() { int_types.insert(TypeAbt::I8); }
                if values.i16.is_some() { int_types.insert(TypeAbt::I16); }
                if values.i32.is_some() { int_types.insert(TypeAbt::I32); }
                if values.i64.is_some() { int_types.insert(TypeAbt::I64); }

                info.type_variables.insert(id, TypeConstraint::OneOf(int_types));
                
                TypeAbt::Var(id)
            }
            // same for floating-point numbers
            ExprAbt::Decimal(values) => {
                let id = self.make_unique_id();

                let func_id = self.scope.current_func_id;
                let info = self.functions.get_mut(&func_id).unwrap();

                let mut float_types = HashSet::new();
                if values.f32.is_some() { float_types.insert(TypeAbt::F32); }
                if values.f64.is_some() { float_types.insert(TypeAbt::F64); }

                info.type_variables.insert(id, TypeConstraint::OneOf(float_types));
                
                TypeAbt::Var(id)
            },

            ExprAbt::Boolean(_) => TypeAbt::Bool,

            ExprAbt::Variable(var_id) => self.variables.get(var_id).unwrap().ty,
            ExprAbt::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty,
            ExprAbt::Assignment(id, _) => self.type_of(&ExprAbt::Variable(*id)),

            ExprAbt::Binary(op, _, _) => op.out_ty,
            ExprAbt::Unary(op, _) => op.ty,
            ExprAbt::Debug(_, ty) => *ty,
        }
    }

    pub fn check_type(&mut self, left: TypeAbt, right: TypeAbt) -> bool {
        if !left.is_known() || !right.is_known() {
            return true; // just accept the check assuming an error was produced
        }

        let func_id = self.scope.current_func_id;
        let info = self.functions.get_mut(&func_id).unwrap();
        
        use TypeAbt as Ty;
        match (left, right) {
            (Ty::Var(x), Ty::Var(y)) => {
                // intersection types of x and y
                let mut xs = info.type_variables.get(&x).unwrap();
                while let TypeConstraint::Exactly(xx) = xs {
                    xs = info.type_variables.get(xx).unwrap();
                }
                let mut ys = info.type_variables.get(&y).unwrap();
                while let TypeConstraint::Exactly(yy) = ys {
                    ys = info.type_variables.get(yy).unwrap();
                }
                
                if matches!(xs, TypeConstraint::Unknown) || matches!(ys, TypeConstraint::Unknown) {
                    return true; // don't care
                }

                let TypeConstraint::OneOf(xs) = xs else {
                    unreachable!()
                };
                let TypeConstraint::OneOf(ys) = ys else {
                    unreachable!()
                };

                let inter: HashSet<_> = xs.intersection(ys).copied().collect();

                if inter.is_empty() {
                    info.type_variables.insert(x, TypeConstraint::Unknown);
                    info.type_variables.insert(y, TypeConstraint::Unknown);
                    panic!("type mismatch: intersection of types failed");
                }

                let cx = TypeConstraint::OneOf(inter.clone());
                let cy = if x == y {
                    TypeConstraint::OneOf(inter)
                } else {
                    TypeConstraint::Exactly(x)
                };

                info.type_variables.insert(x, cx);
                info.type_variables.insert(y, cy);

                true
            }
            (Ty::Var(x), target) | (target, Ty::Var(x)) => {
                // constrain type x to be target type
                let mut xs = info.type_variables.get(&x).unwrap();
                while let TypeConstraint::Exactly(xx) = xs {
                    println!("{xx}");
                    xs = info.type_variables.get(xx).unwrap();
                }

                if matches!(xs, TypeConstraint::Unknown) {
                    return true; // don't care
                }

                let TypeConstraint::OneOf(xs) = xs else {
                    unreachable!();
                };

                if !xs.contains(&target) {
                    info.type_variables.insert(x, TypeConstraint::Unknown);
                    panic!("type mismatch: of ty_var x into {target}")
                }

                info.type_variables.insert(x, TypeConstraint::OneOf([target].into()));
                true
            }
            _ => left == right // simple type equality
        }
    }

    pub fn check_integer_type(&self, ty: TypeAbt) -> bool {
        match ty {
            TypeAbt::Unknown => true,
            TypeAbt::U8
                | TypeAbt::U16
                | TypeAbt::U32
                | TypeAbt::U64
                | TypeAbt::I8
                | TypeAbt::I16
                | TypeAbt::I32
                | TypeAbt::I64 => true,
            TypeAbt::Var(x) => {
                let func_id = self.scope.current_func_id;
                let func_info = self.functions.get(&func_id).unwrap();
                match func_info.type_variables.get(&x).unwrap() {
                    TypeConstraint::Exactly(y) => self.check_integer_type(TypeAbt::Var(*y)),
                    TypeConstraint::OneOf(xs) => xs.iter().all(|ty| self.check_integer_type(*ty)),
                    TypeConstraint::Unknown => true,
                }
            },
            _ => false,
        }
    }

    pub fn check_float_type(&self, ty: TypeAbt) -> bool {
        match ty {
            TypeAbt::Unknown => true,
            TypeAbt::F32 | TypeAbt::F64 => true,
            TypeAbt::Var(x) => {
                let func_id = self.scope.current_func_id;
                let func_info = self.functions.get(&func_id).unwrap();
                match func_info.type_variables.get(&x).unwrap() {
                    TypeConstraint::Exactly(y) => self.check_float_type(TypeAbt::Var(*y)),
                    TypeConstraint::OneOf(xs) => xs.iter().all(|ty| self.check_float_type(*ty)),
                    TypeConstraint::Unknown => true,
                }
            },
            _ => false,
        }
    }

    pub fn resolve_type_variables(&mut self) {
        let func_id = self.scope.current_func_id;
        let func_info = self.functions.get_mut(&func_id).unwrap();
        for (id, mut c) in &func_info.type_variables {
            while let TypeConstraint::Exactly(x) = c {
                c = func_info.type_variables.get(x).unwrap();
            }

            let ty = match c {
                TypeConstraint::OneOf(types) => {
                    let ty = *types.iter().next().unwrap();
                    if types.len() == 1 {
                        ty
                    } else {
                        panic!("{{{id}}} needs an annotation");
                    }
                },
                TypeConstraint::Exactly(_) => unreachable!(),
                TypeConstraint::Unknown => TypeAbt::Unknown,
            };

            self.type_variables.insert(*id, ty);
        }
    }
}
