use std::io::Cursor;

use crate::{
    com::abt::{BinOpAbtKind, TypeAbt, UnOpAbtKind},
    run::opcode::Opcode,
};

use super::abt::{ExprAbt, Function, ProgramAbt, StmtAbt};

pub struct Codegen {
    code: Vec<Opcode>,
}

impl Codegen {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn gen(&mut self, abt: &ProgramAbt) -> Vec<u8> {
        for (_id, (name, func)) in abt.functions_by_id.iter() {
            self.gen_function(name.clone(), func);
        }

        let program = Vec::new();
        let mut cursor = Cursor::new(program);
        for opcode in &self.code {
            opcode.write_bytes(&mut cursor).unwrap();
        }

        cursor.into_inner()
    }

    fn gen_function(&mut self, name: String, func: &Function) {
        self.code
            .push(Opcode::function(name, func.param_types.len() as u8, 0));
        self.gen_statement(&func.code);
    }

    fn gen_statement(&mut self, stmt: &StmtAbt) {
        use super::abt::StmtAbtKind as S;
        match &stmt.kind {
            S::Empty => {}
            S::Block(stmts) => self.gen_block(stmts),
            S::Expr(_) => todo!(),
            S::IfThen(_, _) => todo!(),
            S::IfThenElse(_, _, _) => todo!(),
            S::WhileDo(_, _) => todo!(),
            S::DoWhile(_, _) => todo!(),
            S::Return(expr) => self.gen_return(expr),
        }
    }

    fn gen_block(&mut self, stmts: &Vec<StmtAbt>) {
        for stmt in stmts {
            self.gen_statement(stmt);
        }
    }

    fn gen_return(&mut self, expr: &ExprAbt) {
        self.gen_expression(expr);
        self.code.push(Opcode::ret);
    }

    fn gen_expression(&mut self, expr: &ExprAbt) {
        use super::abt::ExprAbt as E;
        match expr {
            E::Unknown => unreachable!(),
            E::Unit => self.code.push(Opcode::ld_unit),
            E::Number(num) => self.code.push(Opcode::ld_f64(*num)),
            E::Boolean(b) => self.code.push(match b {
                true => Opcode::ld_bool_true,
                false => Opcode::ld_bool_false,
            }),
            E::Variable(var) => self.code.push(Opcode::ld_loc(var.id)),
            E::Assignment(var, expr) => {
                self.gen_expression(expr);
                self.code.push(Opcode::st_loc(var.id))
            }
            E::Call(_, _, _) => todo!(),
            E::Binary(op, left, right) => {
                self.gen_expression(left);
                self.gen_expression(right);

                use BinOpAbtKind as K;
                use TypeAbt as Ty;
                let opcode = match (&op.in_ty, &op.kind) {
                    (Ty::U8, K::Add) => Opcode::add_u8,
                    (Ty::U8, K::Sub) => Opcode::sub_u8,
                    (Ty::U8, K::Mul) => Opcode::mul_u8,
                    (Ty::U8, K::Div) => Opcode::div_u8,
                    (Ty::U8, K::Rem) => Opcode::rem_u8,
                    (Ty::U8, K::Eq) => Opcode::eq_u8,
                    (Ty::U8, K::Ne) => Opcode::ne_u8,
                    (Ty::U8, K::Le) => Opcode::le_u8,
                    (Ty::U8, K::Lt) => Opcode::lt_u8,
                    (Ty::U8, K::Ge) => Opcode::ge_u8,
                    (Ty::U8, K::Gt) => Opcode::gt_u8,
                    (Ty::U8, K::BitAnd) => Opcode::bitand_u8,
                    (Ty::U8, K::BitXor) => Opcode::bitxor_u8,
                    (Ty::U8, K::BitOr) => Opcode::bitor_u8,
                    (Ty::U16, K::Add) => Opcode::add_u16,
                    (Ty::U16, K::Sub) => Opcode::sub_u16,
                    (Ty::U16, K::Mul) => Opcode::mul_u16,
                    (Ty::U16, K::Div) => Opcode::div_u16,
                    (Ty::U16, K::Rem) => Opcode::rem_u16,
                    (Ty::U16, K::Eq) => Opcode::eq_u16,
                    (Ty::U16, K::Ne) => Opcode::ne_u16,
                    (Ty::U16, K::Le) => Opcode::le_u16,
                    (Ty::U16, K::Lt) => Opcode::lt_u16,
                    (Ty::U16, K::Ge) => Opcode::ge_u16,
                    (Ty::U16, K::Gt) => Opcode::gt_u16,
                    (Ty::U16, K::BitAnd) => Opcode::bitand_u16,
                    (Ty::U16, K::BitXor) => Opcode::bitxor_u16,
                    (Ty::U16, K::BitOr) => Opcode::bitor_u16,
                    (Ty::U32, K::Add) => Opcode::add_u32,
                    (Ty::U32, K::Sub) => Opcode::sub_u32,
                    (Ty::U32, K::Mul) => Opcode::mul_u32,
                    (Ty::U32, K::Div) => Opcode::div_u32,
                    (Ty::U32, K::Rem) => Opcode::rem_u32,
                    (Ty::U32, K::Eq) => Opcode::eq_u32,
                    (Ty::U32, K::Ne) => Opcode::ne_u32,
                    (Ty::U32, K::Le) => Opcode::le_u32,
                    (Ty::U32, K::Lt) => Opcode::lt_u32,
                    (Ty::U32, K::Ge) => Opcode::ge_u32,
                    (Ty::U32, K::Gt) => Opcode::gt_u32,
                    (Ty::U32, K::BitAnd) => Opcode::bitand_u32,
                    (Ty::U32, K::BitXor) => Opcode::bitxor_u32,
                    (Ty::U32, K::BitOr) => Opcode::bitor_u32,
                    (Ty::U64, K::Add) => Opcode::add_u64,
                    (Ty::U64, K::Sub) => Opcode::sub_u64,
                    (Ty::U64, K::Mul) => Opcode::mul_u64,
                    (Ty::U64, K::Div) => Opcode::div_u64,
                    (Ty::U64, K::Rem) => Opcode::rem_u64,
                    (Ty::U64, K::Eq) => Opcode::eq_u64,
                    (Ty::U64, K::Ne) => Opcode::ne_u64,
                    (Ty::U64, K::Le) => Opcode::le_u64,
                    (Ty::U64, K::Lt) => Opcode::lt_u64,
                    (Ty::U64, K::Ge) => Opcode::ge_u64,
                    (Ty::U64, K::Gt) => Opcode::gt_u64,
                    (Ty::U64, K::BitAnd) => Opcode::bitand_u64,
                    (Ty::U64, K::BitXor) => Opcode::bitxor_u64,
                    (Ty::U64, K::BitOr) => Opcode::bitor_u64,
                    (Ty::I8, K::Add) => Opcode::add_i8,
                    (Ty::I8, K::Sub) => Opcode::sub_i8,
                    (Ty::I8, K::Mul) => Opcode::mul_i8,
                    (Ty::I8, K::Div) => Opcode::div_i8,
                    (Ty::I8, K::Rem) => Opcode::rem_i8,
                    (Ty::I8, K::Eq) => Opcode::eq_i8,
                    (Ty::I8, K::Ne) => Opcode::ne_i8,
                    (Ty::I8, K::Le) => Opcode::le_i8,
                    (Ty::I8, K::Lt) => Opcode::lt_i8,
                    (Ty::I8, K::Ge) => Opcode::ge_i8,
                    (Ty::I8, K::Gt) => Opcode::gt_i8,
                    (Ty::I8, K::BitAnd) => Opcode::bitand_i8,
                    (Ty::I8, K::BitXor) => Opcode::bitxor_i8,
                    (Ty::I8, K::BitOr) => Opcode::bitor_i8,
                    (Ty::I16, K::Add) => Opcode::add_i16,
                    (Ty::I16, K::Sub) => Opcode::sub_i16,
                    (Ty::I16, K::Mul) => Opcode::mul_i16,
                    (Ty::I16, K::Div) => Opcode::div_i16,
                    (Ty::I16, K::Rem) => Opcode::rem_i16,
                    (Ty::I16, K::Eq) => Opcode::eq_i16,
                    (Ty::I16, K::Ne) => Opcode::ne_i16,
                    (Ty::I16, K::Le) => Opcode::le_i16,
                    (Ty::I16, K::Lt) => Opcode::lt_i16,
                    (Ty::I16, K::Ge) => Opcode::ge_i16,
                    (Ty::I16, K::Gt) => Opcode::gt_i16,
                    (Ty::I16, K::BitAnd) => Opcode::bitand_i16,
                    (Ty::I16, K::BitXor) => Opcode::bitxor_i16,
                    (Ty::I16, K::BitOr) => Opcode::bitor_i16,
                    (Ty::I32, K::Add) => Opcode::add_i32,
                    (Ty::I32, K::Sub) => Opcode::sub_i32,
                    (Ty::I32, K::Mul) => Opcode::mul_i32,
                    (Ty::I32, K::Div) => Opcode::div_i32,
                    (Ty::I32, K::Rem) => Opcode::rem_i32,
                    (Ty::I32, K::Eq) => Opcode::eq_i32,
                    (Ty::I32, K::Ne) => Opcode::ne_i32,
                    (Ty::I32, K::Le) => Opcode::le_i32,
                    (Ty::I32, K::Lt) => Opcode::lt_i32,
                    (Ty::I32, K::Ge) => Opcode::ge_i32,
                    (Ty::I32, K::Gt) => Opcode::gt_i32,
                    (Ty::I32, K::BitAnd) => Opcode::bitand_i32,
                    (Ty::I32, K::BitXor) => Opcode::bitxor_i32,
                    (Ty::I32, K::BitOr) => Opcode::bitor_i32,
                    (Ty::I64, K::Add) => Opcode::add_i64,
                    (Ty::I64, K::Sub) => Opcode::sub_i64,
                    (Ty::I64, K::Mul) => Opcode::mul_i64,
                    (Ty::I64, K::Div) => Opcode::div_i64,
                    (Ty::I64, K::Rem) => Opcode::rem_i64,
                    (Ty::I64, K::Eq) => Opcode::eq_i64,
                    (Ty::I64, K::Ne) => Opcode::ne_i64,
                    (Ty::I64, K::Le) => Opcode::le_i64,
                    (Ty::I64, K::Lt) => Opcode::lt_i64,
                    (Ty::I64, K::Ge) => Opcode::ge_i64,
                    (Ty::I64, K::Gt) => Opcode::gt_i64,
                    (Ty::I64, K::BitAnd) => Opcode::bitand_i64,
                    (Ty::I64, K::BitXor) => Opcode::bitxor_i64,
                    (Ty::I64, K::BitOr) => Opcode::bitor_i64,
                    (Ty::F32, K::Add) => Opcode::add_f32,
                    (Ty::F32, K::Sub) => Opcode::sub_f32,
                    (Ty::F32, K::Mul) => Opcode::mul_f32,
                    (Ty::F32, K::Div) => Opcode::div_f32,
                    (Ty::F32, K::Rem) => Opcode::rem_f32,
                    (Ty::F32, K::Eq) => Opcode::eq_f32,
                    (Ty::F32, K::Ne) => Opcode::ne_f32,
                    (Ty::F32, K::Le) => Opcode::le_f32,
                    (Ty::F32, K::Lt) => Opcode::lt_f32,
                    (Ty::F32, K::Ge) => Opcode::ge_f32,
                    (Ty::F32, K::Gt) => Opcode::gt_f32,
                    (Ty::F64, K::Add) => Opcode::add_f64,
                    (Ty::F64, K::Sub) => Opcode::sub_f64,
                    (Ty::F64, K::Mul) => Opcode::mul_f64,
                    (Ty::F64, K::Div) => Opcode::div_f64,
                    (Ty::F64, K::Rem) => Opcode::rem_f64,
                    (Ty::F64, K::Eq) => Opcode::eq_f64,
                    (Ty::F64, K::Ne) => Opcode::ne_f64,
                    (Ty::F64, K::Le) => Opcode::le_f64,
                    (Ty::F64, K::Lt) => Opcode::lt_f64,
                    (Ty::F64, K::Ge) => Opcode::ge_f64,
                    (Ty::F64, K::Gt) => Opcode::gt_f64,
                    (Ty::Bool, K::Eq) => Opcode::eq_bool,
                    (Ty::Bool, K::Ne) => Opcode::ne_bool,
                    (Ty::Bool, K::BitAnd) => Opcode::and_bool,
                    (Ty::Bool, K::BitOr) => Opcode::or_bool,
                    (Ty::Bool, K::BitXor) => Opcode::xor_bool,
                    (Ty::Bool, K::And) => todo!(),
                    (Ty::Bool, K::Or) => todo!(),
                    (Ty::Bool, K::Xor) => todo!(),
                    _ => unreachable!("{:?}", op),
                };
                self.code.push(opcode);
            }
            E::Unary(op, expr) => {
                self.gen_expression(expr);

                use TypeAbt as Ty;
                use UnOpAbtKind as K;
                let opcode = match (&op.ty, &op.kind) {
                    (Ty::U8, K::Pos) => return,
                    (Ty::U16, K::Pos) => return,
                    (Ty::U32, K::Pos) => return,
                    (Ty::U64, K::Pos) => return,
                    (Ty::I8, K::Pos) => return,
                    (Ty::I8, K::Neg) => Opcode::neg_i8,
                    (Ty::I16, K::Pos) => return,
                    (Ty::I16, K::Neg) => Opcode::neg_i16,
                    (Ty::I32, K::Pos) => return,
                    (Ty::I32, K::Neg) => Opcode::neg_i32,
                    (Ty::I64, K::Pos) => return,
                    (Ty::I64, K::Neg) => Opcode::neg_i64,
                    (Ty::F32, K::Pos) => return,
                    (Ty::F32, K::Neg) => Opcode::neg_f32,
                    (Ty::F64, K::Pos) => return,
                    (Ty::F64, K::Neg) => Opcode::neg_f64,
                    (Ty::Bool, K::Pos) => return,
                    (Ty::Bool, K::Not) => Opcode::not_bool,
                    _ => unreachable!(),
                };

                self.code.push(opcode);
            }
        }
    }
}
