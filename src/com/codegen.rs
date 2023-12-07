use std::{
    collections::HashMap,
    io::{self, Cursor},
};

use byteorder::WriteBytesExt;
use byteorder::LE;

use crate::{
    com::abt::{BinOpAbtKind, StmtAbtKind, TypeAbt, UnOpAbtKind},
    run::opcode::{self, Opcode},
};

use super::abt::{ExprAbt, Function, ProgramAbt, StmtAbt};

pub struct Codegen {
    cursor: Cursor<Vec<u8>>,
    function_positions: HashMap<u32, u32>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            cursor: Cursor::new(Vec::new()),
            function_positions: HashMap::new(),
        }
    }

    fn position(&self) -> u32 {
        self.cursor.position().try_into().unwrap()
    }

    fn gen_u32_placeholder(&mut self) -> io::Result<u32> {
        let position = self.position();
        self.cursor.write_u32::<LE>(0)?;
        Ok(position)
    }

    fn patch_u32_placeholder(&mut self, placeholder: u32, value: u32) -> io::Result<()> {
        let back = self.cursor.position();
        self.cursor.set_position(placeholder as u64);
        self.cursor.write_u32::<LE>(value)?;
        self.cursor.set_position(back);
        Ok(())
    }

    pub fn gen(mut self, abt: &ProgramAbt) -> io::Result<Vec<u8>> {
        self.cursor.write_u8(opcode::entry_point)?;
        let entry_point_cursor = self.gen_u32_placeholder()?;

        for (id, (name, func)) in abt.functions_by_id.iter() {
            let position = self.position();
            self.function_positions.insert(*id, position);
            self.gen_function(name.clone(), func)?;
        }

        let last_function_id = abt.functions_by_id.last_key_value().unwrap().0;
        self.patch_u32_placeholder(
            entry_point_cursor,
            *self.function_positions.get(last_function_id).unwrap(),
        )?;

        Ok(self.cursor.into_inner())
    }

    fn gen_function(&mut self, name: String, func: &Function) -> io::Result<()> {
        let param_count = func.param_types.len() as u8;
        let local_count = func.local_count;
        let opcode = Opcode::function(name, param_count, local_count);
        opcode.write_bytes(&mut self.cursor)?;

        self.gen_statement(&func.code)?;
        Ok(())
    }

    fn gen_statement(&mut self, stmt: &StmtAbt) -> io::Result<()> {
        use StmtAbtKind as S;
        match &stmt.kind {
            S::Empty => {}
            S::Block(stmts) => self.gen_block(stmts)?,
            S::Expr(expr) => {
                self.gen_expression(expr)?;
                Opcode::pop.write_bytes(&mut self.cursor)?;
            }
            S::IfThen(guard, body) => {
                // if ...
                self.gen_expression(guard)?;
                self.cursor.write_u8(opcode::not_bool)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_from = self.gen_u32_placeholder()?;

                // then ...
                self.gen_statement(body.as_ref())?;

                // write saved jump address
                let cursor_to = self.position();
                self.patch_u32_placeholder(cursor_from, cursor_to)?;
            }
            S::IfThenElse(guard, body_then, body_else) => {
                // if ... then
                self.gen_expression(guard)?;
                self.cursor.write_u8(opcode::not_bool)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_guard_end = self.gen_u32_placeholder()?;

                // then ...
                self.gen_statement(body_then)?;
                self.cursor.write_u8(opcode::jmp)?;
                let cursor_body_then_end = self.gen_u32_placeholder()?;

                // else ...
                let cursor_body_else_start = self.position();
                self.gen_statement(body_else)?;
                let cursor_body_else_end = self.position();

                // write saved jump adresses
                self.patch_u32_placeholder(cursor_body_then_end, cursor_body_else_end)?;
                self.patch_u32_placeholder(cursor_guard_end, cursor_body_else_start)?;
            }
            S::WhileDo(guard, body) => {
                // while ...
                let cursor_guard_start = self.position();
                self.gen_expression(guard)?;
                self.cursor.write_u8(opcode::not_bool)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_guard_end = self.gen_u32_placeholder()?;

                // do ...
                self.gen_statement(body)?;
                self.cursor.write_u8(opcode::jmp)?;
                self.cursor.write_u32::<LE>(cursor_guard_start)?;

                // write saved jump address
                let cursor_body_end = self.position();
                self.patch_u32_placeholder(cursor_guard_end, cursor_body_end)?;
            }
            S::DoWhile(body, guard) => {
                // do ...
                let cursor_stmt_start = self.position();
                self.gen_statement(body)?;

                // while ...
                self.gen_expression(guard)?;
                Opcode::jmp_if(cursor_stmt_start).write_bytes(&mut self.cursor)?;
            }
            S::Return(expr) => self.gen_return(expr)?,
        }

        Ok(())
    }

    fn gen_block(&mut self, stmts: &Vec<StmtAbt>) -> io::Result<()> {
        for stmt in stmts {
            self.gen_statement(stmt)?;
        }

        Ok(())
    }

    fn gen_return(&mut self, expr: &ExprAbt) -> io::Result<()> {
        self.gen_expression(expr)?;
        Opcode::ret.write_bytes(&mut self.cursor)?;
        Ok(())
    }

    fn gen_expression(&mut self, expr: &ExprAbt) -> io::Result<()> {
        use super::abt::ExprAbt as E;
        match expr {
            E::Unknown => unreachable!(),
            E::Unit => Opcode::ld_unit.write_bytes(&mut self.cursor),
            E::Number(num) => Opcode::ld_f64(*num).write_bytes(&mut self.cursor),
            E::Boolean(b) => {
                let opcode = match b {
                    true => Opcode::ld_bool_true,
                    false => Opcode::ld_bool_false,
                };
                opcode.write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Variable(var) => Opcode::ld_loc(var.id).write_bytes(&mut self.cursor),
            E::Assignment(var, expr) => {
                self.gen_expression(expr)?;
                Opcode::st_loc(var.id).write_bytes(&mut self.cursor)?;
                Opcode::ld_loc(var.id).write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Call(id, params, _) => {
                for param in params {
                    self.gen_expression(param)?;
                }

                let to = self.function_positions.get(id).cloned().unwrap();
                Opcode::call(to).write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Binary(op, left, right) => {
                use BinOpAbtKind as K;
                use TypeAbt as Ty;

                // short-circuiting logic
                match &op.kind {
                    K::And => return self.gen_short_circuit_and(left, right),
                    K::Or => return self.gen_short_circuit_or(left, right),
                    _ => {}
                }

                self.gen_expression(left)?;
                self.gen_expression(right)?;

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
                    (Ty::Bool, K::Xor) => Opcode::xor_bool,
                    _ => unreachable!("{:?}", op),
                };
                opcode.write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Unary(op, expr) => {
                self.gen_expression(expr)?;

                use TypeAbt as Ty;
                use UnOpAbtKind as K;
                let opcode = match (&op.ty, &op.kind) {
                    (Ty::U8, K::Pos) => return Ok(()),
                    (Ty::U16, K::Pos) => return Ok(()),
                    (Ty::U32, K::Pos) => return Ok(()),
                    (Ty::U64, K::Pos) => return Ok(()),
                    (Ty::I8, K::Pos) => return Ok(()),
                    (Ty::I8, K::Neg) => Opcode::neg_i8,
                    (Ty::I16, K::Pos) => return Ok(()),
                    (Ty::I16, K::Neg) => Opcode::neg_i16,
                    (Ty::I32, K::Pos) => return Ok(()),
                    (Ty::I32, K::Neg) => Opcode::neg_i32,
                    (Ty::I64, K::Pos) => return Ok(()),
                    (Ty::I64, K::Neg) => Opcode::neg_i64,
                    (Ty::F32, K::Pos) => return Ok(()),
                    (Ty::F32, K::Neg) => Opcode::neg_f32,
                    (Ty::F64, K::Pos) => return Ok(()),
                    (Ty::F64, K::Neg) => Opcode::neg_f64,
                    (Ty::Bool, K::Pos) => return Ok(()),
                    (Ty::Bool, K::Not) => Opcode::not_bool,
                    _ => unreachable!(),
                };

                opcode.write_bytes(&mut self.cursor)?;
                Ok(())
            }
        }
    }

    fn gen_short_circuit_and(&mut self, left: &ExprAbt, right: &ExprAbt) -> io::Result<()> {
        self.gen_expression(left)?;
        self.cursor.write_u8(opcode::dup)?;
        self.cursor.write_u8(opcode::not_bool)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right)?;
        self.cursor.write_u8(opcode::and_bool)?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }

    fn gen_short_circuit_or(&mut self, left: &ExprAbt, right: &ExprAbt) -> Result<(), io::Error> {
        self.gen_expression(left)?;
        self.cursor.write_u8(opcode::dup)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right)?;
        self.cursor.write_u8(opcode::or_bool)?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }
}
