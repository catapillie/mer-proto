use std::{
    collections::HashMap,
    io::{self, Cursor},
};

use byteorder::WriteBytesExt;
use byteorder::LE;

use crate::{
    com::abt::{BinOpAbtKind, StmtAbtKind, TypeAbt, UnOpAbtKind},
    run::{
        native_type::NativeType,
        opcode::{self, Opcode},
    },
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
                Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
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
                Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
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
                Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
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
            E::Unit => Opcode::ld_u8(0).write_bytes(&mut self.cursor),
            E::Integer(num) => Opcode::ld_i64(*num).write_bytes(&mut self.cursor),
            E::Decimal(num) => Opcode::ld_f64(*num).write_bytes(&mut self.cursor),
            E::Boolean(b) => {
                let opcode = match b {
                    true => Opcode::ld_u8(1),
                    false => Opcode::ld_u8(0),
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
                use NativeType as Nt;
                use Opcode as O;
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
                    (Ty::U8, K::Add) => O::add(Nt::u8),
                    (Ty::U8, K::Sub) => O::sub(Nt::u8),
                    (Ty::U8, K::Mul) => O::mul(Nt::u8),
                    (Ty::U8, K::Div) => O::div(Nt::u8),
                    (Ty::U8, K::Rem) => O::rem(Nt::u8),
                    (Ty::U8, K::Eq) => O::eq(Nt::u8),
                    (Ty::U8, K::Ne) => O::ne(Nt::u8),
                    (Ty::U8, K::Le) => O::le(Nt::u8),
                    (Ty::U8, K::Lt) => O::lt(Nt::u8),
                    (Ty::U8, K::Ge) => O::ge(Nt::u8),
                    (Ty::U8, K::Gt) => O::gt(Nt::u8),
                    (Ty::U8, K::BitAnd) => O::bitand(Nt::u8),
                    (Ty::U8, K::BitXor) => O::bitxor(Nt::u8),
                    (Ty::U8, K::BitOr) => O::bitor(Nt::u8),
                    (Ty::U16, K::Add) => O::add(Nt::u16),
                    (Ty::U16, K::Sub) => O::sub(Nt::u16),
                    (Ty::U16, K::Mul) => O::mul(Nt::u16),
                    (Ty::U16, K::Div) => O::div(Nt::u16),
                    (Ty::U16, K::Rem) => O::rem(Nt::u16),
                    (Ty::U16, K::Eq) => O::eq(Nt::u16),
                    (Ty::U16, K::Ne) => O::ne(Nt::u16),
                    (Ty::U16, K::Le) => O::le(Nt::u16),
                    (Ty::U16, K::Lt) => O::lt(Nt::u16),
                    (Ty::U16, K::Ge) => O::ge(Nt::u16),
                    (Ty::U16, K::Gt) => O::gt(Nt::u16),
                    (Ty::U16, K::BitAnd) => O::bitand(Nt::u16),
                    (Ty::U16, K::BitXor) => O::bitxor(Nt::u16),
                    (Ty::U16, K::BitOr) => O::bitor(Nt::u16),
                    (Ty::U32, K::Add) => O::add(Nt::u32),
                    (Ty::U32, K::Sub) => O::sub(Nt::u32),
                    (Ty::U32, K::Mul) => O::mul(Nt::u32),
                    (Ty::U32, K::Div) => O::div(Nt::u32),
                    (Ty::U32, K::Rem) => O::rem(Nt::u32),
                    (Ty::U32, K::Eq) => O::eq(Nt::u32),
                    (Ty::U32, K::Ne) => O::ne(Nt::u32),
                    (Ty::U32, K::Le) => O::le(Nt::u32),
                    (Ty::U32, K::Lt) => O::lt(Nt::u32),
                    (Ty::U32, K::Ge) => O::ge(Nt::u32),
                    (Ty::U32, K::Gt) => O::gt(Nt::u32),
                    (Ty::U32, K::BitAnd) => O::bitand(Nt::u32),
                    (Ty::U32, K::BitXor) => O::bitxor(Nt::u32),
                    (Ty::U32, K::BitOr) => O::bitor(Nt::u32),
                    (Ty::U64, K::Add) => O::add(Nt::u64),
                    (Ty::U64, K::Sub) => O::sub(Nt::u64),
                    (Ty::U64, K::Mul) => O::mul(Nt::u64),
                    (Ty::U64, K::Div) => O::div(Nt::u64),
                    (Ty::U64, K::Rem) => O::rem(Nt::u64),
                    (Ty::U64, K::Eq) => O::eq(Nt::u64),
                    (Ty::U64, K::Ne) => O::ne(Nt::u64),
                    (Ty::U64, K::Le) => O::le(Nt::u64),
                    (Ty::U64, K::Lt) => O::lt(Nt::u64),
                    (Ty::U64, K::Ge) => O::ge(Nt::u64),
                    (Ty::U64, K::Gt) => O::gt(Nt::u64),
                    (Ty::U64, K::BitAnd) => O::bitand(Nt::u64),
                    (Ty::U64, K::BitXor) => O::bitxor(Nt::u64),
                    (Ty::U64, K::BitOr) => O::bitor(Nt::u64),
                    (Ty::I8, K::Add) => O::add(Nt::i8),
                    (Ty::I8, K::Sub) => O::sub(Nt::i8),
                    (Ty::I8, K::Mul) => O::mul(Nt::i8),
                    (Ty::I8, K::Div) => O::div(Nt::i8),
                    (Ty::I8, K::Rem) => O::rem(Nt::i8),
                    (Ty::I8, K::Eq) => O::eq(Nt::i8),
                    (Ty::I8, K::Ne) => O::ne(Nt::i8),
                    (Ty::I8, K::Le) => O::le(Nt::i8),
                    (Ty::I8, K::Lt) => O::lt(Nt::i8),
                    (Ty::I8, K::Ge) => O::ge(Nt::i8),
                    (Ty::I8, K::Gt) => O::gt(Nt::i8),
                    (Ty::I8, K::BitAnd) => O::bitand(Nt::i8),
                    (Ty::I8, K::BitXor) => O::bitxor(Nt::i8),
                    (Ty::I8, K::BitOr) => O::bitor(Nt::i8),
                    (Ty::I16, K::Add) => O::add(Nt::i16),
                    (Ty::I16, K::Sub) => O::sub(Nt::i16),
                    (Ty::I16, K::Mul) => O::mul(Nt::i16),
                    (Ty::I16, K::Div) => O::div(Nt::i16),
                    (Ty::I16, K::Rem) => O::rem(Nt::i16),
                    (Ty::I16, K::Eq) => O::eq(Nt::i16),
                    (Ty::I16, K::Ne) => O::ne(Nt::i16),
                    (Ty::I16, K::Le) => O::le(Nt::i16),
                    (Ty::I16, K::Lt) => O::lt(Nt::i16),
                    (Ty::I16, K::Ge) => O::ge(Nt::i16),
                    (Ty::I16, K::Gt) => O::gt(Nt::i16),
                    (Ty::I16, K::BitAnd) => O::bitand(Nt::i16),
                    (Ty::I16, K::BitXor) => O::bitxor(Nt::i16),
                    (Ty::I16, K::BitOr) => O::bitor(Nt::i16),
                    (Ty::I32, K::Add) => O::add(Nt::i32),
                    (Ty::I32, K::Sub) => O::sub(Nt::i32),
                    (Ty::I32, K::Mul) => O::mul(Nt::i32),
                    (Ty::I32, K::Div) => O::div(Nt::i32),
                    (Ty::I32, K::Rem) => O::rem(Nt::i32),
                    (Ty::I32, K::Eq) => O::eq(Nt::i32),
                    (Ty::I32, K::Ne) => O::ne(Nt::i32),
                    (Ty::I32, K::Le) => O::le(Nt::i32),
                    (Ty::I32, K::Lt) => O::lt(Nt::i32),
                    (Ty::I32, K::Ge) => O::ge(Nt::i32),
                    (Ty::I32, K::Gt) => O::gt(Nt::i32),
                    (Ty::I32, K::BitAnd) => O::bitand(Nt::i32),
                    (Ty::I32, K::BitXor) => O::bitxor(Nt::i32),
                    (Ty::I32, K::BitOr) => O::bitor(Nt::i32),
                    (Ty::I64, K::Add) => O::add(Nt::i64),
                    (Ty::I64, K::Sub) => O::sub(Nt::i64),
                    (Ty::I64, K::Mul) => O::mul(Nt::i64),
                    (Ty::I64, K::Div) => O::div(Nt::i64),
                    (Ty::I64, K::Rem) => O::rem(Nt::i64),
                    (Ty::I64, K::Eq) => O::eq(Nt::i64),
                    (Ty::I64, K::Ne) => O::ne(Nt::i64),
                    (Ty::I64, K::Le) => O::le(Nt::i64),
                    (Ty::I64, K::Lt) => O::lt(Nt::i64),
                    (Ty::I64, K::Ge) => O::ge(Nt::i64),
                    (Ty::I64, K::Gt) => O::gt(Nt::i64),
                    (Ty::I64, K::BitAnd) => O::bitand(Nt::i64),
                    (Ty::I64, K::BitXor) => O::bitxor(Nt::i64),
                    (Ty::I64, K::BitOr) => O::bitor(Nt::i64),
                    (Ty::F32, K::Add) => O::add(Nt::f32),
                    (Ty::F32, K::Sub) => O::sub(Nt::f32),
                    (Ty::F32, K::Mul) => O::mul(Nt::f32),
                    (Ty::F32, K::Div) => O::div(Nt::f32),
                    (Ty::F32, K::Rem) => O::rem(Nt::f32),
                    (Ty::F32, K::Eq) => O::eq(Nt::f32),
                    (Ty::F32, K::Ne) => O::ne(Nt::f32),
                    (Ty::F32, K::Le) => O::le(Nt::f32),
                    (Ty::F32, K::Lt) => O::lt(Nt::f32),
                    (Ty::F32, K::Ge) => O::ge(Nt::f32),
                    (Ty::F32, K::Gt) => O::gt(Nt::f32),
                    (Ty::F64, K::Add) => O::add(Nt::f64),
                    (Ty::F64, K::Sub) => O::sub(Nt::f64),
                    (Ty::F64, K::Mul) => O::mul(Nt::f64),
                    (Ty::F64, K::Div) => O::div(Nt::f64),
                    (Ty::F64, K::Rem) => O::rem(Nt::f64),
                    (Ty::F64, K::Eq) => O::eq(Nt::f64),
                    (Ty::F64, K::Ne) => O::ne(Nt::f64),
                    (Ty::F64, K::Le) => O::le(Nt::f64),
                    (Ty::F64, K::Lt) => O::lt(Nt::f64),
                    (Ty::F64, K::Ge) => O::ge(Nt::f64),
                    (Ty::F64, K::Gt) => O::gt(Nt::f64),
                    (Ty::Bool, K::Eq) => O::eq(Nt::bool),
                    (Ty::Bool, K::Ne) => O::ne(Nt::bool),
                    (Ty::Bool, K::BitAnd) => O::bitand(Nt::bool),
                    (Ty::Bool, K::BitOr) => O::bitor(Nt::bool),
                    (Ty::Bool, K::BitXor) => O::bitxor(Nt::bool),
                    (Ty::Bool, K::Xor) => O::bitxor(Nt::bool),
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
                    (Ty::U8, K::Pos)
                    | (Ty::U16, K::Pos)
                    | (Ty::U32, K::Pos)
                    | (Ty::U64, K::Pos)
                    | (Ty::I8, K::Pos)
                    | (Ty::I16, K::Pos)
                    | (Ty::I32, K::Pos)
                    | (Ty::I64, K::Pos)
                    | (Ty::F32, K::Pos)
                    | (Ty::F64, K::Pos) => return Ok(()), // this operation does nothing
                    (Ty::I8, K::Neg) => Opcode::neg(NativeType::i8),
                    (Ty::I16, K::Neg) => Opcode::neg(NativeType::i16),
                    (Ty::I32, K::Neg) => Opcode::neg(NativeType::i32),
                    (Ty::I64, K::Neg) => Opcode::neg(NativeType::i64),
                    (Ty::F32, K::Neg) => Opcode::neg(NativeType::f32),
                    (Ty::F64, K::Neg) => Opcode::neg(NativeType::f64),
                    (Ty::Bool, K::Not) => Opcode::neg(NativeType::bool),
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
        Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right)?;
        Opcode::bitand(NativeType::bool).write_bytes(&mut self.cursor)?;
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
        Opcode::bitor(NativeType::bool).write_bytes(&mut self.cursor)?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }
}
