use byteorder::WriteBytesExt;
use std::io;

use super::Codegen;
use crate::{
    binary,
    com::{
        abt::{BinOp, BinOpKind, Expr, Program, Type, UnOp, UnOpKind},
        codegen::expression::Value,
    },
    runtime::{opcode, NativeType, Opcode},
};

impl Codegen {
    pub fn gen_binary_operation_expression(
        &mut self,
        op: &BinOp,
        left: &Expr,
        right: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        use BinOpKind as K;
        use NativeType as Nt;
        use Opcode as O;
        use Type as Ty;

        // short-circuiting logic
        match &op.kind {
            K::And => return self.gen_short_circuit_and(left, right, abt),
            K::Or => return self.gen_short_circuit_or(left, right, abt),
            _ => {}
        }

        self.gen_expression(left, abt)?;
        self.gen_expression(right, abt)?;

        // there is literally nothing to do:
        // the two values appear in sequence on the stack, so they are concatenated!
        if let K::Concat = op.kind {
            return Ok(Value::Done);
        }

        let opcode = match (&op.in_left, &op.kind) {
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

        binary::write_opcode(&mut self.cursor, &opcode)?;
        Ok(Value::Done)
    }

    fn gen_short_circuit_and(
        &mut self,
        left: &Expr,
        right: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        binary::write_opcode(&mut self.cursor, &Opcode::neg(NativeType::bool))?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::bitand(NativeType::bool))?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(Value::Done)
    }

    fn gen_short_circuit_or(
        &mut self,
        left: &Expr,
        right: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::bitor(NativeType::bool))?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(Value::Done)
    }

    pub fn gen_unary_operation_expression(
        &mut self,
        op: &UnOp,
        expr: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_expression(expr, abt)?;

        use Type as Ty;
        use UnOpKind as K;
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
            | (Ty::F64, K::Pos) => return Ok(Value::Done), // this operation does nothing
            (Ty::I8, K::Neg) => Opcode::neg(NativeType::i8),
            (Ty::I16, K::Neg) => Opcode::neg(NativeType::i16),
            (Ty::I32, K::Neg) => Opcode::neg(NativeType::i32),
            (Ty::I64, K::Neg) => Opcode::neg(NativeType::i64),
            (Ty::F32, K::Neg) => Opcode::neg(NativeType::f32),
            (Ty::F64, K::Neg) => Opcode::neg(NativeType::f64),
            (Ty::Bool, K::Not) => Opcode::neg(NativeType::bool),
            _ => unreachable!(),
        };

        binary::write_opcode(&mut self.cursor, &opcode)?;
        Ok(Value::Done)
    }
}
