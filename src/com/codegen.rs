use std::{
    collections::HashMap,
    io::{self, Cursor},
};

use byteorder::WriteBytesExt;
use byteorder::LE;

use crate::{
    com::abt::{BinOpAbtKind, StmtAbtKind, TypeAbt, UnOpAbtKind},
    runtime::{
        native_type::NativeType,
        opcode::{self, Opcode},
    },
};

use super::{
    abt::{ExprAbt, ProgramAbt, StmtAbt},
    analysis::FunctionInfo,
};

pub struct Codegen {
    cursor: Cursor<Vec<u8>>,
    current_locals: HashMap<u64, u8>,
    function_positions: HashMap<u64, u32>,
    call_placeholders: Vec<(u32, u64)>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            cursor: Cursor::new(Vec::new()),
            current_locals: Default::default(),
            function_positions: Default::default(),
            call_placeholders: Default::default(),
        }
    }

    fn position(&self) -> u32 {
        self.cursor.position().try_into().unwrap()
    }

    fn add_fn_addr_placeholder(&mut self, id: u64) -> io::Result<()> {
        let position = self.position();
        self.call_placeholders.push((position, id));
        self.cursor.write_u32::<LE>(0)
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
        self.add_fn_addr_placeholder(abt.main_fn_id)?;

        for info in abt.functions.values() {
            self.function_positions.insert(info.id, self.position());
            self.gen_function(info, abt)?;
        }

        for (pos, id) in self.call_placeholders.clone() {
            let addr = *self.function_positions.get(&id).unwrap();
            self.patch_u32_placeholder(pos, addr)?;
        }

        Ok(self.cursor.into_inner())
    }

    fn gen_function(&mut self, info: &FunctionInfo, abt: &ProgramAbt) -> io::Result<()> {
        let param_count: u8 = info.args.len().try_into().unwrap();
        let local_count: u8 = info.used_variables.len().try_into().unwrap();
        let opcode = Opcode::function(info.name.clone(), param_count, local_count);
        opcode.write_bytes(&mut self.cursor)?;

        self.current_locals.clear();
        for (loc, id) in info
            .used_variables
            .iter()
            .enumerate()
            .map(|(loc, (&id, _))| (loc as u8, id))
        {
            self.current_locals.insert(id, loc);
        }

        /*
         * before we generate the code, some of the functions arguments may be marked as heap-allocated.
         * we need the function to replace its arguments by pointers to corresponding to the re-allocated arguments.
         * it is not possible to allocate the arguments directly when generating an expression,
         * because functions may be used as values, and in that case, there is no way to know
         * whether the arguments of the function (as a value) being called are heap-allocated.
         * we make each function re-allocate its own arguments when they are marked as heap-allocated.
         */
        for arg_id in &info.arg_ids {
            let is_on_heap = abt.variables[arg_id].is_on_heap;
            if is_on_heap {
                Opcode::realloc_loc(self.current_locals[arg_id]).write_bytes(&mut self.cursor)?;
            }
        }

        let code = info
            .code
            .as_ref()
            .unwrap_or_else(|| panic!("unresolved function code {}", info.name));
        self.gen_statement(code, abt)?;
        Ok(())
    }

    fn gen_statement(&mut self, stmt: &StmtAbt, abt: &ProgramAbt) -> io::Result<()> {
        use StmtAbtKind as S;
        match &stmt.kind {
            S::Empty => {}
            S::Block(stmts) => {
                for stmt in stmts {
                    self.gen_statement(stmt, abt)?;
                }
            }
            S::Expr(expr) => {
                self.gen_expression(expr, abt)?;
                Opcode::pop.write_bytes(&mut self.cursor)?;
            }
            S::VarInit(var_id, expr) => {
                // = <value>
                self.gen_expression(expr, abt)?;

                let id = *self.current_locals.get(var_id).unwrap();
                let info = abt.variables.get(var_id).unwrap();

                // if variable is heap-allocated, allocate the value on the heap, keep the address
                if info.is_on_heap {
                    Opcode::alloc.write_bytes(&mut self.cursor)?;
                }

                // write value
                Opcode::st_loc(id).write_bytes(&mut self.cursor)?;
            }
            S::IfThen(guard, body) => {
                // if ...
                self.gen_expression(guard, abt)?;
                Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_from = self.gen_u32_placeholder()?;

                // then ...
                self.gen_statement(body.as_ref(), abt)?;

                // write saved jump address
                let cursor_to = self.position();
                self.patch_u32_placeholder(cursor_from, cursor_to)?;
            }
            S::IfThenElse(guard, body_then, body_else) => {
                // if ... then
                self.gen_expression(guard, abt)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_guard_end = self.gen_u32_placeholder()?;

                // else ...
                self.gen_statement(body_else, abt)?;
                self.cursor.write_u8(opcode::jmp)?;
                let cursor_body_else_end = self.gen_u32_placeholder()?;

                // then ...
                let cursor_body_then_start = self.position();
                self.gen_statement(body_then, abt)?;
                let cursor_body_then_end = self.position();

                // write saved jump addresses
                self.patch_u32_placeholder(cursor_body_else_end, cursor_body_then_end)?;
                self.patch_u32_placeholder(cursor_guard_end, cursor_body_then_start)?;
            }
            S::WhileDo(guard, body) => {
                // while ...
                let cursor_guard_start = self.position();
                self.gen_expression(guard, abt)?;
                Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_guard_end = self.gen_u32_placeholder()?;

                // do ...
                self.gen_statement(body, abt)?;
                self.cursor.write_u8(opcode::jmp)?;
                self.cursor.write_u32::<LE>(cursor_guard_start)?;

                // write saved jump address
                let cursor_body_end = self.position();
                self.patch_u32_placeholder(cursor_guard_end, cursor_body_end)?;
            }
            S::DoWhile(body, guard) => {
                // do ...
                let cursor_stmt_start = self.position();
                self.gen_statement(body, abt)?;

                // while ...
                self.gen_expression(guard, abt)?;
                Opcode::jmp_if(cursor_stmt_start).write_bytes(&mut self.cursor)?;
            }
            S::Return(expr) => {
                self.gen_expression(expr, abt)?;
                Opcode::ret.write_bytes(&mut self.cursor)?;
            }
        }

        Ok(())
    }

    fn gen_expression(&mut self, expr: &ExprAbt, abt: &ProgramAbt) -> io::Result<()> {
        use super::abt::ExprAbt as E;
        match expr {
            E::Unknown => unreachable!(),
            E::Todo => Opcode::todo.write_bytes(&mut self.cursor),
            E::Unreachable => Opcode::unreachable.write_bytes(&mut self.cursor),
            E::Debug(inner, ty) => {
                self.gen_expression(inner, abt)?;
                let ty = match ty {
                    TypeAbt::Unit => NativeType::unit,
                    TypeAbt::U8 => NativeType::u8,
                    TypeAbt::U16 => NativeType::u16,
                    TypeAbt::U32 => NativeType::u32,
                    TypeAbt::U64 => NativeType::u64,
                    TypeAbt::I8 => NativeType::i8,
                    TypeAbt::I16 => NativeType::i16,
                    TypeAbt::I32 => NativeType::i32,
                    TypeAbt::I64 => NativeType::i64,
                    TypeAbt::F32 => NativeType::f32,
                    TypeAbt::F64 => NativeType::f64,
                    TypeAbt::Bool => NativeType::bool,
                    _ => unreachable!(),
                };
                Opcode::dbg(ty).write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Unit => Opcode::ld_unit.write_bytes(&mut self.cursor),
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
            E::Variable(var) => {
                let info = abt.variables.get(var).unwrap();
                let id = *self.current_locals.get(var).unwrap();
                Opcode::ld_loc(id).write_bytes(&mut self.cursor)?;

                if info.is_on_heap {
                    Opcode::ld_heap.write_bytes(&mut self.cursor)?;
                }

                Ok(())
            }
            E::Function(func) => {
                self.cursor.write_u8(opcode::ld_u32)?;
                self.add_fn_addr_placeholder(*func)?;
                Ok(())
            }
            E::Assignment {
                var_id,
                deref_count,
                expr,
            } => {
                // = <value>
                self.gen_expression(expr, abt)?;
                Opcode::dup.write_bytes(&mut self.cursor)?;

                let id = *self.current_locals.get(var_id).unwrap();
                let info = abt.variables.get(var_id).unwrap();
                if *deref_count == 0 {
                    // simple variable assignment
                    if info.is_on_heap {
                        Opcode::ld_loc(id).write_bytes(&mut self.cursor)?;
                        Opcode::st_heap.write_bytes(&mut self.cursor)?;
                    } else {
                        Opcode::st_loc(id).write_bytes(&mut self.cursor)?;
                    }
                } else {
                    // assignment to a n-dereferenced pointer
                    // we dereference the variable n - 1 times so we have the address, (and not the value)
                    Opcode::ld_loc(id).write_bytes(&mut self.cursor)?;
                    for _ in 1..(*deref_count) {
                        Opcode::ld_heap.write_bytes(&mut self.cursor)?;
                    }

                    // if variable is on heap, then the value we have is an address to an address, so read from heap again
                    if info.is_on_heap {
                        Opcode::ld_heap.write_bytes(&mut self.cursor)?;
                    }

                    // write the value at the address (on the heap)
                    Opcode::st_heap.write_bytes(&mut self.cursor)?;
                }

                Ok(())
            }
            E::Call(id, params, _) => {
                for param in params {
                    self.gen_expression(param, abt)?;
                }
                self.cursor.write_u8(opcode::call)?;
                self.add_fn_addr_placeholder(*id)?;
                Ok(())
            }
            E::IndirectCall(callee, args, _) => {
                for arg in args {
                    self.gen_expression(arg, abt)?;
                }
                self.gen_expression(callee, abt)?;
                Opcode::call_addr.write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Binary(op, left, right) => {
                use BinOpAbtKind as K;
                use NativeType as Nt;
                use Opcode as O;
                use TypeAbt as Ty;

                // short-circuiting logic
                match &op.kind {
                    K::And => return self.gen_short_circuit_and(left, right, abt),
                    K::Or => return self.gen_short_circuit_or(left, right, abt),
                    _ => {}
                }

                self.gen_expression(left, abt)?;
                self.gen_expression(right, abt)?;

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
                self.gen_expression(expr, abt)?;

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
            E::Ref(expr) => {
                self.gen_expression(expr, abt)?;
                Opcode::alloc.write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::VarRef(var_id) => {
                let id = *self.current_locals.get(var_id).unwrap();
                Opcode::ld_loc(id).write_bytes(&mut self.cursor)?;
                Ok(())
            }
            E::Deref(expr) => {
                self.gen_expression(expr, abt)?;
                Opcode::ld_heap.write_bytes(&mut self.cursor)?;
                Ok(())
            }
        }
    }

    fn gen_short_circuit_and(
        &mut self,
        left: &ExprAbt,
        right: &ExprAbt,
        abt: &ProgramAbt,
    ) -> io::Result<()> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        Opcode::neg(NativeType::bool).write_bytes(&mut self.cursor)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        Opcode::bitand(NativeType::bool).write_bytes(&mut self.cursor)?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }

    fn gen_short_circuit_or(
        &mut self,
        left: &ExprAbt,
        right: &ExprAbt,
        abt: &ProgramAbt,
    ) -> Result<(), io::Error> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        Opcode::bitor(NativeType::bool).write_bytes(&mut self.cursor)?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }
}
