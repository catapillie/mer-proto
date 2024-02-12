use byteorder::{WriteBytesExt, LE};
use std::{
    collections::HashMap,
    io::{self, Cursor},
};

use crate::{
    binary,
    com::abt::{Assignee, BinOpKind, Expr, FunctionInfo, Program, Stmt, StmtKind, Type, UnOpKind},
    runtime::{
        native_type::NativeType,
        opcode::{self, Opcode},
    },
};

struct Loc {
    offset: u8,
    size: u8,
}

pub struct Codegen {
    cursor: Cursor<Vec<u8>>,
    current_locals: HashMap<u64, Loc>,
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

    fn size_of_var_storage(id: u64, abt: &Program) -> usize {
        let info = abt.variables.get(&id).unwrap();
        if info.is_on_heap {
            1
        } else {
            abt.size_of(&info.ty)
        }
    }

    pub fn gen(mut self, abt: &Program) -> io::Result<Vec<u8>> {
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

    fn gen_function(&mut self, info: &FunctionInfo, abt: &Program) -> io::Result<()> {
        let param_count: u8 = info
            .arg_ids
            .iter()
            .map(|id| abt.size_of(&abt.variables.get(id).unwrap().ty) as u8)
            .sum();
        let local_count: u8 = info
            .used_variables
            .keys()
            .map(|id| abt.size_of(&abt.variables.get(id).unwrap().ty) as u8)
            .sum();
        let opcode = Opcode::function(info.name.clone(), param_count, local_count);
        binary::write_opcode(&mut self.cursor, &opcode)?;

        let mut loc = 0;
        self.current_locals.clear();
        for (&id, _) in info.used_variables.iter() {
            let storage = Self::size_of_var_storage(id, abt) as u8;
            let param_size = abt.size_of(&abt.variables.get(&id).unwrap().ty) as u8;
            self.current_locals.insert(
                id,
                Loc {
                    offset: loc,
                    size: storage,
                },
            );
            loc += param_size;
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
                let size = abt.size_of(&abt.variables.get(arg_id).unwrap().ty) as u8;
                if size == 1 {
                    binary::write_opcode(
                        &mut self.cursor,
                        &Opcode::realloc_loc(self.current_locals[arg_id].offset),
                    )?;
                } else {
                    binary::write_opcode(
                        &mut self.cursor,
                        &Opcode::realloc_loc_n(self.current_locals[arg_id].offset, size),
                    )?;
                }
            }
        }

        let code = info
            .code
            .as_ref()
            .unwrap_or_else(|| panic!("unresolved function code {}", info.name));
        self.gen_statement(code, abt)?;
        Ok(())
    }

    fn gen_statement(&mut self, stmt: &Stmt, abt: &Program) -> io::Result<()> {
        use StmtKind as S;
        match &stmt.value {
            S::Empty => {}
            S::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.gen_statement(stmt, abt)?;
                }
            }
            S::Expr(expr) => {
                let size = abt.size_of(&abt.type_of(expr)) as u8;
                self.gen_expression(expr, abt)?;
                if size == 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::pop)?;
                } else {
                    binary::write_opcode(&mut self.cursor, &Opcode::pop_n(size))?;
                }
            }
            S::VarInit(var_id, expr) => {
                // = <value>
                self.gen_expression(expr, abt)?;

                let loc = self.current_locals.get(var_id).unwrap();
                let info = abt.variables.get(var_id).unwrap();

                // if variable is heap-allocated, allocate the value on the heap, keep the address
                if info.is_on_heap {
                    let size = abt.size_of(&info.ty) as u8;
                    if size == 1 {
                        binary::write_opcode(&mut self.cursor, &Opcode::alloc)?;
                    } else {
                        binary::write_opcode(&mut self.cursor, &Opcode::alloc_n(size))?;
                    }
                }

                // write value (only one value space is taken if the variable is heap-allocated)
                if loc.size == 1 || info.is_on_heap {
                    binary::write_opcode(&mut self.cursor, &Opcode::st_loc(loc.offset))?;
                } else {
                    binary::write_opcode(
                        &mut self.cursor,
                        &Opcode::st_loc_n(loc.offset, loc.size),
                    )?;
                }
            }
            S::IfThen(guard, body) => {
                // if ...
                self.gen_expression(guard, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::neg(NativeType::bool))?;
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
                binary::write_opcode(&mut self.cursor, &Opcode::neg(NativeType::bool))?;
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
                binary::write_opcode(&mut self.cursor, &Opcode::jmp_if(cursor_stmt_start))?;
            }
            S::Return(expr) => {
                self.gen_expression(expr, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::ret)?;
            }
        }

        Ok(())
    }

    fn gen_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        use super::abt::Expr as E;
        match expr {
            E::Unknown => unreachable!(),
            E::Todo => binary::write_opcode(&mut self.cursor, &Opcode::todo),
            E::Unreachable => binary::write_opcode(&mut self.cursor, &Opcode::unreachable),
            E::Debug(inner, ty) => {
                self.gen_expression(inner, abt)?;
                let ty = match ty {
                    Type::Unit => NativeType::unit,
                    Type::U8 => NativeType::u8,
                    Type::U16 => NativeType::u16,
                    Type::U32 => NativeType::u32,
                    Type::U64 => NativeType::u64,
                    Type::I8 => NativeType::i8,
                    Type::I16 => NativeType::i16,
                    Type::I32 => NativeType::i32,
                    Type::I64 => NativeType::i64,
                    Type::F32 => NativeType::f32,
                    Type::F64 => NativeType::f64,
                    Type::Bool => NativeType::bool,
                    _ => unreachable!(),
                };
                binary::write_opcode(&mut self.cursor, &Opcode::dbg(ty))?;
                Ok(())
            }
            E::Unit => binary::write_opcode(&mut self.cursor, &Opcode::ld_unit),
            E::Integer(num) => binary::write_opcode(&mut self.cursor, &Opcode::ld_i64(*num)),
            E::Decimal(num) => binary::write_opcode(&mut self.cursor, &Opcode::ld_f64(*num)),
            E::Boolean(b) => {
                let opcode = match b {
                    true => Opcode::ld_u8(1),
                    false => Opcode::ld_u8(0),
                };
                binary::write_opcode(&mut self.cursor, &opcode)?;
                Ok(())
            }
            E::Tuple(head, tail) => {
                self.gen_expression(head, abt)?;
                for expr in tail.iter() {
                    self.gen_expression(expr, abt)?;
                }
                Ok(())
            }
            E::TupleImmediateIndex(tuple, index) => {
                let tuple_ty = abt.type_of(tuple);
                let total_size = abt.size_of(&tuple_ty) as u8;
                let Type::Tuple(head, tail) = tuple_ty else {
                    unreachable!()
                };
                self.gen_expression(tuple, abt)?;

                if *index == 0 {
                    let size = abt.size_of(&head) as u8;
                    binary::write_opcode(&mut self.cursor, &Opcode::keep(0, size, total_size))?;
                } else {
                    let size = abt.size_of(&tail[index - 1]) as u8;
                    let mut offset = abt.size_of(&head) as u8;
                    for ty in &tail[..(index - 1)] {
                        offset += abt.size_of(ty) as u8;
                    }
                    binary::write_opcode(
                        &mut self.cursor,
                        &Opcode::keep(offset, size, total_size),
                    )?;
                }
                Ok(())
            }
            E::Array(exprs) => {
                for expr in exprs.iter() {
                    self.gen_expression(expr, abt)?;
                }
                Ok(())
            }
            E::ArrayImmediateIndex(array, index) => {
                let array_ty = abt.type_of(array);
                let total_size = abt.size_of(&array_ty) as u8;
                let Type::Array(inner_ty, _) = array_ty else {
                    unreachable!()
                };
                self.gen_expression(array, abt)?;

                let size = abt.size_of(&inner_ty) as u8;
                let offset = *index as u8 * size;
                binary::write_opcode(&mut self.cursor, &Opcode::keep(offset, size, total_size))?;
                Ok(())
            }
            E::ArrayIndex(array, index) => {
                let array_ty = abt.type_of(array);
                let total_size = abt.size_of(&array_ty) as u8;
                let Type::Array(inner_ty, _) = array_ty else {
                    unreachable!()
                };

                self.gen_expression(array, abt)?;
                self.gen_expression(index, abt)?;

                let size = abt.size_of(&inner_ty) as u8;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::keep_at(size, total_size))?;
                Ok(())
            }
            E::Variable(var) => {
                let info = abt.variables.get(var).unwrap();
                let loc = self.current_locals.get(var).unwrap();
                if loc.size == 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
                } else {
                    binary::write_opcode(
                        &mut self.cursor,
                        &Opcode::ld_loc_n(loc.offset, loc.size),
                    )?;
                }

                if info.is_on_heap {
                    let ty_size = abt.size_of(&info.ty) as u8;
                    if ty_size == 1 {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                    } else {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(ty_size))?;
                    }
                }

                Ok(())
            }
            E::Function(func) => {
                self.cursor.write_u8(opcode::ld_u32)?;
                self.add_fn_addr_placeholder(*func)?;
                Ok(())
            }
            E::Assignment {
                assignee,
                var_id,
                expr,
            } => self.gen_assignment(assignee, *var_id, expr, abt),
            E::Call(id, params, _) => {
                for param in params.iter() {
                    self.gen_expression(param, abt)?;
                }
                self.cursor.write_u8(opcode::call)?;
                self.add_fn_addr_placeholder(*id)?;
                Ok(())
            }
            E::IndirectCall(callee, args, _) => {
                for arg in args.iter() {
                    self.gen_expression(arg, abt)?;
                }
                self.gen_expression(callee, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::call_addr)?;
                Ok(())
            }
            E::Binary(op, left, right) => {
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
                binary::write_opcode(&mut self.cursor, &opcode)?;
                Ok(())
            }
            E::Unary(op, expr) => {
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

                binary::write_opcode(&mut self.cursor, &opcode)?;
                Ok(())
            }
            E::Ref(expr) => {
                self.gen_expression(expr, abt)?;
                let size = abt.size_of(&abt.type_of(expr)) as u8;
                if size == 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::alloc)?;
                } else {
                    binary::write_opcode(&mut self.cursor, &Opcode::alloc_n(size))?;
                }
                Ok(())
            }
            E::VarRef(var_id) => {
                let loc = self.current_locals.get(var_id).unwrap();
                binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
                Ok(())
            }
            E::Deref(expr) => {
                self.gen_expression(expr, abt)?;
                let Type::Ref(inner) = abt.type_of(expr) else {
                    unreachable!()
                };

                let size = abt.size_of(&inner) as u8;
                if size == 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                } else {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(size))?;
                }
                Ok(())
            }
            E::VarDeref(var_id) => {
                let loc = self.current_locals.get(var_id).unwrap();
                binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;

                let Type::Ref(inner) = &abt.variables.get(var_id).unwrap().ty else {
                    unreachable!()
                };
                let size = abt.size_of(inner) as u8;
                if size == 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                } else {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(size))?;
                }
                Ok(())
            }
            E::Case(paths, default, _) => {
                let mut skip_placeholders = Vec::new();
                let mut prev_guard_cursor = None;
                for (guard, expr) in paths.iter() {
                    let cursor_before_guard = self.position();

                    // <guard> then ...
                    self.gen_expression(guard, abt)?;
                    binary::write_opcode(&mut self.cursor, &Opcode::neg(NativeType::bool))?;
                    self.cursor.write_u8(opcode::jmp_if)?;

                    // wire previous conditional jump
                    if let Some(cursor) = prev_guard_cursor {
                        self.patch_u32_placeholder(cursor, cursor_before_guard)?;
                    }
                    prev_guard_cursor = Some(self.gen_u32_placeholder()?);

                    // ... then <expr>
                    self.gen_expression(expr, abt)?;
                    self.cursor.write_u8(opcode::jmp)?;
                    skip_placeholders.push(self.gen_u32_placeholder()?);
                }

                // wire last conditional jump
                let cursor_before_default = self.position();
                if let Some(cursor) = prev_guard_cursor {
                    self.patch_u32_placeholder(cursor, cursor_before_default)?;
                }

                // otherwise <default>
                self.gen_expression(default, abt)?;

                // wire all jumps to end of case-then-otherwise expression
                let cursor_end = self.position();
                for placeholder in skip_placeholders {
                    self.patch_u32_placeholder(placeholder, cursor_end)?;
                }

                Ok(())
            }
            E::CaseTernary(guard, expr, fallback, _) => {
                // <guard> then ...
                self.gen_expression(guard, abt)?;
                self.cursor.write_u8(opcode::jmp_if)?;
                let cursor_guard_end = self.gen_u32_placeholder()?;

                // otherwise <fallback>
                self.gen_expression(fallback, abt)?;
                self.cursor.write_u8(opcode::jmp)?;
                let cursor_body_else_end = self.gen_u32_placeholder()?;

                // ... then <expr>
                let cursor_body_then_start = self.position();
                self.gen_expression(expr, abt)?;
                let cursor_body_then_end = self.position();

                // write saved jump addresses
                self.patch_u32_placeholder(cursor_body_else_end, cursor_body_then_end)?;
                self.patch_u32_placeholder(cursor_guard_end, cursor_body_then_start)?;

                Ok(())
            }
            E::Data(_, fields) => {
                for field in fields.iter() {
                    self.gen_expression(field, abt)?;
                }
                Ok(())
            }
        }
    }

    fn gen_short_circuit_and(
        &mut self,
        left: &Expr,
        right: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        binary::write_opcode(&mut self.cursor, &Opcode::neg(NativeType::bool))?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::bitand(NativeType::bool))?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }

    fn gen_short_circuit_or(
        &mut self,
        left: &Expr,
        right: &Expr,
        abt: &Program,
    ) -> Result<(), io::Error> {
        self.gen_expression(left, abt)?;
        self.cursor.write_u8(opcode::dup)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_a = self.gen_u32_placeholder()?;

        self.gen_expression(right, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::bitor(NativeType::bool))?;
        let cursor_b = self.position();

        self.patch_u32_placeholder(cursor_a, cursor_b)?;
        Ok(())
    }

    fn gen_assignment(
        &mut self,
        assignee: &Assignee,
        var_id: u64,
        expr: &Expr,
        abt: &Program,
    ) -> Result<(), io::Error> {
        // write right-hand side
        let size = abt.size_of(&abt.type_of(expr)) as u8;
        self.gen_expression(expr, abt)?;
        if size == 1 {
            binary::write_opcode(&mut self.cursor, &Opcode::dup)?;
        } else {
            binary::write_opcode(&mut self.cursor, &Opcode::dup_n(size))?;
        }

        // write left-hand side
        let assignment = self.gen_assignment_lhs(assignee, var_id, abt)?;

        // write assignment
        match assignment {
            Lhs::Local(loc_offset) => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::st_loc(loc_offset)),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::st_loc_n(loc_offset, size)),
            },
            Lhs::UnknownLocal => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::st_sloc),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::st_sloc_n(size)),
            },
            Lhs::Address => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::st_heap),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::st_heap_n(size)),
            },
        }
    }

    fn gen_assignment_lhs(
        &mut self,
        assignee: &Assignee,
        var_id: u64,
        abt: &Program,
    ) -> Result<Lhs, io::Error> {
        let loc = self.current_locals.get(&var_id).unwrap();
        let info = abt.variables.get(&var_id).unwrap();
        match assignee {
            Assignee::Variable => {
                if info.is_on_heap {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
                    Ok(Lhs::Address)
                } else {
                    Ok(Lhs::Local(loc.offset))
                }
            }
            Assignee::VarDeref => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
                if info.is_on_heap {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                }
                Ok(Lhs::Address)
            }
            Assignee::Deref(a) => {
                let assignment = self.gen_assignment_lhs(a, var_id, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                Ok(assignment)
            }
            Assignee::TupleImmediateIndex(a, ty, index) => {
                let assignment = self.gen_assignment_lhs(a, var_id, abt)?;
                let Type::Tuple(head, tail) = ty else {
                    unreachable!()
                };

                let offset = if *index == 0 {
                    0
                } else {
                    let mut offset = abt.size_of(head) as u8;
                    for ty in &tail[..(index - 1)] {
                        offset += abt.size_of(ty) as u8;
                    }
                    offset
                };

                match assignment {
                    Lhs::Local(loc) => Ok(Lhs::Local(loc + offset)),
                    Lhs::UnknownLocal => {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(offset))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::UnknownLocal)
                    }
                    Lhs::Address => {
                        if offset != 0 {
                            binary::write_opcode(
                                &mut self.cursor,
                                &Opcode::ld_u64(offset as u64 * 8),
                            )?;
                            binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        }
                        Ok(Lhs::Address)
                    }
                }
            }
            Assignee::ArrayImmediateIndex(a, ty, index) => {
                let assignment = self.gen_assignment_lhs(a, var_id, abt)?;
                let Type::Array(inner, _) = ty else {
                    unreachable!()
                };
                let offset = abt.size_of(inner) * index;
                match assignment {
                    Lhs::Local(loc) => Ok(Lhs::Local(loc + offset as u8)),
                    Lhs::UnknownLocal => {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(offset as u8))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::UnknownLocal)
                    }
                    Lhs::Address => {
                        if offset != 0 {
                            binary::write_opcode(
                                &mut self.cursor,
                                &Opcode::ld_u64(offset as u64 * 8),
                            )?;
                            binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        }
                        Ok(Lhs::Address)
                    }
                }
            }
            Assignee::ArrayIndex(a, ty, index_expr) => {
                let assignment = self.gen_assignment_lhs(a, var_id, abt)?;
                let Type::Array(inner, _) = ty else {
                    unreachable!()
                };

                // gen index
                let inner_size = abt.size_of(inner);
                self.gen_expression(index_expr, abt)?;
                if inner_size > 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(inner_size as u64))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                }

                match assignment {
                    Lhs::Local(loc) => {
                        // index is unknown at compile time
                        // we're going to have to push the local on the stack
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(loc))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::UnknownLocal)
                    }
                    Lhs::UnknownLocal => {
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::UnknownLocal)
                    }
                    Lhs::Address => {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::Address)
                    }
                }
            }
        }
    }
}

enum Lhs {
    Local(u8),
    UnknownLocal,
    Address,
}
