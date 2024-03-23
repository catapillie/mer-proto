use byteorder::WriteBytesExt;
use std::io;

use super::Codegen;
use crate::{
    binary,
    com::abt::{Expr, Program, Type},
    runtime::{opcode, NativeType, Opcode},
};

#[allow(dead_code)]
pub enum Value {
    Done,
    Local(u8),
    UnknownLocal,
    Address,
}

impl Codegen {
    pub fn gen_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        let ty = abt.type_of(expr);
        let size = abt.size_of(&ty).unwrap();
        match self.gen_value(expr, abt)? {
            Value::Done => Ok(()),
            Value::Local(loc) => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc)),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_loc_n(loc, size as u8)),
            },
            Value::UnknownLocal => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_sloc),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_sloc_n(size as u8)),
            },
            Value::Address => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(size as u8)),
            },
        }
    }

    pub fn gen_value(&mut self, expr: &Expr, abt: &Program) -> io::Result<Value> {
        use Expr as E;
        match expr {
            E::Unknown => unreachable!(
                "analysis stage should prevent unknown expressions from being compiled"
            ),
            E::Debug(expr, ty) => self.gen_debug_expression(expr, ty, abt),
            E::Todo => {
                binary::write_opcode(&mut self.cursor, &Opcode::todo)?;
                Ok(Value::Done)
            }
            E::Unreachable => {
                binary::write_opcode(&mut self.cursor, &Opcode::unreachable)?;
                Ok(Value::Done)
            }
            E::Unit => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_unit)?;
                Ok(Value::Done)
            }
            E::Integer(num) => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_i64(*num))?;
                Ok(Value::Done)
            }
            E::Decimal(num) => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_f64(*num))?;
                Ok(Value::Done)
            }
            E::Boolean(b) => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(*b as u8))?;
                Ok(Value::Done)
            }
            E::StringLiteral(s) => {
                for byte in s.bytes() {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(byte))?;
                }
                Ok(Value::Done)
            }
            E::Binary(op, left, right) => {
                self.gen_binary_operation_expression(op, left, right, abt)
            }
            E::Unary(op, expr) => self.gen_unary_operation_expression(op, expr, abt),
            E::Assignment {
                assignee,
                var_id,
                expr,
            } => self.gen_assignment_expression(assignee, *var_id, expr, abt),
            E::Variable(var_id) => self.gen_variable_expression(*var_id, abt),
            E::Function(func_id) => self.gen_function_expression(*func_id),
            E::Tuple(head, tail) => self.gen_tuple_expression(head, tail, abt),
            E::TupleImmediateIndex(tuple, index) => {
                self.gen_tuple_immediate_index_expression(tuple, index, abt)
            }
            E::Array(exprs) => self.gen_array_expression(exprs, abt),
            E::ArrayImmediateIndex(array, index) => {
                self.gen_array_immediate_index_expression(array, index, abt)
            }
            E::ArrayIndex(array, index) => self.gen_array_index_expression(array, index, abt),
            E::PointerIndex(pointer, index) => {
                self.gen_pointer_index_expression(pointer, index, abt)
            }
            E::Call(id, params, _) => self.gen_call_expression(*id, params, abt),
            E::IndirectCall(callee, args, _) => {
                self.gen_indirect_call_expression(callee, args, abt)
            }
            E::Heap(expr) => self.gen_heap_expression(expr, abt),
            E::Ref(lvalue, var_id, _) => self.gen_ref_expression(lvalue, *var_id, abt),
            E::Deref(expr) => self.gen_deref_expression(expr, abt),
            E::Case(paths, fallback, _) => self.gen_case_expression(paths, fallback, abt),
            E::CaseTernary(guard, expr, fallback, _) => {
                self.gen_case_ternary_expression(guard, expr, fallback, abt)
            }
            E::Data(_, fields) => self.gen_data_expression(fields, abt),
            E::DataWith(id, expr, fields) => self.gen_data_with_expression(*id, expr, fields, abt),
            E::FieldAccess {
                expr,
                data_id,
                field_id,
            } => self.gen_field_access_expression(expr, *data_id, *field_id, abt),
            E::Alloc(ty, size) => self.gen_alloc_expression(ty, size, abt),
            E::ToPointer(expr) => self.gen_to_pointer_expression(expr, abt),
        }
    }

    fn gen_debug_expression(&mut self, expr: &Expr, ty: &Type, abt: &Program) -> io::Result<Value> {
        self.gen_expression(expr, abt)?;
        let ty = match abt.dealias_type(ty) {
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
        Ok(Value::Done)
    }

    fn gen_tuple_expression(
        &mut self,
        head: &Expr,
        tail: &[Expr],
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_expression(head, abt)?;
        for expr in tail.iter() {
            self.gen_expression(expr, abt)?;
        }
        Ok(Value::Done)
    }

    fn gen_tuple_immediate_index_expression(
        &mut self,
        tuple: &Expr,
        index: &usize,
        abt: &Program,
    ) -> io::Result<Value> {
        let tuple_ty = abt.type_of(tuple);
        let total_size = abt.size_of(&tuple_ty).unwrap() as u8;
        let Type::Tuple(head, tail) = abt.dealias_type(&tuple_ty) else {
            unreachable!()
        };

        let (offset, size) = match index {
            0 => (0, abt.size_of(head).unwrap() as u8),
            _ => {
                let mut offset = abt.size_of(head).unwrap() as u8;
                for ty in &tail[..(index - 1)] {
                    offset += abt.size_of(ty).unwrap() as u8;
                }
                (offset, abt.size_of(&tail[index - 1]).unwrap() as u8)
            }
        };

        let value = self.gen_value(tuple, abt)?;
        match value {
            Value::Done => {
                binary::write_opcode(&mut self.cursor, &Opcode::keep(offset, size, total_size))?;
                Ok(Value::Done)
            }
            Value::Local(loc) => Ok(Value::Local(loc + offset)),
            Value::UnknownLocal => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::UnknownLocal)
            }
            Value::Address => {
                if offset != 0 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64 * 8))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                }
                Ok(Value::Address)
            }
        }
    }

    fn gen_array_expression(&mut self, exprs: &[Expr], abt: &Program) -> io::Result<Value> {
        for expr in exprs.iter() {
            self.gen_expression(expr, abt)?;
        }
        Ok(Value::Done)
    }

    fn gen_array_immediate_index_expression(
        &mut self,
        array: &Expr,
        index: &usize,
        abt: &Program,
    ) -> io::Result<Value> {
        let array_ty = abt.type_of(array);
        let total_size = abt.size_of(&array_ty).unwrap() as u8;
        let Type::Array(inner_ty, _) = abt.dealias_type(&array_ty) else {
            unreachable!()
        };

        let size = abt.size_of(inner_ty).unwrap() as u8;
        let offset = *index as u8 * size;

        let value = self.gen_value(array, abt)?;
        match value {
            Value::Done => {
                binary::write_opcode(&mut self.cursor, &Opcode::keep(offset, size, total_size))?;
                Ok(Value::Done)
            }
            Value::Local(loc) => Ok(Value::Local(loc + offset)),
            Value::UnknownLocal => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::UnknownLocal)
            }
            Value::Address => {
                if offset != 0 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64 * 8))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                }
                Ok(Value::Address)
            }
        }
    }

    fn gen_array_index_expression(
        &mut self,
        array: &Expr,
        index: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        let array_ty = abt.type_of(array);
        let total_size = abt.size_of(&array_ty).unwrap() as u8;
        let Type::Array(inner_ty, _) = abt.dealias_type(&array_ty) else {
            unreachable!()
        };

        let size = abt.size_of(inner_ty).unwrap() as u8;
        let value = self.gen_value(array, abt)?;

        self.gen_expression(index, abt)?;

        match value {
            Value::Done => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::keep_at(size, total_size))?;
                Ok(Value::Done)
            }
            Value::Local(loc) => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(loc as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::UnknownLocal)
            }
            Value::UnknownLocal => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::UnknownLocal)
            }
            Value::Address => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8 * size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::Address)
            }
        }
    }

    fn gen_pointer_index_expression(
        &mut self,
        pointer: &Expr,
        index: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        let pointer_ty = abt.type_of(pointer);
        let Type::Pointer(inner_ty) = abt.dealias_type(&pointer_ty) else {
            unreachable!()
        };

        let pointer_value = self.gen_value(pointer, abt)?;
        match pointer_value {
            Value::Done => binary::write_opcode(&mut self.cursor, &Opcode::pop)?,
            Value::Local(loc) => binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc))?,
            Value::UnknownLocal => binary::write_opcode(&mut self.cursor, &Opcode::ld_sloc)?,
            Value::Address => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?,
        }

        let size = abt.size_of(inner_ty).unwrap() as u8;
        self.gen_expression(index, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8 * size as u64))?;
        binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
        Ok(Value::Address)
    }

    fn gen_variable_expression(&mut self, var_id: u64, abt: &Program) -> io::Result<Value> {
        let info = abt.variables.get(&var_id).unwrap();
        let loc = self.current_locals.get(&var_id).unwrap();

        if info.is_on_heap {
            binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
            Ok(Value::Address)
        } else {
            Ok(Value::Local(loc.offset))
        }
    }

    fn gen_function_expression(&mut self, func_id: u64) -> io::Result<Value> {
        self.cursor.write_u8(opcode::ld_u32)?;
        self.add_fn_addr_placeholder(func_id)?;
        Ok(Value::Done)
    }

    fn gen_call_expression(
        &mut self,
        id: u64,
        params: &[Expr],
        abt: &Program,
    ) -> io::Result<Value> {
        for param in params.iter() {
            self.gen_expression(param, abt)?;
        }
        self.cursor.write_u8(opcode::call)?;
        self.add_fn_addr_placeholder(id)?;
        Ok(Value::Done)
    }

    fn gen_heap_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<Value> {
        self.gen_expression(expr, abt)?;
        let size = abt.size_of(&abt.type_of(expr)).unwrap() as u8;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::alloc)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::alloc_n(size))?,
        }
        Ok(Value::Done)
    }

    fn gen_deref_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<Value> {
        self.gen_expression(expr, abt)?;
        Ok(Value::Address)
    }

    fn gen_data_expression(&mut self, fields: &[Expr], abt: &Program) -> io::Result<Value> {
        for field in fields.iter() {
            self.gen_expression(field, abt)?;
        }
        Ok(Value::Done)
    }

    fn gen_data_with_expression(
        &mut self,
        data_id: u64,
        expr: &Expr,
        fields: &[(usize, Expr)],
        abt: &Program,
    ) -> io::Result<Value> {
        let data_ty = abt.type_of(expr);
        let total_size = abt.size_of(&data_ty).unwrap();
        let data_info = abt.datas.get(&data_id).unwrap();

        self.gen_expression(expr, abt)?;

        let mut offset = 0;
        let mut field_index = 0;
        for (field_id, field_expr) in fields.iter() {
            while field_index < *field_id {
                offset += abt.size_of(&data_info.fields[field_index].1.value).unwrap();
                field_index += 1;
            }
            let field_ty = &data_info.fields[*field_id].1.value;
            let field_size = abt.size_of(field_ty).unwrap();

            self.gen_expression(field_expr, abt)?;
            binary::write_opcode(
                &mut self.cursor,
                &Opcode::replace(field_size as u8, (total_size - offset) as u8),
            )?;

            offset += field_size;
            field_index += 1;
        }

        Ok(Value::Done)
    }

    fn gen_field_access_expression(
        &mut self,
        expr: &Expr,
        data_id: u64,
        field_id: usize,
        abt: &Program,
    ) -> io::Result<Value> {
        let info = abt.datas.get(&data_id).unwrap();
        let data_size = abt.size_of(&abt.type_of(expr)).unwrap();
        let field_size = abt.size_of(&info.fields[field_id].1.value).unwrap();
        let field_offset = info
            .fields
            .iter()
            .take(field_id)
            .map(|(_, ty)| abt.size_of(&ty.value).unwrap())
            .sum::<usize>();

        let value = self.gen_value(expr, abt)?;
        match value {
            Value::Done => {
                binary::write_opcode(
                    &mut self.cursor,
                    &Opcode::keep(field_offset as u8, field_size as u8, data_size as u8),
                )?;
                Ok(Value::Done)
            }
            Value::Local(loc) => Ok(Value::Local(loc + field_offset as u8)),
            Value::UnknownLocal => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(field_offset as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::UnknownLocal)
            }
            Value::Address => {
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8 * field_offset as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(Value::Address)
            }
        }
    }

    fn gen_case_expression(
        &mut self,
        paths: &[(Expr, Expr)],
        fallback: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        let mut skip_placeholders = Vec::new();
        let mut prev_guard_cursor = None;
        for (guard, expr) in paths.iter() {
            let cursor_before_guard = self.position();

            // <guard> then ...
            self.gen_expression(guard, abt)?;
            self.cursor.write_u8(opcode::jmp_if_not)?;

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
        self.gen_expression(fallback, abt)?;

        // wire all jumps to end of case-then-otherwise expression
        let cursor_end = self.position();
        for placeholder in skip_placeholders {
            self.patch_u32_placeholder(placeholder, cursor_end)?;
        }

        Ok(Value::Done)
    }

    fn gen_indirect_call_expression(
        &mut self,
        callee: &Expr,
        args: &[Expr],
        abt: &Program,
    ) -> io::Result<Value> {
        for arg in args.iter() {
            self.gen_expression(arg, abt)?;
        }
        self.gen_expression(callee, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::call_addr)?;
        Ok(Value::Done)
    }

    fn gen_case_ternary_expression(
        &mut self,
        guard: &Expr,
        expr: &Expr,
        fallback: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
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

        Ok(Value::Done)
    }

    fn gen_alloc_expression(
        &mut self,
        ty: &Type,
        size_expr: &Expr,
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_expression(size_expr, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::dup)?;

        let size = abt.size_of(ty).unwrap();
        if size != 1 {
            binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
            binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
        }

        binary::write_opcode(&mut self.cursor, &Opcode::mem_alloc)?;
        binary::write_opcode(&mut self.cursor, &Opcode::rot)?;
        Ok(Value::Done)
    }

    pub fn gen_to_pointer_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<Value> {
        let Type::Ref(inner) = abt.type_of(expr) else {
            unreachable!()
        };
        let Type::Array(_, size) = *inner else {
            unreachable!()
        };

        self.gen_expression(expr, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(size as u64))?;
        Ok(Value::Done)
    }
}
