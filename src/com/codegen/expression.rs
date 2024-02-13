use std::io;

use byteorder::WriteBytesExt;

use super::Codegen;
use crate::{
    binary,
    com::abt::{Expr, Program, Type},
    runtime::{opcode, NativeType, Opcode},
};

impl Codegen {
    pub fn gen_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        use Expr as E;
        match expr {
            E::Unknown => unreachable!(
                "analysis stage should prevent unknown expressions from being compiled"
            ),
            E::Debug(expr, ty) => self.gen_debug_expression(expr, ty, abt),
            E::Todo => binary::write_opcode(&mut self.cursor, &Opcode::todo),
            E::Unreachable => binary::write_opcode(&mut self.cursor, &Opcode::unreachable),
            E::Unit => binary::write_opcode(&mut self.cursor, &Opcode::ld_unit),
            E::Integer(num) => binary::write_opcode(&mut self.cursor, &Opcode::ld_i64(*num)),
            E::Decimal(num) => binary::write_opcode(&mut self.cursor, &Opcode::ld_f64(*num)),
            E::Boolean(b) => binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(*b as u8)),
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
            E::Call(id, params, _) => self.gen_call_expression(*id, params, abt),
            E::IndirectCall(callee, args, _) => {
                self.gen_indirect_call_expression(callee, args, abt)
            }
            E::Ref(expr) => self.gen_ref_expression(expr, abt),
            E::VarRef(var_id) => self.gen_var_ref_expression(*var_id),
            E::Deref(expr) => self.gen_deref_expression(expr, abt),
            E::VarDeref(var_id) => self.gen_var_deref_expression(*var_id, abt),
            E::Case(paths, fallback, _) => self.gen_case_expression(paths, fallback, abt),
            E::CaseTernary(guard, expr, fallback, _) => {
                self.gen_case_ternary_expression(guard, expr, fallback, abt)
            }
            E::Data(_, fields) => self.gen_data_expression(fields, abt),
            E::FieldAccess {
                expr,
                data_id,
                field_id,
            } => self.gen_field_access_expression(expr, *data_id, *field_id, abt),
        }
    }

    fn gen_debug_expression(&mut self, expr: &Expr, ty: &Type, abt: &Program) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
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
        binary::write_opcode(&mut self.cursor, &Opcode::dbg(ty))
    }

    fn gen_tuple_expression(
        &mut self,
        head: &Expr,
        tail: &[Expr],
        abt: &Program,
    ) -> io::Result<()> {
        self.gen_expression(head, abt)?;
        for expr in tail.iter() {
            self.gen_expression(expr, abt)?;
        }
        Ok(())
    }

    fn gen_tuple_immediate_index_expression(
        &mut self,
        tuple: &Expr,
        index: &usize,
        abt: &Program,
    ) -> io::Result<()> {
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
            binary::write_opcode(&mut self.cursor, &Opcode::keep(offset, size, total_size))?;
        }
        Ok(())
    }

    fn gen_array_expression(&mut self, exprs: &[Expr], abt: &Program) -> io::Result<()> {
        for expr in exprs.iter() {
            self.gen_expression(expr, abt)?;
        }
        Ok(())
    }

    fn gen_array_immediate_index_expression(
        &mut self,
        array: &Expr,
        index: &usize,
        abt: &Program,
    ) -> io::Result<()> {
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

    fn gen_array_index_expression(
        &mut self,
        array: &Expr,
        index: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
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

    fn gen_variable_expression(&mut self, var_id: u64, abt: &Program) -> io::Result<()> {
        let info = abt.variables.get(&var_id).unwrap();
        let loc = self.current_locals.get(&var_id).unwrap();
        match loc.size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_loc_n(loc.offset, loc.size))?,
        }

        if info.is_on_heap {
            let ty_size = abt.size_of(&info.ty) as u8;
            match ty_size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?,
                _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(ty_size))?,
            }
        }

        Ok(())
    }

    fn gen_function_expression(&mut self, func_id: u64) -> io::Result<()> {
        self.cursor.write_u8(opcode::ld_u32)?;
        self.add_fn_addr_placeholder(func_id)?;
        Ok(())
    }

    fn gen_call_expression(&mut self, id: u64, params: &[Expr], abt: &Program) -> io::Result<()> {
        for param in params.iter() {
            self.gen_expression(param, abt)?;
        }
        self.cursor.write_u8(opcode::call)?;
        self.add_fn_addr_placeholder(id)?;
        Ok(())
    }

    fn gen_ref_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
        let size = abt.size_of(&abt.type_of(expr)) as u8;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::alloc)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::alloc_n(size))?,
        }
        Ok(())
    }

    fn gen_var_ref_expression(&mut self, var_id: u64) -> io::Result<()> {
        let loc = self.current_locals.get(&var_id).unwrap();
        binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
        Ok(())
    }

    fn gen_deref_expression(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
        let Type::Ref(inner) = abt.type_of(expr) else {
            unreachable!()
        };

        let size = abt.size_of(&inner) as u8;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(size))?,
        }
        Ok(())
    }

    fn gen_var_deref_expression(&mut self, var_id: u64, abt: &Program) -> io::Result<()> {
        let loc = self.current_locals.get(&var_id).unwrap();
        binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;

        let Type::Ref(inner) = &abt.variables.get(&var_id).unwrap().ty else {
            unreachable!()
        };
        let size = abt.size_of(inner) as u8;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(size))?,
        }
        Ok(())
    }

    fn gen_data_expression(&mut self, fields: &[Expr], abt: &Program) -> Result<(), io::Error> {
        for field in fields.iter() {
            self.gen_expression(field, abt)?;
        }
        Ok(())
    }

    fn gen_field_access_expression(
        &mut self,
        expr: &Expr,
        data_id: u64,
        field_id: usize,
        abt: &Program,
    ) -> Result<(), io::Error> {
        let info = abt.datas.get(&data_id).unwrap();
        let data_size = abt.size_of(&abt.type_of(expr));
        let field_size = abt.size_of(&info.fields[field_id].1.value);
        let field_offset = info
            .fields
            .iter()
            .take(field_id)
            .map(|(_, ty)| abt.size_of(&ty.value))
            .sum::<usize>();

        self.gen_expression(expr, abt)?;
        binary::write_opcode(
            &mut self.cursor,
            &Opcode::keep(field_offset as u8, field_size as u8, data_size as u8),
        )?;
        Ok(())
    }

    fn gen_case_expression(
        &mut self,
        paths: &[(Expr, Expr)],
        fallback: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
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
        self.gen_expression(fallback, abt)?;

        // wire all jumps to end of case-then-otherwise expression
        let cursor_end = self.position();
        for placeholder in skip_placeholders {
            self.patch_u32_placeholder(placeholder, cursor_end)?;
        }

        Ok(())
    }

    fn gen_indirect_call_expression(
        &mut self,
        callee: &Expr,
        args: &[Expr],
        abt: &Program,
    ) -> io::Result<()> {
        for arg in args.iter() {
            self.gen_expression(arg, abt)?;
        }
        self.gen_expression(callee, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::call_addr)?;
        Ok(())
    }

    fn gen_case_ternary_expression(
        &mut self,
        guard: &Expr,
        expr: &Expr,
        fallback: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
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
}
