use std::io;

use super::{expression::Value, Codegen};
use crate::{
    binary,
    com::abt::{LValue, Program, Type},
    runtime::{NativeType, Opcode},
};

impl Codegen {
    pub fn gen_ref_expression(
        &mut self,
        lvalue: &LValue,
        var_id: u64,
        abt: &Program,
    ) -> io::Result<Value> {
        self.gen_lvalue_address(lvalue, var_id, abt)?;
        Ok(Value::Done)
    }

    fn gen_lvalue_address(
        &mut self,
        lvalue: &LValue,
        var_id: u64,
        abt: &Program,
    ) -> io::Result<()> {
        let info = abt.variables.get(&var_id).unwrap();
        match lvalue {
            LValue::Variable => {
                let loc = self.current_locals.get(&var_id).unwrap();
                binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
                Ok(())
            }
            LValue::Deref(inner) => {
                self.gen_lvalue_address(inner, var_id, abt)?;
                if info.is_on_heap {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                }
                Ok(())
            }
            LValue::TupleImmediateIndex(inner, tuple_ty, index) => {
                let Type::Tuple(head, tail) = tuple_ty else {
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

                self.gen_lvalue_address(inner, var_id, abt)?;
                if offset != 0 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64 * 8))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                }
                Ok(())
            }
            LValue::ArrayImmediateIndex(inner, array_ty, index) => {
                let Type::Array(inner_ty, _) = array_ty else {
                    unreachable!()
                };
                let offset = abt.size_of(inner_ty) * index;

                self.gen_lvalue_address(inner, var_id, abt)?;
                if offset != 0 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(offset as u64 * 8))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                }
                Ok(())
            }
            LValue::ArrayIndex(a, array_ty, index_expr) => {
                let Type::Array(inner_ty, _) = array_ty else {
                    unreachable!()
                };

                self.gen_lvalue_address(a, var_id, abt)?;
                self.gen_expression(index_expr, abt)?;
                let inner_size = abt.size_of(inner_ty);
                if inner_size > 1 {
                    binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(inner_size as u64))?;
                    binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                }
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;

                Ok(())
            }
            LValue::PointerIndex(inner, ptr_ty, index_expr) => {
                let Type::Pointer(inner_ty) = ptr_ty else {
                    unreachable!()
                };

                self.gen_lvalue_address(inner, var_id, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?;
                let inner_size = abt.size_of(inner_ty);
                self.gen_expression(index_expr, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8 * inner_size as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::mul(NativeType::u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;

                Ok(())
            }
            LValue::FieldAccess(inner, data_id, field_id) => {
                let info = abt.datas.get(data_id).unwrap();
                let field_offset = info
                    .fields
                    .iter()
                    .take(*field_id)
                    .map(|(_, ty)| abt.size_of(&ty.value))
                    .sum::<usize>();
                self.gen_lvalue_address(inner, var_id, abt)?;
                binary::write_opcode(&mut self.cursor, &Opcode::ld_u64(8 * field_offset as u64))?;
                binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                Ok(())
            }
        }
    }
}
