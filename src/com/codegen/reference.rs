use std::io;

use super::{expression::Value, Codegen};
use crate::{
    binary,
    com::abt::{LValue, Program, Type},
    runtime::Opcode,
};

impl Codegen {
    pub fn gen_ref_expression(
        &mut self,
        lvalue: &LValue,
        var_id: u64,
        _ty: &Type,
        _abt: &Program,
    ) -> io::Result<Value> {
        match lvalue {
            LValue::Variable => self.gen_var_ref_expression(var_id),
            LValue::Deref(_) => todo!(),
            LValue::TupleImmediateIndex(_, _, _) => todo!(),
            LValue::ArrayImmediateIndex(_, _, _) => todo!(),
            LValue::ArrayIndex(_, _, _) => todo!(),
            LValue::PointerIndex(_, _, _) => todo!(),
            LValue::FieldAccess(_, _, _) => todo!(),
        }
    }

    fn gen_var_ref_expression(&mut self, var_id: u64) -> io::Result<Value> {
        let loc = self.current_locals.get(&var_id).unwrap();
        binary::write_opcode(&mut self.cursor, &Opcode::ld_loc(loc.offset))?;
        Ok(Value::Done)
    }
}
