use std::io;

use super::{Codegen, Loc};
use crate::{
    binary,
    com::abt::{FunctionInfo, Program},
    runtime::Opcode,
};

impl Codegen {
    pub fn gen_function(&mut self, info: &FunctionInfo, abt: &Program) -> io::Result<()> {
        let param_count: u8 = info
            .arg_ids
            .iter()
            .map(|id| abt.size_of(&abt.variables.get(id).unwrap().ty).unwrap() as u8)
            .sum();
        let local_count: u8 = info
            .used_variables
            .keys()
            .map(|id| abt.size_of(&abt.variables.get(id).unwrap().ty).unwrap() as u8)
            .sum();
        let opcode = Opcode::function(info.name.value.clone(), param_count, local_count);
        binary::write_opcode(&mut self.cursor, &opcode)?;
        let mut loc = 0;
        self.current_locals.clear();
        for (&id, _) in info.used_variables.iter() {
            let storage = Self::size_of_var_storage(id, abt) as u8;
            let param_size = abt.size_of(&abt.variables.get(&id).unwrap().ty).unwrap() as u8;
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
                let size = abt.size_of(&abt.variables.get(arg_id).unwrap().ty).unwrap() as u8;
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
            .unwrap_or_else(|| panic!("unresolved function code for '{}'", info.name.value));
        self.gen_statement(code, abt)?;
        Ok(())
    }
}
