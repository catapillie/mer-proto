use std::io;

use super::{Codegen, Loc};
use crate::{
    binary,
    com::abt::{FunctionInfo, Program},
    runtime::Opcode,
};

impl Codegen {
    fn size_of_variable_id(id: u64, abt: &Program) -> u8 {
        abt.size_of(&abt.variables.get(&id).unwrap().ty).unwrap() as u8
    }

    fn count_args_size(info: &FunctionInfo, abt: &Program) -> u8 {
        info.arg_ids
            .iter()
            .chain(info.captured_variables.iter())
            .map(|&id| Self::size_of_variable_id(id, abt))
            .sum()
    }

    fn count_locals_size(info: &FunctionInfo, abt: &Program) -> u8 {
        info.local_variables
            .iter()
            .map(|&id| Self::size_of_variable_id(id, abt))
            .sum()
    }

    pub fn gen_function(&mut self, info: &FunctionInfo, abt: &Program) -> io::Result<()> {
        let args_count = Self::count_args_size(info, abt);
        let local_count = Self::count_locals_size(info, abt);
        let opcode = Opcode::function(info.name.value.clone(), args_count, local_count);
        binary::write_opcode(&mut self.cursor, &opcode)?;

        // reset the map from variable ids to offsets and storage sizes
        self.current_locals.clear();

        // register all local variables
        let mut loc = 0;
        let ids = info
            .captured_variables
            .iter()
            .chain(info.local_variables.iter());

        for &id in ids {
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
                let opcode = match size {
                    1 => Opcode::realloc_loc(self.current_locals[arg_id].offset),
                    _ => Opcode::realloc_loc_n(self.current_locals[arg_id].offset, size),
                };
                binary::write_opcode(&mut self.cursor, &opcode)?;
            }
        }

        // generate function code
        let code = info
            .code
            .as_ref()
            .unwrap_or_else(|| panic!("unresolved function code for '{}'", info.name.value));
        self.gen_statement(code, abt)?;

        Ok(())
    }
}
