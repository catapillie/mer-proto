use std::io;

use super::Codegen;
use crate::{
    binary,
    com::abt::{BoundPattern, Expr, Program, Size},
    runtime::Opcode,
};

impl Codegen {
    pub fn gen_deconstruction(
        &mut self,
        pattern: &BoundPattern,
        expr: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
        self.apply_pattern(pattern, abt)?;
        Ok(())
    }

    pub fn apply_pattern(&mut self, pattern: &BoundPattern, abt: &Program) -> io::Result<()> {
        match pattern {
            BoundPattern::Discard {
                len: Size::Known(size),
            } => match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::pop),
                _ => binary::write_opcode(&mut self.cursor, &Opcode::pop_n(*size as u8)),
            },
            BoundPattern::Loc { id } => self.gen_variable_init_pattern(*id, abt),
            BoundPattern::Seq(pats) => {
                for pat in pats.iter().rev() {
                    self.apply_pattern(pat, abt)?;
                }
                Ok(())
            }
            BoundPattern::Ref {
                pat,
                len: Size::Known(size),
            } => {
                match size {
                    1 => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap)?,
                    _ => binary::write_opcode(&mut self.cursor, &Opcode::ld_heap_n(*size as u8))?,
                };
                self.apply_pattern(pat, abt)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn gen_variable_init_pattern(&mut self, var_id: u64, abt: &Program) -> io::Result<()> {
        let loc = self.current_locals.get(&var_id).unwrap();
        let info = abt.variables.get(&var_id).unwrap();

        // if variable is heap-allocated, allocate the value on the heap, keep the address
        if info.is_on_heap {
            let size = abt.size_of(&info.ty).unwrap() as u8;
            match size {
                1 => binary::write_opcode(&mut self.cursor, &Opcode::alloc)?,
                _ => binary::write_opcode(&mut self.cursor, &Opcode::alloc_n(size))?,
            }
        }

        // write value (only one value space is taken if the variable is heap-allocated)
        match loc.size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::st_loc(loc.offset))?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::st_loc_n(loc.offset, loc.size))?,
        }

        Ok(())
    }
}
