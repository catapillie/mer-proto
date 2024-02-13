use byteorder::{WriteBytesExt, LE};
use std::{
    collections::HashMap,
    io::{self, Cursor},
};

use super::abt::Program;
use crate::runtime::opcode;

mod assignment;
mod expression;
mod function;
mod operation;
mod statement;

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
}
