use std::io;

use super::Codegen;
use crate::{
    binary,
    com::abt::{Assignee, Expr, Program, Type},
    runtime::{NativeType, Opcode},
};

#[derive(Debug)]
enum Lhs {
    Local(u8),
    UnknownLocal,
    Address,
}

impl Codegen {
    pub fn gen_assignment_expression(
        &mut self,
        assignee: &Assignee,
        var_id: u64,
        expr: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
        // write right-hand side
        let size = abt.size_of(&abt.type_of(expr)) as u8;
        self.gen_expression(expr, abt)?;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::dup)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::dup_n(size))?,
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
            Assignee::FieldAccess(a, data_id, field_id) => {
                let assignment = self.gen_assignment_lhs(a, var_id, abt)?;

                let info = abt.datas.get(data_id).unwrap();
                let field_offset = info
                    .fields
                    .iter()
                    .take(*field_id)
                    .map(|(_, ty)| abt.size_of(&ty.value))
                    .sum::<usize>();

                match assignment {
                    Lhs::Local(loc) => Ok(Lhs::Local(loc + field_offset as u8)),
                    Lhs::UnknownLocal => {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(field_offset as u8))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::UnknownLocal)
                    }
                    Lhs::Address => {
                        binary::write_opcode(&mut self.cursor, &Opcode::ld_u8(8 * field_offset as u8))?;
                        binary::write_opcode(&mut self.cursor, &Opcode::add(NativeType::u64))?;
                        Ok(Lhs::Address)
                    },
                }
            }
        }
    }
}
