use std::{
    collections::HashMap,
    io::{self},
};

use crate::run::opcode::Opcode;

use super::ast::{BinaryOperator, ExprAst, ProgramAst, StmtAst, UnaryOperator};

#[derive(Default)]
struct LocalsInfo {
    indices: HashMap<(String, u8), u8>,
}

impl LocalsInfo {
    fn count(&self) -> u8 {
        self.indices.len() as u8
    }

    fn get_local_index(&self, id: &str, mut depth: u8) -> Option<u8> {
        loop {
            if let Some(&index) = self.indices.get(&(id.to_string(), depth)) {
                return Some(index);
            }

            if depth == 0 {
                break;
            }

            depth -= 1;
        }

        None
    }
}

pub struct Codegen {
    code: Vec<u8>,
}

impl Codegen {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn gen(mut self, ast: &ProgramAst) -> Result<Vec<u8>, io::Error> {
        let locals = Self::count_locals(ast);

        self.code.push(Opcode::entry_point as u8);
        let cursor_entry_point = self.code.len();

        self.gen_marker("@main");

        let bytes = (4 + self.code.len() as u32).to_be_bytes();
        self.code.insert(cursor_entry_point, bytes[3]);
        self.code.insert(cursor_entry_point, bytes[2]);
        self.code.insert(cursor_entry_point, bytes[1]);
        self.code.insert(cursor_entry_point, bytes[0]);

        self.code.push(Opcode::init_loc as u8);
        self.code.push(locals.count().to_be());

        self.gen_stmt_list(ast, &locals, 0, 0)?;

        self.code.push(Opcode::ret as u8);

        Ok(self.code)
    }

    fn gen_marker(&mut self, name: &str) {
        let bytes = name.as_bytes();
        let len: u16 = bytes.len().try_into().unwrap();

        self.code.push(Opcode::marker as u8);
        self.code.extend_from_slice(&len.to_be_bytes());
        self.code.extend_from_slice(bytes);
    }

    fn count_locals(stmts: &Vec<StmtAst>) -> LocalsInfo {
        let mut locals = LocalsInfo::default();
        Self::count_locals_at(&mut locals, stmts, 0);
        locals
    }

    fn count_locals_at(locals: &mut LocalsInfo, stmts: &Vec<StmtAst>, depth: u8) {
        for stmt in stmts {
            Self::count_locals_in_stmt(locals, stmt, depth);
        }
    }

    // must issue recursive calls for statements that contain nested statements
    fn count_locals_in_stmt(locals: &mut LocalsInfo, stmt: &StmtAst, depth: u8) {
        match stmt {
            StmtAst::VarDef(id, _) => {
                let Some(id) = id else {
                    return;
                };

                let n = locals.indices.len() as u8;
                locals.indices.entry((id.clone(), depth)).or_insert(n);
            }
            StmtAst::Block(stmts) => {
                Self::count_locals_at(locals, stmts, depth + 1);
            }
            StmtAst::IfThen(_, stmt) => Self::count_locals_in_stmt(locals, stmt, depth),
            StmtAst::IfThenElse(_, stmt_if, stmt_else) => {
                Self::count_locals_in_stmt(locals, stmt_if, depth);
                Self::count_locals_in_stmt(locals, stmt_else, depth);
            }
            StmtAst::WhileDo(_, stmt) => Self::count_locals_in_stmt(locals, stmt, depth),
            StmtAst::DoWhile(stmt, _) => Self::count_locals_in_stmt(locals, stmt, depth),
            _ => {}
        }
    }

    fn gen_stmt_list(
        &mut self,
        stmts: &Vec<StmtAst>,
        locals: &LocalsInfo,
        depth: u8,
        cursor_offset: u32,
    ) -> Result<(), io::Error> {
        for stmt in stmts {
            self.gen_stmt(stmt, locals, depth, cursor_offset)?;
        }
        Ok(())
    }

    fn gen_stmt(
        &mut self,
        stmt: &StmtAst,
        locals: &LocalsInfo,
        depth: u8,
        cursor_offset: u32,
    ) -> Result<(), io::Error> {
        match stmt {
            StmtAst::Empty => Ok(()),

            StmtAst::VarDef(Some(id), expr) => {
                self.gen_expr(expr, locals, depth)?;
                let loc = locals.get_local_index(id, depth).unwrap();
                self.code.push(Opcode::st_loc as u8);
                self.code.push(loc);
                Ok(())
            }

            StmtAst::Expr(expr) => {
                let is_assignemt = self.gen_expr(expr, locals, depth)?;
                if !is_assignemt {
                    self.code.push(Opcode::dbg as u8);
                }
                Ok(())
            }
            StmtAst::Block(stmts) => self.gen_block(stmts, locals, depth, cursor_offset),

            StmtAst::IfThen(guard, body) => {
                self.gen_expr(guard, locals, depth)?;
                self.code.push(Opcode::op_not as u8);
                self.code.push(Opcode::jmp_if as u8);
                let cursor_from = self.code.len();
                self.gen_stmt(body.as_ref(), locals, depth, cursor_offset + 4)?;
                let cursor_to = cursor_offset + 4 + self.code.len() as u32;
                let bytes = cursor_to.to_be_bytes();
                self.code.insert(cursor_from, bytes[3]);
                self.code.insert(cursor_from, bytes[2]);
                self.code.insert(cursor_from, bytes[1]);
                self.code.insert(cursor_from, bytes[0]);
                Ok(())
            }
            StmtAst::IfThenElse(guard, body_if, body_else) => {
                self.gen_expr(guard, locals, depth)?;
                self.code.push(Opcode::op_not as u8);

                self.code.push(Opcode::jmp_if as u8);
                let cursor_guard_end = self.code.len();

                self.gen_stmt(body_if, locals, depth, cursor_offset + 8)?;

                self.code.push(Opcode::jmp as u8);
                let cursor_body_if_end = self.code.len();

                self.gen_stmt(body_else, locals, depth, cursor_offset + 8)?;

                let cursor_body_else_end = self.code.len();

                let bytes = (cursor_offset + 8 + cursor_body_else_end as u32).to_be_bytes();
                self.code.insert(cursor_body_if_end, bytes[3]);
                self.code.insert(cursor_body_if_end, bytes[2]);
                self.code.insert(cursor_body_if_end, bytes[1]);
                self.code.insert(cursor_body_if_end, bytes[0]);

                let bytes = (cursor_offset + 8 + cursor_body_if_end as u32).to_be_bytes();
                self.code.insert(cursor_guard_end, bytes[3]);
                self.code.insert(cursor_guard_end, bytes[2]);
                self.code.insert(cursor_guard_end, bytes[1]);
                self.code.insert(cursor_guard_end, bytes[0]);

                Ok(())
            }

            StmtAst::WhileDo(guard, stmt) => {
                let cursor_guard_start = self.code.len();
                self.gen_expr(guard, locals, depth)?;
                self.code.push(Opcode::op_not as u8);
                self.code.push(Opcode::jmp_if as u8);
                let cursor_guard_end = self.code.len();

                self.gen_stmt(stmt, locals, depth, cursor_offset + 4)?;
                self.code.push(Opcode::jmp as u8);
                let bytes = (cursor_offset + cursor_guard_start as u32).to_be_bytes();
                self.code.push(bytes[0]);
                self.code.push(bytes[1]);
                self.code.push(bytes[2]);
                self.code.push(bytes[3]);

                let cursor_body_end = self.code.len();

                let bytes = (cursor_offset + 4 + cursor_body_end as u32).to_be_bytes();
                self.code.insert(cursor_guard_end, bytes[3]);
                self.code.insert(cursor_guard_end, bytes[2]);
                self.code.insert(cursor_guard_end, bytes[1]);
                self.code.insert(cursor_guard_end, bytes[0]);

                Ok(())
            }

            StmtAst::DoWhile(stmt, guard) => {
                let cursor_stmt_start = self.code.len();
                self.gen_stmt(stmt, locals, depth, cursor_offset)?;

                self.gen_expr(guard, locals, depth)?;
                self.code.push(Opcode::jmp_if as u8);

                let bytes = (cursor_offset + cursor_stmt_start as u32).to_be_bytes();
                self.code.extend_from_slice(&bytes);

                Ok(())
            }

            StmtAst::VarDef(None, _) => unreachable!(),

            StmtAst::Return => todo!(),
            StmtAst::ReturnWith(_) => todo!(),

            StmtAst::Then(_) => unreachable!(),
            StmtAst::Else(_) => unreachable!(),
            StmtAst::Do(_) => unreachable!(),
        }
    }

    fn gen_block(
        &mut self,
        stmts: &Vec<StmtAst>,
        locals: &LocalsInfo,
        depth: u8,
        cursor_offset: u32,
    ) -> Result<(), io::Error> {
        self.gen_stmt_list(stmts, locals, depth + 1, cursor_offset)
    }

    fn gen_expr(
        &mut self,
        expr: &ExprAst,
        locals: &LocalsInfo,
        depth: u8,
    ) -> Result<(bool), io::Error> {
        match expr {
            ExprAst::Number(num) => {
                self.code.push(Opcode::ld_num_const as u8);
                self.code.extend_from_slice(&num.to_be_bytes());
                Ok(false)
            }
            ExprAst::Identifier(id) => {
                let loc = locals.get_local_index(id, depth).expect("unknown variable");
                self.code.push(Opcode::ld_loc as u8);
                self.code.push(loc);
                Ok(false)
            }
            ExprAst::Boolean(b) => {
                let op = if *b {
                    Opcode::ld_true_const
                } else {
                    Opcode::ld_false_const
                } as u8;
                self.code.push(op);
                Ok(false)
            }
            ExprAst::BinaryOp(BinaryOperator::Equal, assignee, value) => {
                let ExprAst::Identifier(ref id) = **assignee else {
                    panic!();
                };
                self.gen_expr(value, locals, depth)?;
                let loc = locals.get_local_index(id, depth).unwrap();
                self.code.push(Opcode::st_loc as u8);
                self.code.push(loc);
                Ok(true) // assignment
            }
            ExprAst::BinaryOp(op, left, right) => {
                self.gen_expr(left, locals, depth)?;
                self.gen_expr(right, locals, depth)?;
                self.code.push(match op {
                    BinaryOperator::Plus => Opcode::op_add,
                    BinaryOperator::Minus => Opcode::op_sub,
                    BinaryOperator::Star => Opcode::op_mul,
                    BinaryOperator::Slash => Opcode::op_div,
                    BinaryOperator::Percent => Opcode::op_mod,
                    BinaryOperator::EqualEqual => Opcode::op_eq,
                    BinaryOperator::NotEqual => Opcode::op_ne,
                    BinaryOperator::LessEqual => Opcode::op_le,
                    BinaryOperator::LessThan => Opcode::op_lt,
                    BinaryOperator::GreaterEqual => Opcode::op_ge,
                    BinaryOperator::GreaterThan => Opcode::op_gt,
                    BinaryOperator::Ampersand => Opcode::op_amp,
                    BinaryOperator::Caret => Opcode::op_car,
                    BinaryOperator::Bar => Opcode::op_bar,
                    BinaryOperator::And => todo!(),
                    BinaryOperator::Or => todo!(),
                    BinaryOperator::Equal => unreachable!(),
                } as u8);
                Ok(false)
            }
            ExprAst::UnaryOp(op, expr) => {
                self.gen_expr(expr, locals, depth)?;
                self.code.push(match op {
                    UnaryOperator::Plus => Opcode::op_plus,
                    UnaryOperator::Minus => Opcode::op_minus,
                    UnaryOperator::Not => Opcode::op_not,
                } as u8);
                Ok(false)
            }
            ExprAst::Bad => unreachable!(),
        }
    }
}
