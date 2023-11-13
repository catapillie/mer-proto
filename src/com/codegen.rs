use std::{
    collections::HashMap,
    fs::File,
    io::{self, Write},
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

pub struct Codegen<'a> {
    file: &'a mut File,
}

impl<'a> Codegen<'a> {
    pub fn new(file: &'a mut File) -> Self {
        Self { file }
    }

    pub fn gen(&mut self, ast: &ProgramAst) -> Result<(), io::Error> {
        let locals = Self::count_locals(ast);
        self.file
            .write_all(&[Opcode::init_loc as u8, locals.count().to_be_bytes()[0]])?;

        self.gen_stmt_list(ast, &locals, 0)?;
        self.file.write_all(&[Opcode::halt as u8])?;
        Ok(())
    }

    fn count_locals(stmts: &Vec<StmtAst>) -> LocalsInfo {
        let mut locals = LocalsInfo::default();
        Self::count_locals_at(&mut locals, stmts, 0);
        locals
    }

    fn count_locals_at(locals: &mut LocalsInfo, stmts: &Vec<StmtAst>, depth: u8) {
        for stmt in stmts {
            match stmt {
                StmtAst::Expr(ExprAst::BinaryOp(BinaryOperator::Equal, assignee, _)) => {
                    let ExprAst::Identifier(ref id) = **assignee else {
                        continue;
                    };

                    let n = locals.indices.len() as u8;
                    locals.indices.entry((id.clone(), depth)).or_insert(n);
                }
                StmtAst::Block(stmts) => {
                    Self::count_locals_at(locals, stmts, depth + 1);
                }
                _ => {}
            }
        }
    }

    fn gen_stmt_list(&mut self, stmts: &Vec<StmtAst>, locals: &LocalsInfo, depth: u8) -> Result<(), io::Error> {
        for stmt in stmts {
            self.gen_stmt(stmt, locals, depth)?;
        }
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &StmtAst, locals: &LocalsInfo, depth: u8) -> Result<(), io::Error> {
        match stmt {
            StmtAst::Empty => Ok(()),
            StmtAst::Expr(expr) => {
                let is_assignemt = self.gen_expr(expr, locals, depth)?;
                if !is_assignemt {
                    self.file.write_all(&[Opcode::pop as u8])?;
                }
                Ok(())
            },
            StmtAst::Block(stmts) => self.gen_block(stmts, locals, depth),
            StmtAst::IfThen(_, _) => todo!(),
            StmtAst::Then(_) => todo!(),
            StmtAst::IfThenElse(_, _, _) => todo!(),
            StmtAst::Else(_) => unreachable!(),
            StmtAst::WhileDo(_, _) => todo!(),
            StmtAst::DoWhile(_, _) => todo!(),
            StmtAst::Do(_) => unreachable!(),
            StmtAst::Return => todo!(),
            StmtAst::ReturnWith(_) => todo!(),
        }
    }

    fn gen_block(&mut self, stmts: &Vec<StmtAst>, locals: &LocalsInfo, depth: u8) -> Result<(), io::Error> {
        self.gen_stmt_list(stmts, locals, depth + 1)
    }

    // Ok(true) -> assignment was codegen'd
    fn gen_expr(&mut self, expr: &ExprAst, locals: &LocalsInfo, depth: u8) -> Result<bool, io::Error> {
        match expr {
            ExprAst::Number(num) => {
                self.file.write_all(&[Opcode::ld_num_const as u8])?;
                self.file.write_all(&num.to_be_bytes())?;
                Ok(false)
            }
            ExprAst::Identifier(id) => {
                let loc = locals.get_local_index(id, depth).expect("unknown variable");
                self.file.write_all(&[Opcode::ld_loc as u8, loc])?;
                Ok(false)
            },
            ExprAst::Boolean(b) => {
                let op = if *b {
                    Opcode::ld_true_const
                } else {
                    Opcode::ld_false_const
                } as u8;
                self.file.write_all(&[op])?;
                Ok(false)
            }
            ExprAst::BinaryOp(BinaryOperator::Equal, assignee, value) => {
                let ExprAst::Identifier(ref id) = **assignee else {
                    panic!();
                };
                self.gen_expr(value, locals, depth)?;
                let loc = locals.get_local_index(id, depth).unwrap();
                self.file.write_all(&[Opcode::st_loc as u8, loc])?;
                Ok(true) // assignment
            }
            ExprAst::BinaryOp(op, left, right) => {
                self.gen_expr(left, locals, depth)?;
                self.gen_expr(right, locals, depth)?;
                self.file.write_all(&[match op {
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
                } as u8])?;
                Ok(false)
            }
            ExprAst::UnaryOp(op, expr) => {
                self.gen_expr(expr, locals, depth)?;
                self.file.write_all(&[match op {
                    UnaryOperator::Plus => Opcode::op_plus,
                    UnaryOperator::Minus => Opcode::op_minus,
                    UnaryOperator::Not => Opcode::op_not,
                } as u8])?;
                Ok(false)
            }
            ExprAst::Bad => unreachable!(),
        }
    }
}
