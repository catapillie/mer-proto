use std::{
    fs::File,
    io::{self, Write},
};

use crate::run::opcode::Opcode;

use super::ast::{ExprAst, ProgramAst, StmtAst, UnaryOperator, BinaryOperator};

pub struct Codegen<'a> {
    file: &'a mut File,
}

impl<'a> Codegen<'a> {
    pub fn new(file: &'a mut File) -> Self {
        Self { file }
    }

    pub fn gen(&mut self, ast: &ProgramAst) -> Result<(), io::Error> {
        self.gen_stmt_list(ast)?;
        self.file.write_all(&[Opcode::halt as u8])?;
        Ok(())
    }

    fn gen_stmt_list(&mut self, stmts: &Vec<StmtAst>) -> Result<(), io::Error> {
        for stmt in stmts {
            self.gen_stmt(stmt)?;
        }
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &StmtAst) -> Result<(), io::Error> {
        match stmt {
            StmtAst::Empty => Ok(()),
            StmtAst::Expr(expr) => self.gen_expr(expr),
            StmtAst::Block(_) => todo!(),
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

    fn gen_block(&mut self, block: &Vec<StmtAst>) -> Result<(), io::Error> {
        todo!()
    }

    fn gen_expr(&mut self, expr: &ExprAst) -> Result<(), io::Error> {
        match expr {
            ExprAst::Number(num) => {
                self.file.write_all(&[Opcode::ld_num_const as u8])?;
                self.file.write_all(&num.to_be_bytes())?;
                Ok(())
            }
            ExprAst::Identifier(_) => todo!(),
            ExprAst::Boolean(b) => {
                let op = if *b {
                    Opcode::ld_true_const
                } else {
                    Opcode::ld_false_const
                } as u8;
                self.file.write_all(&[op])?;
                Ok(())
            }
            ExprAst::BinaryOp(op, left, right) => {
                self.gen_expr(left)?;
                self.gen_expr(right)?;
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
                    BinaryOperator::Equal => todo!(),
                } as u8])?;
                Ok(())
            },
            ExprAst::UnaryOp(op, expr) => {
                self.gen_expr(expr)?;
                self.file.write_all(&[match op {
                    UnaryOperator::Plus => Opcode::op_plus,
                    UnaryOperator::Minus => Opcode::op_minus,
                    UnaryOperator::Not => Opcode::op_not,
                } as u8])?;
                Ok(())
            }
            ExprAst::Bad => unreachable!(),
        }
    }
}
