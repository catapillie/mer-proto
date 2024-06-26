use byteorder::{WriteBytesExt, LE};
use std::io;

use super::Codegen;
use crate::{
    binary,
    com::abt::{Expr, Program, Stmt, StmtKind},
    runtime::{opcode, Opcode},
};

impl Codegen {
    #[rustfmt::skip]
    pub fn gen_statement(&mut self, stmt: &Stmt, abt: &Program) -> io::Result<()> {
        use StmtKind as S;
        match &stmt.value {
            S::Empty => Ok(()),
            S::Block(stmts)
                => self.gen_block_statement(stmts, abt),
            S::Expr(expr)
                => self.gen_expression_statement(expr, abt),
            S::Deconstruct(pat, expr)
                => self.gen_deconstruction(pat, expr, abt),
            S::IfThen(guard, body)
                => self.gen_if_then_statement(guard, body, abt),
            S::IfThenElse(guard, body_then, body_else)
                => self.gen_if_then_else_statement(guard, body_else, body_then, abt),
            S::WhileDo(guard, body)
                => self.gen_while_do_statement(guard, body, abt),
            S::DoWhile(body, guard)
                => self.gen_do_while_statement(body, guard, abt),
            S::Return(expr)
                => self.gen_return_statement(expr, abt),
            S::Print(expr)
                => self.gen_print_statement(expr, abt),
        }
    }

    fn gen_block_statement(&mut self, stmts: &[Stmt], abt: &Program) -> io::Result<()> {
        for stmt in stmts {
            self.gen_statement(stmt, abt)?;
        }
        Ok(())
    }

    fn gen_expression_statement(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        let size = abt.size_of(&expr.value.ty).unwrap() as u8;
        self.gen_expression(expr, abt)?;
        match size {
            1 => binary::write_opcode(&mut self.cursor, &Opcode::pop)?,
            _ => binary::write_opcode(&mut self.cursor, &Opcode::pop_n(size))?,
        }
        Ok(())
    }

    fn gen_if_then_statement(
        &mut self,
        guard: &Expr,
        body: &Stmt,
        abt: &Program,
    ) -> io::Result<()> {
        // if ...
        self.gen_expression(guard, abt)?;
        self.cursor.write_u8(opcode::jmp_if_not)?;
        let cursor_from = self.gen_u32_placeholder()?;

        // then ...
        self.gen_statement(body, abt)?;

        // write saved jump address
        let cursor_to = self.position();
        self.patch_u32_placeholder(cursor_from, cursor_to)?;

        Ok(())
    }

    fn gen_if_then_else_statement(
        &mut self,
        guard: &Expr,
        body_else: &Stmt,
        body_then: &Stmt,
        abt: &Program,
    ) -> io::Result<()> {
        // if ... then
        self.gen_expression(guard, abt)?;
        self.cursor.write_u8(opcode::jmp_if)?;
        let cursor_guard_end = self.gen_u32_placeholder()?;

        // else ...
        self.gen_statement(body_else, abt)?;
        self.cursor.write_u8(opcode::jmp)?;
        let cursor_body_else_end = self.gen_u32_placeholder()?;

        // then ...
        let cursor_body_then_start = self.position();
        self.gen_statement(body_then, abt)?;
        let cursor_body_then_end = self.position();

        // write saved jump addresses
        self.patch_u32_placeholder(cursor_body_else_end, cursor_body_then_end)?;
        self.patch_u32_placeholder(cursor_guard_end, cursor_body_then_start)?;

        Ok(())
    }

    fn gen_while_do_statement(
        &mut self,
        guard: &Expr,
        body: &Stmt,
        abt: &Program,
    ) -> io::Result<()> {
        // while ...
        let cursor_guard_start = self.position();
        self.gen_expression(guard, abt)?;
        self.cursor.write_u8(opcode::jmp_if_not)?;
        let cursor_guard_end = self.gen_u32_placeholder()?;

        // do ...
        self.gen_statement(body, abt)?;
        self.cursor.write_u8(opcode::jmp)?;
        self.cursor.write_u32::<LE>(cursor_guard_start)?;

        // write saved jump address
        let cursor_body_end = self.position();
        self.patch_u32_placeholder(cursor_guard_end, cursor_body_end)?;

        Ok(())
    }

    pub fn gen_do_while_statement(
        &mut self,
        body: &Stmt,
        guard: &Expr,
        abt: &Program,
    ) -> io::Result<()> {
        // do ...
        let cursor_stmt_start = self.position();
        self.gen_statement(body, abt)?;

        // while ...
        self.gen_expression(guard, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::jmp_if(cursor_stmt_start))?;

        Ok(())
    }

    fn gen_return_statement(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::ret)?;
        Ok(())
    }

    fn gen_print_statement(&mut self, expr: &Expr, abt: &Program) -> io::Result<()> {
        self.gen_expression(expr, abt)?;
        binary::write_opcode(&mut self.cursor, &Opcode::print)?;
        Ok(())
    }
}
