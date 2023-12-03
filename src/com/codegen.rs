use std::io::Cursor;

use crate::run::opcode::Opcode;

use super::abt::{ProgramAbt, Function, StmtAbt, ExprAbt};

pub struct Codegen {
    code: Vec<Opcode>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
        }
    }

    pub fn gen(&mut self, abt: &ProgramAbt) -> Vec<u8> {
        for (id, (name, func)) in abt.functions_by_id.iter() {
            self.gen_function(*id, name.clone(), func);
        }

        let program = Vec::new();
        let mut cursor = Cursor::new(program);
        for opcode in &self.code {
            opcode.write_bytes(&mut cursor).unwrap();
        }

        cursor.into_inner()
    }

    fn gen_function(&mut self, id: u32, name: String, func: &Function) {
        self.code.push(Opcode::function(name, func.param_types.len() as u8, 0));
        self.gen_statement(&func.code);
    }

    fn gen_statement(&mut self, stmt: &StmtAbt) {
        use super::abt::StmtAbtKind as S;
        match &stmt.kind {
            S::Empty => {},
            S::Block(stmts) => self.gen_block(stmts),
            S::Expr(_) => todo!(),
            S::IfThen(_, _) => todo!(),
            S::IfThenElse(_, _, _) => todo!(),
            S::WhileDo(_, _) => todo!(),
            S::DoWhile(_, _) => todo!(),
            S::Return(expr) => self.gen_return(expr),
        }
    }

    fn gen_block(&mut self, stmts: &Vec<StmtAbt>) {
        for stmt in stmts {
            self.gen_statement(stmt);
        }
    }

    fn gen_return(&mut self, expr: &ExprAbt) {
        self.gen_expression(expr);
        self.code.push(Opcode::ret);
    }

    fn gen_expression(&mut self, expr: &ExprAbt) {
        use super::abt::ExprAbt as E;
        match expr {
            E::Unknown => unreachable!(),
            E::Unit => self.code.push(Opcode::ld_unit),
            E::Number(_) => todo!(),
            E::Boolean(_) => todo!(),
            E::Variable(_) => todo!(),
            E::Assignment(_, _) => todo!(),
            E::Unary(_, _) => todo!(),
            E::Binary(_, _, _) => todo!(),
            E::Call(_, _, _) => todo!(),
        }
    }
}