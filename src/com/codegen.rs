use std::{
    collections::HashMap,
    io::{self},
};

use crate::run::opcode::{self, Opcode};

use super::ast::{
    BinaryOperator, ExprAst, ExprAstKind, ProgramAst, StmtAst, StmtAstKind, UnaryOperator,
};

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
    code: Vec<Opcode>,
}

impl Codegen {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn gen(mut self, ast: &ProgramAst) -> Result<Vec<u8>, io::Error> {
        let locals = Self::count_locals(ast);

        let cursor_entry_point: usize = self.gen_entry_point_placeholder();

        let functions = self.gen_functions(ast);

        self.replace_u32(cursor_entry_point, self.code.len() as u32);
        self.gen_function_header("@main", 0, locals.count());
        self.gen_stmt_list(ast, &locals, &functions, 0);
        self.code.push(opcode::ret);

        Ok(self.code)
    }

    fn gen_functions(&mut self, stmts: &Vec<StmtAst>) -> HashMap<(String, u8), u32> {
        let mut functions = HashMap::new();
        for stmt in stmts {
            let StmtAstKind::Func(Some(name), params, body, _) = &stmt.kind else {
                continue;
            };

            let mut locals = LocalsInfo::default();
            for (i, (param, _)) in params.iter().enumerate() {
                locals.indices.insert((param.clone(), 0), i as u8);
            }
            Self::count_locals_in_stmt(&mut locals, body, 0);

            let fp = self.code.len() as u32;
            functions.insert((name.clone(), params.len() as u8), fp);

            self.gen_function_header(name.as_str(), params.len() as u8, locals.count());
            self.gen_stmt(body, &locals, &functions, 0);
            self.code.push(opcode::ret);
        }
        functions
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
        match &stmt.kind {
            StmtAstKind::VarDef(id, _) => {
                let Some(id) = id else {
                    return;
                };

                let n = locals.indices.len() as u8;
                locals.indices.entry((id.clone(), depth)).or_insert(n);
            }
            StmtAstKind::Block(stmts) => {
                Self::count_locals_at(locals, stmts, depth + 1);
            }
            StmtAstKind::IfThen(_, stmt) => Self::count_locals_in_stmt(locals, stmt, depth),
            StmtAstKind::IfThenElse(_, stmt_if, stmt_else) => {
                Self::count_locals_in_stmt(locals, stmt_if, depth);
                Self::count_locals_in_stmt(locals, stmt_else, depth);
            }
            StmtAstKind::WhileDo(_, stmt) => Self::count_locals_in_stmt(locals, stmt, depth),
            StmtAstKind::DoWhile(stmt, _) => Self::count_locals_in_stmt(locals, stmt, depth),
            _ => {}
        }
    }

    fn gen_entry_point_placeholder(&mut self) -> usize {
        let cursor = self.code.len() + 1;
        let code = [opcode::entry_point, 0, 0, 0, 0];
        self.code.extend_from_slice(&code);
        cursor
    }

    fn gen_function_header(&mut self, name: &str, param_count: u8, local_count: u8) {
        let bytes = name.as_bytes();
        let len: u16 = bytes.len().try_into().unwrap();

        self.code.push(opcode::function);
        self.code.extend_from_slice(&len.to_le_bytes());
        self.code.extend_from_slice(bytes);
        self.code.push(param_count);
        self.code.push(local_count);
    }

    fn gen_call(&mut self, fp: u32) {
        self.code.push(opcode::call);
        self.push_u32(fp);
    }

    fn gen_jmp_placeholder(&mut self) -> usize {
        let cursor = self.code.len() + 1;
        let code = [opcode::jmp, 0, 0, 0, 0];
        self.code.extend_from_slice(&code);
        cursor
    }

    fn gen_jmp_if_placeholder(&mut self) -> usize {
        let cursor = self.code.len() + 1;
        let code = [opcode::jmp_if, 0, 0, 0, 0];
        self.code.extend_from_slice(&code);
        cursor
    }

    #[allow(clippy::identity_op)]
    fn replace_u32(&mut self, cursor: usize, val: u32) {
        let bytes = val.to_le_bytes();
        self.code[cursor + 0] = bytes[0];
        self.code[cursor + 1] = bytes[1];
        self.code[cursor + 2] = bytes[2];
        self.code[cursor + 3] = bytes[3];
    }

    fn push_u32(&mut self, val: u32) {
        self.code.extend_from_slice(&val.to_le_bytes());
    }

    fn gen_stmt_list(
        &mut self,
        stmts: &Vec<StmtAst>,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) {
        for stmt in stmts {
            self.gen_stmt(stmt, locals, functions, depth);
        }
    }

    fn gen_st_loc(&mut self, loc: u8) {
        self.code.extend_from_slice(&[opcode::st_loc, loc]);
    }

    fn gen_ld_loc(&mut self, loc: u8) {
        self.code.extend_from_slice(&[opcode::ld_loc, loc]);
    }

    fn gen_stmt(
        &mut self,
        stmt: &StmtAst,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) {
        match &stmt.kind {
            StmtAstKind::Empty => {}

            StmtAstKind::VarDef(Some(id), expr) => {
                self.gen_expr(expr, locals, functions, depth);
                self.gen_st_loc(locals.get_local_index(id, depth).unwrap());
            }

            StmtAstKind::Expr(expr) => {
                let is_assignemt = self.gen_expr(expr, locals, functions, depth);
                if !is_assignemt {
                    self.code.push(opcode::dbg);
                }
            }

            StmtAstKind::Block(stmts) => self.gen_block(stmts, locals, functions, depth),

            StmtAstKind::IfThen(guard, body) => {
                // if ...
                self.gen_expr(guard, locals, functions, depth);
                self.code.push(opcode::op_not);
                let cursor_from = self.gen_jmp_if_placeholder();

                // then ...
                self.gen_stmt(body.as_ref(), locals, functions, depth);

                // write saved jump address
                let cursor_to = self.code.len() as u32;
                self.replace_u32(cursor_from, cursor_to);
            }

            StmtAstKind::IfThenElse(guard, body_if, body_else) => {
                // if ... then
                self.gen_expr(guard, locals, functions, depth);
                self.code.push(opcode::op_not);
                let cursor_guard_end = self.gen_jmp_if_placeholder();

                // then ...
                self.gen_stmt(body_if, locals, functions, depth);
                let cursor_body_if_end = self.gen_jmp_placeholder();

                // else ...
                let cursor_body_else_start = self.code.len();
                self.gen_stmt(body_else, locals, functions, depth);
                let cursor_body_else_end = self.code.len();

                // write saved jump adresses
                self.replace_u32(cursor_body_if_end, cursor_body_else_end as u32);
                self.replace_u32(cursor_guard_end, cursor_body_else_start as u32);
            }

            StmtAstKind::WhileDo(guard, stmt) => {
                // while ...
                let cursor_guard_start = self.code.len();
                self.gen_expr(guard, locals, functions, depth);
                self.code.push(opcode::op_not);
                let cursor_guard_end = self.gen_jmp_if_placeholder();

                // do ...
                self.gen_stmt(stmt, locals, functions, depth);
                self.code.push(opcode::jmp);
                self.push_u32(cursor_guard_start as u32);

                // write saved jump address
                let cursor_body_end = self.code.len();
                self.replace_u32(cursor_guard_end, cursor_body_end as u32);
            }

            StmtAstKind::DoWhile(stmt, guard) => {
                // do ...
                let cursor_stmt_start = self.code.len();
                self.gen_stmt(stmt, locals, functions, depth);

                // while ...
                self.gen_expr(guard, locals, functions, depth);
                self.code.push(opcode::jmp_if);
                self.push_u32(cursor_stmt_start as u32);
            }

            StmtAstKind::VarDef(None, _) => unreachable!(),

            StmtAstKind::Return => self.code.push(opcode::ret),
            StmtAstKind::ReturnWith(expr) => {
                self.gen_expr(expr, locals, functions, depth);
                self.code.push(opcode::ret_val);
            }

            StmtAstKind::Func(_, _, _, _) => (),

            StmtAstKind::Then(_) => unreachable!(),
            StmtAstKind::Else(_) => unreachable!(),
            StmtAstKind::Do(_) => unreachable!(),
        }
    }

    fn gen_block(
        &mut self,
        stmts: &Vec<StmtAst>,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) {
        self.gen_stmt_list(stmts, locals, functions, depth + 1)
    }

    fn gen_expr(
        &mut self,
        expr: &ExprAst,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) -> bool {
        match &expr.kind {
            ExprAstKind::Number(num) => {
                self.code.push(opcode::ld_num_const);
                self.code.extend_from_slice(&num.to_le_bytes());
                false
            }
            ExprAstKind::Identifier(id) => {
                self.gen_ld_loc(locals.get_local_index(id, depth).expect("unknown variable"));
                false
            }
            ExprAstKind::Boolean(value) => {
                self.code.push(if *value {
                    opcode::ld_true_const
                } else {
                    opcode::ld_false_const
                });
                false
            }
            ExprAstKind::BinaryOp(BinaryOperator::Equal, assignee, value) => {
                let ExprAstKind::Identifier(ref id) = assignee.kind else {
                    panic!("assignee must be identifier");
                };

                self.gen_expr(value, locals, functions, depth);
                self.gen_st_loc(locals.get_local_index(id, depth).unwrap());
                true // signal an assignment
            }
            ExprAstKind::Parenthesized(inner) => self.gen_expr(inner, locals, functions, depth),
            ExprAstKind::BinaryOp(BinaryOperator::And, left, right) => {
                self.gen_short_circuit_and(left, right, locals, functions, depth);
                false
            }
            ExprAstKind::BinaryOp(BinaryOperator::Or, left, right) => {
                self.gen_short_circuit_or(left, right, locals, functions, depth);
                false
            }
            ExprAstKind::BinaryOp(op, left, right) => {
                self.gen_expr(left, locals, functions, depth);
                self.gen_expr(right, locals, functions, depth);

                let opcode = match op {
                    BinaryOperator::Plus => opcode::op_add,
                    BinaryOperator::Minus => opcode::op_sub,
                    BinaryOperator::Star => opcode::op_mul,
                    BinaryOperator::Slash => opcode::op_div,
                    BinaryOperator::Percent => opcode::op_mod,
                    BinaryOperator::EqualEqual => opcode::op_eq,
                    BinaryOperator::NotEqual => opcode::op_ne,
                    BinaryOperator::LessEqual => opcode::op_le,
                    BinaryOperator::LessThan => opcode::op_lt,
                    BinaryOperator::GreaterEqual => opcode::op_ge,
                    BinaryOperator::GreaterThan => opcode::op_gt,
                    BinaryOperator::Ampersand => opcode::or_and,
                    BinaryOperator::Caret => opcode::op_xor,
                    BinaryOperator::Bar => opcode::or_or,
                    BinaryOperator::And => unreachable!(),
                    BinaryOperator::Or => unreachable!(),
                    BinaryOperator::Equal => unreachable!(),
                };
                self.code.push(opcode);

                false
            }

            ExprAstKind::UnaryOp(op, expr) => {
                self.gen_expr(expr, locals, functions, depth);

                match op {
                    UnaryOperator::Pos => {} // does nothing
                    UnaryOperator::Neg => self.code.push(opcode::op_neg),
                    UnaryOperator::Not => self.code.push(opcode::op_not),
                }

                false
            }

            ExprAstKind::Call(name, params) => {
                let fp = *functions
                    .get(&(name.clone(), params.len() as u8))
                    .expect("unknown function");

                for param in params {
                    self.gen_expr(param, locals, functions, depth);
                }
                self.gen_call(fp);

                false
            }

            ExprAstKind::Bad => unreachable!(),
        }
    }

    fn gen_short_circuit_and(
        &mut self,
        left: &ExprAst,
        right: &ExprAst,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) {
        self.gen_expr(left, locals, functions, depth);
        self.code.push(opcode::dup);
        self.code.push(opcode::op_not);
        let cursor_a = self.gen_jmp_if_placeholder();

        self.gen_expr(right, locals, functions, depth);
        self.code.push(opcode::or_and);
        let cursor_b = self.code.len();

        self.replace_u32(cursor_a, cursor_b as u32);
    }

    fn gen_short_circuit_or(
        &mut self,
        left: &ExprAst,
        right: &ExprAst,
        locals: &LocalsInfo,
        functions: &HashMap<(String, u8), u32>,
        depth: u8,
    ) {
        self.gen_expr(left, locals, functions, depth);
        self.code.push(opcode::dup);
        let cursor_a = self.gen_jmp_if_placeholder();

        self.gen_expr(right, locals, functions, depth);
        self.code.push(opcode::or_or);
        let cursor_b = self.code.len();

        self.replace_u32(cursor_a, cursor_b as u32);
    }
}
