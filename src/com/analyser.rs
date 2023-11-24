use super::{
    abt::{ExprAbt, StmtAbt},
    ast::{BinaryOperator, ExprAst, ProgramAst, StmtAst},
    diagnostics::{Diagnostics, self},
};

pub struct Analyser<'a> {
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Analyser<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Self { diagnostics }
    }

    pub fn analyse_program(self, ast: &ProgramAst) {
        for stmt in ast {
            self.analyse_statement(stmt);
        }
    }

    fn analyse_statement(&self, stmt: &StmtAst) -> StmtAbt {
        match stmt {
            StmtAst::Empty => StmtAbt::Empty,
            StmtAst::VarDef(_, _) => todo!(),
            StmtAst::Expr(expr) => StmtAbt::Expr(self.analyse_expression(expr)),
            StmtAst::Block(_) => todo!(),
            StmtAst::IfThen(_, _) => todo!(),
            StmtAst::Then(_) => todo!(),
            StmtAst::IfThenElse(_, _, _) => todo!(),
            StmtAst::Else(_) => todo!(),
            StmtAst::WhileDo(_, _) => todo!(),
            StmtAst::DoWhile(_, _) => todo!(),
            StmtAst::Do(_) => todo!(),
            StmtAst::Func(_, _, _, _) => todo!(),
            StmtAst::Return => todo!(),
            StmtAst::ReturnWith(_) => todo!(),
        }
    }

    fn analyse_expression(&self, expr: &ExprAst) -> ExprAbt {
        match expr {
            ExprAst::Bad => ExprAbt::Unknown,
            ExprAst::Number(num) => ExprAbt::Number(*num),
            ExprAst::Identifier(_) => todo!(),
            ExprAst::Boolean(b) => ExprAbt::Boolean(*b),
            ExprAst::BinaryOp(op, left, right) => self.analyse_binary_operation(*op, left, right),
            ExprAst::UnaryOp(_, _) => todo!(),
            ExprAst::Call(_, _) => todo!(),
        }
    }

    fn analyse_binary_operation(
        &self,
        op: BinaryOperator,
        left: &ExprAst,
        right: &ExprAst,
    ) -> ExprAbt {
        let _left = self.analyse_expression(left);
        let _right = self.analyse_expression(right);

        match op {
            BinaryOperator::Plus => todo!(),
            BinaryOperator::Minus => todo!(),
            BinaryOperator::Star => todo!(),
            BinaryOperator::Slash => todo!(),
            BinaryOperator::Percent => todo!(),
            BinaryOperator::EqualEqual => todo!(),
            BinaryOperator::NotEqual => todo!(),
            BinaryOperator::LessEqual => todo!(),
            BinaryOperator::LessThan => todo!(),
            BinaryOperator::GreaterEqual => todo!(),
            BinaryOperator::GreaterThan => todo!(),
            BinaryOperator::Ampersand => todo!(),
            BinaryOperator::Caret => todo!(),
            BinaryOperator::Bar => todo!(),
            BinaryOperator::And => todo!(),
            BinaryOperator::Or => todo!(),
            BinaryOperator::Equal => todo!(),
        }
    }
}
