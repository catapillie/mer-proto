use super::{
    abt::{ExprAbt, StmtAbt},
    ast::{BinaryOperator, ExprAstKind, ProgramAst, StmtAstKind, ExprAst, StmtAst},
    diagnostics::Diagnostics,
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
        match &stmt.kind {
            StmtAstKind::Empty => StmtAbt::Empty,
            StmtAstKind::VarDef(_, _) => todo!(),
            StmtAstKind::Expr(expr) => StmtAbt::Expr(self.analyse_expression(expr)),
            StmtAstKind::Block(_) => todo!(),
            StmtAstKind::IfThen(_, _) => todo!(),
            StmtAstKind::Then(_) => todo!(),
            StmtAstKind::IfThenElse(_, _, _) => todo!(),
            StmtAstKind::Else(_) => todo!(),
            StmtAstKind::WhileDo(_, _) => todo!(),
            StmtAstKind::DoWhile(_, _) => todo!(),
            StmtAstKind::Do(_) => todo!(),
            StmtAstKind::Func(_, _, _, _) => todo!(),
            StmtAstKind::Return => todo!(),
            StmtAstKind::ReturnWith(_) => todo!(),
        }
    }

    fn analyse_expression(&self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad => ExprAbt::Unknown,
            ExprAstKind::Number(num) => ExprAbt::Number(*num),
            ExprAstKind::Identifier(_) => todo!(),
            ExprAstKind::Boolean(b) => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(_) => todo!(),
            ExprAstKind::BinaryOp(op, left, right) => self.analyse_binary_operation(*op, left, right),
            ExprAstKind::UnaryOp(_, _) => todo!(),
            ExprAstKind::Call(_, _) => todo!(),
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
