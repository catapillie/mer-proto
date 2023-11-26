use crate::com::abt::TypeAbt;

use super::{
    abt::{ExprAbt, StmtAbt},
    ast::{BinaryOperator, ExprAst, ExprAstKind, ProgramAst, StmtAst, StmtAstKind},
    diagnostics::{Diagnostics, self, DiagnosticKind, Severity},
};

pub struct Analyser<'a> {
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Analyser<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Self { diagnostics }
    }

    pub fn analyse_program(mut self, ast: &ProgramAst) {
        for stmt in ast {
            self.analyse_statement(stmt);
        }
    }

    fn analyse_statement(&mut self, stmt: &StmtAst) -> StmtAbt {
        match &stmt.kind {
            StmtAstKind::Empty => StmtAbt::Empty,
            StmtAstKind::VarDef(_, _) => todo!(),
            StmtAstKind::Expr(expr) => StmtAbt::Expr(Box::new(self.analyse_expression(expr))),
            StmtAstKind::Block(_) => todo!(),
            StmtAstKind::IfThen(guard, body_then) => self.analyse_if_then_statement(guard, body_then),
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
            ExprAstKind::BinaryOp(op, left, right) => {
                self.analyse_binary_operation(*op, left, right)
            }
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

    fn analyse_if_then_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbt {
        let bound_guard = self.analyse_expression(guard);
        let bound_body = self.analyse_statement(body);

        println!("{}", guard.span);
        println!("{}", body.span);
        if matches!(bound_body, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyIfThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !matches!(bound_guard.ty(), TypeAbt::Boolean) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbt::IfThen(Box::new(bound_guard), Box::new(bound_body))
    }
}
