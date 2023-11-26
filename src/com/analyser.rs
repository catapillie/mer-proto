use crate::com::abt::TypeAbt;

use super::{
    abt::{ExprAbt, StmtAbt},
    ast::{BinaryOperator, ExprAst, ExprAstKind, ProgramAst, StmtAst, StmtAstKind},
    diagnostics::{self, DiagnosticKind, Diagnostics, Severity},
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

    #[rustfmt::skip]
    fn analyse_statement(&mut self, stmt: &StmtAst) -> StmtAbt {
        match &stmt.kind {
            StmtAstKind::Empty => StmtAbt::Empty,
            StmtAstKind::VarDef(_, _) => todo!(),
            StmtAstKind::Expr(expr)
                => StmtAbt::Expr(Box::new(self.analyse_expression(expr))),
            StmtAstKind::Block(stmts)
                => self.analyse_block_statement(stmts),
            StmtAstKind::IfThen(guard, body_then)
                => self.analyse_if_then_statement(guard, body_then),
            StmtAstKind::Then(body_then)
                => self.analyse_then_statement(body_then),
            StmtAstKind::IfThenElse(guard, body_then, body_else)
                => self.analyse_if_then_else_statement(guard, body_then, body_else),
            StmtAstKind::Else(body_else)
                => self.analyse_else_statement(body_else),
            StmtAstKind::WhileDo(guard, body)
                => self.analyse_while_do_statement(guard, body),
            StmtAstKind::DoWhile(body, guard)
                => self.analyse_do_while_statement(body, guard),
            StmtAstKind::Do(body)
                => self.analyse_do_statement(body),
            StmtAstKind::Func(_, _, _, _) => todo!(),
            StmtAstKind::Return => todo!(),
            StmtAstKind::ReturnWith(_) => todo!(),
        }
    }

    fn analyse_block_statement(&mut self, stmts: &Vec<StmtAst>) -> StmtAbt {
        if stmts.is_empty() {
            return StmtAbt::Empty;
        }

        StmtAbt::Block(stmts.iter().map(|s| self.analyse_statement(s)).collect())
    }

    fn analyse_if_then_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbt {
        let bound_guard = self.analyse_expression(guard);
        let bound_body = self.analyse_statement(body);

        if matches!(bound_body, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
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

    fn analyse_if_then_else_statement(
        &mut self,
        guard: &ExprAst,
        body_then: &StmtAst,
        body_else: &StmtAst,
    ) -> StmtAbt {
        let bound_guard = self.analyse_expression(guard);
        let bound_body_then = self.analyse_statement(body_then);
        let bound_body_else = self.analyse_statement(body_else);

        if matches!(bound_body_then, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body_then.span)
                .done();
            self.diagnostics.push(d);
        }

        if matches!(bound_body_else, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyElseStatement)
                .with_severity(Severity::Warning)
                .with_span(body_else.span)
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

        StmtAbt::IfThenElse(
            Box::new(bound_guard),
            Box::new(bound_body_then),
            Box::new(bound_body_else),
        )
    }

    fn analyse_then_statement(&mut self, body_then: &StmtAst) -> StmtAbt {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ThenWithoutIf)
            .with_severity(Severity::Error)
            .with_span(body_then.span)
            .done();
        self.diagnostics.push(d);
        StmtAbt::Empty
    }

    fn analyse_else_statement(&mut self, body_else: &StmtAst) -> StmtAbt {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ElseWithoutIfThen)
            .with_severity(Severity::Error)
            .with_span(body_else.span)
            .done();
        self.diagnostics.push(d);
        StmtAbt::Empty
    }

    fn analyse_while_do_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbt {
        let bound_guard = self.analyse_expression(guard);
        let bound_body = self.analyse_statement(body);

        if matches!(bound_body, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyWhileDoStatement)
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

        StmtAbt::WhileDo(Box::new(bound_guard), Box::new(bound_body))
    }

    fn analyse_do_while_statement(&mut self, body: &StmtAst, guard: &ExprAst) -> StmtAbt {
        let bound_body = self.analyse_statement(body);
        let bound_guard = self.analyse_expression(guard);

        if matches!(bound_body, StmtAbt::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyDoWhileStatement)
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

        StmtAbt::DoWhile(Box::new(bound_body), Box::new(bound_guard))
    }

    fn analyse_do_statement(&mut self, body: &StmtAst) -> StmtAbt {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::DoWithoutWhile)
            .with_severity(Severity::Error)
            .with_span(body.span)
            .done();
        self.diagnostics.push(d);
        StmtAbt::Empty
    }

    fn analyse_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad => ExprAbt::Unknown,
            ExprAstKind::Number(num) => ExprAbt::Number(*num),
            ExprAstKind::Identifier(_) => todo!(),
            ExprAstKind::Boolean(b) => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right),
            ExprAstKind::UnaryOp(_, _) => todo!(),
            ExprAstKind::Call(_, _) => todo!(),
        }
    }

    fn analyse_binary_operation(
        &mut self,
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
