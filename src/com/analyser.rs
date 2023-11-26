use std::{collections::HashMap, mem};

use crate::com::abt::TypeAbt;

use super::{
    abt::{ExprAbt, StmtAbt},
    ast::{BinaryOperator, ExprAst, ExprAstKind, ProgramAst, StmtAst, StmtAstKind},
    diagnostics::{self, DiagnosticKind, Diagnostics, Severity},
    span::Span,
};

#[derive(Default)]
struct Scope {
    parent: Option<Box<Scope>>,

    variables: HashMap<String, TypeAbt>,
}

impl Scope {
    pub fn declare_variable(&mut self, name: String, ty: TypeAbt) {
        self.variables.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<TypeAbt> {
        match self.variables.get(name) {
            Some(ty) => Some(ty.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_variable(name),
                None => None,
            },
        }
    }
}

pub struct Analyser<'a> {
    diagnostics: &'a mut Diagnostics,
    scope: Scope,
}

impl<'a> Analyser<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Self {
            diagnostics,
            scope: Scope::default(),
        }
    }

    fn open_scope(&mut self) {
        let parent = mem::take(&mut self.scope);
        self.scope.parent = Some(Box::new(parent));
    }

    fn close_scope(&mut self) {
        let parent = mem::take(&mut self.scope.parent);
        self.scope = *parent.expect("attempted to close non-existing scope");
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
            StmtAstKind::VarDef(name, expr) => self.analyse_variable_definition(name, expr),
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

    fn analyse_variable_definition(&mut self, name: &Option<String>, expr: &ExprAst) -> StmtAbt {
        let bound_expr = self.analyse_expression(expr);

        let Some(name) = name else {
            return StmtAbt::Empty;
        };

        self.scope.declare_variable(name.clone(), bound_expr.ty());
        StmtAbt::VarDef(name.clone(), bound_expr)
    }

    fn analyse_block_statement(&mut self, stmts: &[StmtAst]) -> StmtAbt {
        self.open_scope();
        let bound_stmts = stmts
            .iter()
            .map(|s| self.analyse_statement(s))
            .collect::<Vec<_>>();
        self.close_scope();

        match bound_stmts.first() {
            None => StmtAbt::Empty,
            Some(StmtAbt::Empty) => StmtAbt::Empty,
            _ => StmtAbt::Block(bound_stmts),
        }
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

        if !bound_guard.ty().is(&TypeAbt::Boolean) {
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

        if !bound_guard.ty().is(&TypeAbt::Boolean) {
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

        if !bound_guard.ty().is(&TypeAbt::Boolean) {
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

        if !bound_guard.ty().is(&TypeAbt::Boolean) {
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

    #[rustfmt::skip]
    fn analyse_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad
                => ExprAbt::Unknown,
            ExprAstKind::Number(num)
                => ExprAbt::Number(*num),
            ExprAstKind::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            ExprAstKind::Boolean(b)
                => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right),
            ExprAstKind::UnaryOp(_, _)
                => todo!(),
            ExprAstKind::Call(_, _)
                => todo!(),
        }
    }

    fn analyse_variable_expression(&mut self, id: &str, span: Span) -> ExprAbt {
        match self.scope.get_variable(id) {
            Some(ty) => ExprAbt::Variable(id.to_string(), ty),
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownVariable(id.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }

    fn analyse_binary_operation(
        &mut self,
        op: BinaryOperator,
        left: &ExprAst,
        right: &ExprAst,
    ) -> ExprAbt {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        if !bound_left.ty().is_known() || !bound_right.ty().is_known() {
            return ExprAbt::Unknown;
        }

        match op {
            BinaryOperator::Equal => self.analyse_assignment(left, bound_left, right, bound_right),

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
        }
    }

    fn analyse_assignment(
        &mut self,
        left: &ExprAst,
        bound_left: ExprAbt,
        right: &ExprAst,
        bound_right: ExprAbt,
    ) -> ExprAbt {
        let ExprAbt::Variable(name, ty_left) = bound_left else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        if !bound_right.ty().is(&ty_left) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: bound_right.ty(),
                    expected: ty_left
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::Assignment(name, ty_left, Box::new(bound_right))
    }
}
