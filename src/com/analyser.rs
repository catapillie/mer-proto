use std::{collections::HashMap, mem};

use crate::com::abt::{BinaryOp, TypeAbt};

use super::{
    abt::{ExprAbt, StmtAbt, UnaryOp},
    ast::{BinaryOperator, ExprAst, ExprAstKind, ProgramAst, StmtAst, StmtAstKind, UnaryOperator},
    diagnostics::{self, DiagnosticKind, Diagnostics, Severity},
    span::Span,
};

struct Scope {
    parent: Option<Box<Scope>>,

    variables: HashMap<String, TypeAbt>,
    function: HashMap<String, (Vec<TypeAbt>, TypeAbt)>,

    unary_operations: HashMap<(UnaryOperator, TypeAbt), (UnaryOp, TypeAbt)>,
    binary_operations: HashMap<(BinaryOperator, TypeAbt, TypeAbt), (BinaryOp, TypeAbt)>,

    return_type: TypeAbt,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent: None,

            variables: Default::default(),
            function: Default::default(),

            unary_operations: Default::default(),
            binary_operations: Default::default(),

            return_type: TypeAbt::Unknown,
        }
    }
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

    pub fn declare_function(&mut self, name: String, signature: (Vec<TypeAbt>, TypeAbt)) {
        self.function.insert(name, signature);
    }

    pub fn get_function(&self, name: &str) -> Option<&(Vec<TypeAbt>, TypeAbt)> {
        match self.function.get(name) {
            Some(signature) => Some(signature),
            None => match self.parent {
                Some(ref parent) => parent.get_function(name),
                None => None,
            },
        }
    }

    pub fn declare_unary_operation(
        &mut self,
        key: (UnaryOperator, TypeAbt),
        value: (UnaryOp, TypeAbt),
    ) {
        self.unary_operations.insert(key, value);
    }

    pub fn get_unary_operation(
        &self,
        op: UnaryOperator,
        ty: &TypeAbt,
    ) -> Option<(UnaryOp, TypeAbt)> {
        match self
            .unary_operations
            .get(&(op, ty.clone()))
        {
            Some(res) => Some(res.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_unary_operation(op, ty),
                None => None,
            },
        }
    }

    pub fn declare_binary_operation(
        &mut self,
        key: (BinaryOperator, TypeAbt, TypeAbt),
        value: (BinaryOp, TypeAbt),
    ) {
        self.binary_operations.insert(key, value);
    }

    pub fn get_binary_operation(
        &self,
        op: BinaryOperator,
        left: &TypeAbt,
        right: &TypeAbt,
    ) -> Option<(BinaryOp, TypeAbt)> {
        match self
            .binary_operations
            .get(&(op, left.clone(), right.clone()))
        {
            Some(res) => Some(res.clone()),
            None => match self.parent {
                Some(ref parent) => parent.get_binary_operation(op, left, right),
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
    #[rustfmt::skip]
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        let mut scope = Scope::default();

        use TypeAbt as Ty;
        
        use BinaryOperator as BI;
        use BinaryOp as BO;
        
        // number <op> number -> number
        scope.declare_binary_operation((BI::Plus, Ty::Number, Ty::Number), (BO::Plus, Ty::Number));
        scope.declare_binary_operation((BI::Minus, Ty::Number, Ty::Number), (BO::Minus, Ty::Number));
        scope.declare_binary_operation((BI::Star, Ty::Number, Ty::Number), (BO::Star, Ty::Number));
        scope.declare_binary_operation((BI::Slash, Ty::Number, Ty::Number), (BO::Slash, Ty::Number));
        scope.declare_binary_operation((BI::Percent, Ty::Number, Ty::Number), (BO::Percent, Ty::Number));

        // number <op> number -> boolean
        scope.declare_binary_operation((BI::EqualEqual, Ty::Number, Ty::Number), (BO::EqualEqual, Ty::Boolean));
        scope.declare_binary_operation((BI::NotEqual, Ty::Number, Ty::Number), (BO::NotEqual, Ty::Boolean));
        scope.declare_binary_operation((BI::LessEqual, Ty::Number, Ty::Number), (BO::LessEqual, Ty::Boolean));
        scope.declare_binary_operation((BI::LessThan, Ty::Number, Ty::Number), (BO::LessThan, Ty::Boolean));
        scope.declare_binary_operation((BI::GreaterEqual, Ty::Number, Ty::Number), (BO::GreaterEqual, Ty::Boolean));
        scope.declare_binary_operation((BI::GreaterThan, Ty::Number, Ty::Number), (BO::GreaterThan, Ty::Boolean));

        // boolean <op> boolean -> boolean
        scope.declare_binary_operation((BI::Ampersand, Ty::Boolean, Ty::Boolean), (BO::Ampersand, Ty::Boolean));
        scope.declare_binary_operation((BI::Caret, Ty::Boolean, Ty::Boolean), (BO::Caret, Ty::Boolean));
        scope.declare_binary_operation((BI::Bar, Ty::Boolean, Ty::Boolean), (BO::Bar, Ty::Boolean));
        scope.declare_binary_operation((BI::And, Ty::Boolean, Ty::Boolean), (BO::And, Ty::Boolean));
        scope.declare_binary_operation((BI::Or, Ty::Boolean, Ty::Boolean), (BO::Or, Ty::Boolean));
        scope.declare_binary_operation((BI::EqualEqual, Ty::Boolean, Ty::Boolean), (BO::EqualEqual, Ty::Boolean));
        scope.declare_binary_operation((BI::NotEqual, Ty::Boolean, Ty::Boolean), (BO::NotEqual, Ty::Boolean));

        use UnaryOperator as UI;
        use UnaryOp as UO;

        // <op> number -> number
        scope.declare_unary_operation((UI::Pos, Ty::Number), (UO::Pos, Ty::Number));
        scope.declare_unary_operation((UI::Neg, Ty::Number), (UO::Neg, Ty::Number));

        // <op> boolean -> boolean
        scope.declare_unary_operation((UI::Not, Ty::Boolean), (UO::Not, Ty::Boolean));

        // the main scope must return unit
        scope.return_type = TypeAbt::Unit;

        Self { diagnostics, scope }
    }

    fn open_scope(&mut self) {
        let parent = mem::take(&mut self.scope);
        let return_type = parent.return_type.clone();
        self.scope.parent = Some(Box::new(parent));
        self.scope.return_type = return_type;
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
            StmtAstKind::Func(_, _, _, _)
                => todo!(),
            StmtAstKind::Return
                => self.analyse_return_statement(stmt.span),
            StmtAstKind::ReturnWith(expr)
                => self.analyse_return_with_statement(expr),
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

    fn analyse_return_statement(&mut self, span: Span) -> StmtAbt {
        let ty = TypeAbt::Unit;

        if !ty.is(&self.scope.return_type) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MustReturnValue {
                    expected: self.scope.return_type.clone()
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return StmtAbt::Empty
        }

        StmtAbt::Return(Box::new(ExprAbt::Unit))
    }

    fn analyse_return_with_statement(&mut self, expr: &ExprAst) -> StmtAbt {
        let bound_expr = self.analyse_expression(expr);
        let ty_expr = bound_expr.ty();

        if !ty_expr.is(&self.scope.return_type) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: ty_expr,
                    expected: self.scope.return_type.clone()
                })
                .with_severity(Severity::Error)
                .with_span(expr.span)
                .done();
            self.diagnostics.push(d);
            return StmtAbt::Empty;
        }

        StmtAbt::Return(Box::new(bound_expr))
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
            ExprAstKind::UnaryOp(op, operand)
                => self.analyse_unary_operation(*op, operand, expr.span),
            ExprAstKind::Call(name, params)
                => self.analyse_call(name, params, expr.span),
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
        if matches!(op, BinaryOperator::Equal) {
            return self.analyse_assignment(left, right);
        }

        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ty_left = bound_left.ty();
        let ty_right = bound_right.ty();

        if !ty_left.is_known() || !ty_right.is_known() {
            return ExprAbt::Unknown;
        }

        let Some(bin_op) = self.scope.get_binary_operation(op, &ty_left, &ty_right) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidBinaryOperation {
                    op,
                    left: ty_left,
                    right: ty_right,
                })
                .with_severity(Severity::Error)
                .with_span(right.span.join(left.span))
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        ExprAbt::Binary(bin_op, Box::new(bound_left), Box::new(bound_right))
    }

    fn analyse_assignment(&mut self, left: &ExprAst, right: &ExprAst) -> ExprAbt {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

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
                    expected: ty_left,
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::Assignment(name, ty_left, Box::new(bound_right))
    }

    fn analyse_unary_operation(&mut self, op: UnaryOperator, operand: &ExprAst, span: Span) -> ExprAbt {
        let bound_operand = self.analyse_expression(operand);
        let ty = bound_operand.ty();

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        let Some(un_op) = self.scope.get_unary_operation(op, &ty) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidUnaryOperation {
                    op,
                    ty,
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        ExprAbt::Unary(un_op, Box::new(bound_operand))
    }

    fn analyse_call(&mut self, name: &str, params: &[ExprAst], span: Span) -> ExprAbt {
        let bound_params = params.iter().map(|param| self.analyse_expression(param)).collect::<Vec<_>>();

        let Some((expected_params, ty)) = self.scope.get_function(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFunction(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown
        };

        let mut invalid = false;
        for ((bound_param, param), expected_ty) in bound_params.iter().zip(params).zip(expected_params) {
            let ty_param = bound_param.ty();
            if !ty_param.is(expected_ty) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: ty_param.clone(),
                        expected: expected_ty.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(param.span)
                    .done();
                self.diagnostics.push(d);
                invalid = true;
            }
        }

        if bound_params.len() != expected_params.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidParameterCount {
                    got: bound_params.len(), expected: bound_params.len()
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            ExprAbt::Unknown
        } else {
            ExprAbt::Call(name.to_string(), bound_params, ty.clone())
        }
    }
}
