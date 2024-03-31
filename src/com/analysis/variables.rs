use super::{Analyser, Declaration};
use crate::{
    com::{
        abt::{self, VariableInfo},
        ast::stmt::VarDef,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn declare_variable_here(&mut self, name: Spanned<String>, ty: abt::Type) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.value.clone(), declared);

        let info = VariableInfo {
            id: declared,
            name,
            depth: self.scope.depth,
            position: self.scope.position,
            ty,
            is_on_heap: false,
        };
        let prev = self.program.variables.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        let func_id = self.scope.current_func_id;
        let func_info = self.program.functions.get_mut(&func_id).unwrap();
        func_info.local_variables.insert(declared);

        Declaration { declared, shadowed }
    }

    pub fn analyse_variable_definition(&mut self, ast: &VarDef) -> abt::StmtKind {
        let bound_expr = self.analyse_expression(&ast.expr);
        let bound_ty = self.program.type_of(&bound_expr);
        let pat = self.analyse_pattern(&ast.pattern);
        let bound_pat = self.declare_pattern_bindings(&pat, &bound_ty);
        abt::StmtKind::Deconstruct(Box::new(bound_pat), Box::new(bound_expr))
    }

    pub fn analyse_variable_expression(&mut self, name: &str, span: Span) -> abt::Expr {
        let Some(id) = self.scope.search_id(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownVariable(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        if let Some(info) = self.program.aliases.get(&id) {
            if !info.is_opaque {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::NonOpaqueTypeConstructor(name.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_secondary(
                        Note::MarkedAsOpaque(name.to_string()).dddot_back().num(1),
                        info.name.span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::DoesNotHaveConstructor(name.to_string())
                            .so()
                            .dddot_front()
                            .num(2),
                        span,
                    )
                    .done();
                self.diagnostics.push(d);
                return abt::Expr::Unknown;
            }
            let ctor_id = self.get_opaque_constructor_func_id(id);
            return abt::Expr::OpaqueConstructor { ctor_id, alias_id: id };
        }

        if self.program.functions.contains_key(&id) {
            let current_func = self
                .program
                .functions
                .get_mut(&self.scope.current_func_id)
                .unwrap();
            current_func.called_functions.insert(id);
            return abt::Expr::Function(id);
        }

        let Some(info) = self.program.variables.get_mut(&id) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NotVariable(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        let depth = info.depth;
        let func_id = self.scope.current_func_id;
        let func_info = self.program.functions.get_mut(&func_id).unwrap();
        let captured = depth <= func_info.depth;

        if captured {
            func_info.captured_variables.insert(id);
            info.is_on_heap = true;
        }

        // if captured && !alread_captured {
        //     let func_name = func_info.name.value.clone();
        //     let func_span = func_info
        //         .name
        //         .span
        //         .expect("declared functions have a name span");
        //     let var_name = name.to_string();
        //     let d = diagnostics::create_diagnostic()
        //         .with_kind(DiagnosticKind::UnallowedVariableCapture {
        //             func_name,
        //             var_name,
        //         })
        //         .with_severity(Severity::Error)
        //         .with_span(span)
        //         .annotate_primary(
        //             Note::VariableCapturedBy(name.to_string(), func_info.name.value.to_string())
        //                 .then()
        //                 .dddot_front()
        //                 .num(2),
        //             span,
        //         )
        //         .highlight(func_span)
        //         .annotate_secondary(
        //             Note::VariableDeclaration(name.to_string())
        //                 .dddot_back()
        //                 .num(1),
        //             declaration_span,
        //             NoteSeverity::Annotation,
        //         )
        //         .done();
        //     self.diagnostics.push(d);
        // }

        abt::Expr::Variable(id)
    }
}
