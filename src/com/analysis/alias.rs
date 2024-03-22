use super::Analyser;
use crate::{
    com::{
        abt::{self, AliasInfo},
        ast::stmt::AliasDef,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
};

impl<'d> Analyser<'d> {
    pub fn analyse_alias_header(&mut self, ast: &AliasDef) -> Option<u64> {
        if let Some(shadowed) = self.scope.bindings.get(&ast.name.value) {
            if let Some(info) = self.program.aliases.get(shadowed) {
                let shadowed_span = info.name.span;
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::AliasRedefinition(ast.name.value.clone()))
                    .with_span(ast.name.span)
                    .with_severity(Severity::Error)
                    .annotate_secondary(
                        Note::ShadowedAlias(ast.name.value.clone())
                            .dddot_back()
                            .num(1),
                        shadowed_span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::RedefinedAlias.and().dddot_front().num(2),
                        ast.name.span,
                    )
                    .done();
                self.diagnostics.push(d);
                return None;
            }
        }

        let id = self.make_unique_id();
        self.scope.bindings.insert(ast.name.value.clone(), id);
        self.program.aliases.insert(
            id,
            AliasInfo {
                name: ast.name.clone(),
                ty: abt::Type::Unknown,
            },
        );

        Some(id)
    }

    pub fn analyse_alias_definition(&mut self, ast: &AliasDef, id: u64) {
        let ty = self.analyse_type(&ast.ty);
        self.program.aliases.get_mut(&id).unwrap().ty = ty;
    }
}
