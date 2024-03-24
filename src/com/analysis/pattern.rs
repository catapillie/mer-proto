use super::Analyser;
use crate::{
    com::{abt, ast},
    utils::Spanned,
};

impl<'d> Analyser<'d> {
    pub fn analyse_pattern(&mut self, pattern: &ast::Pattern) -> abt::Pattern {
        match &pattern.value {
            ast::PatternKind::Bad => abt::PatternKind::Discard,
            ast::PatternKind::Discard => abt::PatternKind::Discard,
            ast::PatternKind::Binding(name) => abt::PatternKind::Binding(name.clone()),
        }
        .wrap(pattern.span)
    }

    pub fn declare_pattern_bindings(&mut self, pattern: &abt::Pattern, ty: &abt::Type) {
        use abt::PatternKind as Pat;
        match (&pattern.value, ty) {
            (Pat::Discard, _) => (),
            (Pat::Binding(name), ty) => {
                self.declare_variable_here(
                    Spanned {
                        span: pattern.span,
                        value: name.clone(),
                    },
                    ty.clone(),
                );
            }
        }
    }
}
