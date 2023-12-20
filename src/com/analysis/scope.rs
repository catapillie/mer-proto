use std::collections::HashMap;

use super::Analyser;

pub struct Scope {
    parent: Option<Box<Scope>>,
    pub depth: u16,
    pub bindings: HashMap<String, u64>,
    pub current_func_id: Option<u64>,
}

impl Scope {
    pub fn root() -> Self {
        Scope {
            parent: None,
            depth: 0,
            bindings: Default::default(),
            current_func_id: None
        }
    }

    pub fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    fn create_sub_scope(&self) -> Self {
        Scope {
            parent: None,
            depth: self.depth + 1,
            bindings: Default::default(),
            current_func_id: self.current_func_id,
        }
    }

    pub fn search<F, T>(&self, lookup: F) -> Option<T>
    where
        F: Fn(&Self) -> Option<T>,
    {
        let mut scope = Some(self);
        while let Some(s) = scope {
            let t = lookup(s);
            if t.is_some() {
                return t;
            }

            scope = s.parent.as_deref();
        }

        None
    }
}

impl<'d> Analyser<'d> {
    pub fn open_scope(&mut self) {
        let child = self.scope.create_sub_scope();
        let parent = std::mem::replace(&mut self.scope, child);
        self.scope.parent = Some(Box::new(parent));
    }

    pub fn close_scope(&mut self) {
        self.scope = *std::mem::take(&mut self.scope.parent).expect("scope underflow");
    }
}
