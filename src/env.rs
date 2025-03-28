use std::collections::hash_map::HashMap;

use crate::symbol::Sym;

type Scope<Val> = HashMap<Sym, Val>;

#[derive(Debug, Clone, PartialEq)]
pub struct Env<Val: Clone> {
    // A stack of local environments to keep track of what variables are in scope.
    // Since `Vec::push` pushes to the back of the vector, we will
    // use the back as the "top" of the stack.
    stack: Vec<Scope<Val>>,
}

impl<Val: Clone> Default for Env<Val> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Val: Clone> Env<Val> {
    pub fn new() -> Self {
        Env { stack: vec![] }
    }
    pub fn enter_scope(&mut self) {
        let top = Scope::new();
        self.stack.push(top);
    }
    pub fn exit_scope(&mut self) {
        self.stack.pop();
    }

    pub fn lookup(&self, key: &Sym) -> Option<Val> {
        // Start search from top of stack.
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value.to_owned());
            }
        }
        None
    }

    pub fn add_binding(&mut self, key: &Sym, value: &Val) {
        // NOTE:  Currently does not Err if the key already exists
        // in the current scope. Sometimes we may want to throw an error
        // E.g. if a symbol appears twice in a function param list.
        let top = self.stack.last_mut();
        match top {
            Some(frame) => {
                frame.insert(key.clone(), value.clone());
            }
            None => {
                self.enter_scope();
                self.add_binding(key, value);
            }
        }
    }
}

#[cfg(test)]
mod env_tests {

    use super::*;
    use crate::symbol::sym;

    #[test]
    pub fn scope_test() {
        let key1 = &sym("key1");
        let key2 = &sym("key2");
        let key3 = &sym("key3");
        let val1 = &sym("val1");
        let val2_1 = &sym("val2_1");
        let val2_2 = &sym("val2_2");
        let val3 = &sym("val3");

        let mut env = Env::new();

        env.add_binding(key1, val1);

        assert_eq!(env.lookup(key1), Some(val1.to_owned()));

        env.enter_scope();
        env.add_binding(key2, val2_1);

        assert_eq!(env.lookup(key1), Some(val1.to_owned()));
        assert_eq!(env.lookup(key2), Some(val2_1.to_owned()));

        env.enter_scope();
        env.add_binding(key2, val2_2);
        env.add_binding(key3, val3);

        assert_eq!(env.lookup(key2), Some(val2_2.to_owned()));
        assert_eq!(env.lookup(key3), Some(val3.to_owned()));

        env.exit_scope();

        assert_eq!(env.lookup(key2), Some(val2_1.to_owned()));
        assert_eq!(env.lookup(key3), None);
    }
}
