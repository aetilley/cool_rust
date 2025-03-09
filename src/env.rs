use std::collections::hash_map::HashMap;

type Scope = HashMap<String, String>;

#[derive(Debug, PartialEq)]
pub struct Env {
    // Since `Vec::push` pushes to the back of the vector, we will
    // use the back as the "top" of the stack.
    stack: Vec<Scope>,
}
#[derive(Debug)]
struct EnvError {}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
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

    pub fn lookup(&self, key: &str) -> Option<String> {
        // Start search from top of stack.
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value.to_owned());
            }
        }
        None
    }

    pub fn add_binding(&mut self, key: &str, value: &str) {
        // NOTE:  Currently does not throw if the key already exists
        // in the current scope. Sometimes we may want to throw an error
        // E.g. if a symbol appears twice in a function param list.
        let top = self.stack.last_mut();
        match top {
            Some(frame) => {
                frame.insert(key.to_owned(), value.to_owned());
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

    #[test]
    pub fn scope_test() {
        let key1 = "key1".to_owned();
        let key2 = "key2".to_owned();
        let key3 = "key3".to_owned();
        let val1 = "val1".to_owned();
        let val2_1 = "val2_1".to_owned();
        let val2_2 = "val2_2".to_owned();
        let val3 = "val3".to_owned();

        let mut env = Env::new();

        env.add_binding(&key1, &val1);

        assert_eq!(env.lookup(&key1), Some(val1.clone()));

        env.enter_scope();
        env.add_binding(&key2, &val2_1);

        assert_eq!(env.lookup(&key1), Some(val1));
        assert_eq!(env.lookup(&key2), Some(val2_1.clone()));

        env.enter_scope();
        env.add_binding(&key2, &val2_2);
        env.add_binding(&key3, &val3);

        assert_eq!(env.lookup(&key2), Some(val2_2));
        assert_eq!(env.lookup(&key3), Some(val3));

        env.exit_scope();

        assert_eq!(env.lookup(&key2), Some(val2_1));
        assert_eq!(env.lookup(&key3), None);
    }
}
