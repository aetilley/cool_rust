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
