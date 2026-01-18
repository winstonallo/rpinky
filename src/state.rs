use std::collections::HashMap;

use crate::interpreter::Type;

pub struct Environment {
    vars: HashMap<String, Type>,
}

impl Environment {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn load(&self, identifier: String) -> Option<&Type> {
        self.vars.get(&identifier)
    }

    pub fn store(&mut self, identifier: String, value: Type) {
        self.vars.insert(identifier, value);
    }
}
