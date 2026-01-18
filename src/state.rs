use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::Type;

pub struct Environment {
    vars: HashMap<String, Type>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            vars: HashMap::new(),
            parent: None,
        }))
    }

    pub fn fork(parent: &Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            vars: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub fn load(&self, identifier: &str) -> Option<Type> {
        if let Some(val) = self.vars.get(identifier) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().load(identifier)
        } else {
            None
        }
    }

    // pub fn store(&mut self, identifier: String, value: Type) {
    //     self.vars.insert(identifier, value);
    // }

    /// Store in the scope where the variable is defined, or current if new
    pub fn assign(&mut self, identifier: String, value: Type) {
        if self.vars.contains_key(&identifier) {
            self.vars.insert(identifier, value);
        } else if let Some(parent) = &self.parent
            && parent.borrow().load(&identifier).is_some()
        {
            parent.borrow_mut().assign(identifier, value);
        } else {
            self.vars.insert(identifier, value);
        }
    }

    pub fn parent(&self) -> Option<Rc<RefCell<Self>>> {
        self.parent.clone()
    }
}
