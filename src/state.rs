use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::Type;

pub struct Environment {
    vars: HashMap<Rc<String>, Type>,
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

    pub fn load(&self, identifier: Rc<String>) -> Option<Type> {
        if let Some(val) = self.vars.get(&identifier.clone()) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().load(identifier)
        } else {
            None
        }
    }

    /// Store in the scope where the variable is defined, or current if new
    pub fn assign(&mut self, identifier: &Rc<String>, value: Type) {
        if self.vars.contains_key(identifier) {
            self.vars.insert(identifier.clone(), value);
        } else if let Some(parent) = &self.parent
            && parent.borrow().load(identifier.clone()).is_some()
        {
            parent.borrow_mut().assign(identifier, value);
        } else {
            self.vars.insert(identifier.clone(), value);
        }
    }

    /// Assign in the local scope
    pub fn assign_local(&mut self, identifier: &Rc<String>, value: Type) {
        self.vars.insert(identifier.clone(), value);
    }

    pub fn parent(&self) -> Option<Rc<RefCell<Self>>> {
        self.parent.clone()
    }
}
