use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::Type, model::FuncDecl};

#[derive(Clone)]
pub struct Function {
    declaration: FuncDecl,
    environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(declaration: FuncDecl, environment: Rc<RefCell<Environment>>) -> Self {
        Self {
            declaration,
            environment: environment.clone(),
        }
    }

    pub fn declaration(&self) -> &FuncDecl {
        &self.declaration
    }

    pub fn environment(&self) -> &Rc<RefCell<Environment>> {
        &self.environment
    }
}

pub struct Environment {
    vars: HashMap<Rc<String>, Type>,
    funcs: HashMap<Rc<String>, Function>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            parent: None,
        }))
    }

    pub fn fork(parent: &Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub fn parent(&self) -> Option<Rc<RefCell<Self>>> {
        self.parent.clone()
    }

    /// Attempt to load a variable first from the local environment
    /// , then from its parent.
    ///
    /// Return `None` if no variable is found with name `identifier`.
    pub fn load_var(&self, identifier: Rc<String>) -> Option<Type> {
        if let Some(val) = self.vars.get(&identifier.clone()) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().load_var(identifier)
        } else {
            None
        }
    }

    /// Store a variable in the scope where it is defined, or in the local scope if no
    /// previous definition is found.
    pub fn store_var(&mut self, identifier: &Rc<String>, value: Type) {
        if self.vars.contains_key(identifier) {
            self.vars.insert(identifier.clone(), value);
        } else if let Some(parent) = &self.parent
            && parent.borrow().load_var(identifier.clone()).is_some()
        {
            parent.borrow_mut().store_var(identifier, value);
        } else {
            self.vars.insert(identifier.clone(), value);
        }
    }

    /// Assign in the local scope.
    pub fn store_var_local(&mut self, identifier: &Rc<String>, value: Type) {
        self.vars.insert(identifier.clone(), value);
    }

    /// Attempt to load a variable first from the local environment
    /// , then from its parent.
    ///
    /// Return `None` if no variable is found with name `identifier`.
    pub fn load_func(&self, identifier: Rc<String>) -> Option<Function> {
        if let Some(val) = self.funcs.get(&identifier.clone()) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().load_func(identifier)
        } else {
            None
        }
    }

    /// Store a variable in the scope where it is defined, or in the local scope if no
    /// previous definition is found.
    pub fn store_func(&mut self, identifier: &Rc<String>, value: Function) {
        if self.vars.contains_key(identifier) {
            self.funcs.insert(identifier.clone(), value);
        } else if let Some(parent) = &self.parent
            && parent.borrow().load_func(identifier.clone()).is_some()
        {
            parent.borrow_mut().store_func(identifier, value);
        } else {
            self.funcs.insert(identifier.clone(), value);
        }
    }

    /// Assign in the local scope.
    pub fn store_func_local(&mut self, identifier: &Rc<String>, value: FuncDecl) {
        self.funcs.insert(identifier.clone(), value);
    }
}
