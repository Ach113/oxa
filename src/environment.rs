use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

use crate::tokens::{Token, Literal};

#[derive(Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>, // for global scope this field is None
    pub values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        let values: HashMap<String, Literal> = HashMap::new();
        Environment {enclosing, values}
    }

    pub fn add(&mut self, identifier: Token, value: Literal) -> Result<(), ()> {
        let name = &identifier.lexeme;
        if self.values.contains_key(name) {
            crate::error("NameError", &format!("Redefinition of variable '{}'", name), identifier.line);
            return Err(());
        }
        self.values.insert(identifier.lexeme, value);
        Ok(())
    }

    pub fn assign(&mut self, identifier: Token, value: Literal) -> Result<Literal, ()> {
        let name = &identifier.lexeme;
        if self.values.contains_key(name) {
            self.values.insert(identifier.lexeme, value.clone());
            return Ok(value);
        } else {
            match (&mut self.enclosing) {
                Some(env) => env.borrow_mut().assign(identifier.clone(), value.clone()),
                None => {
                    crate::error("NameError", &format!("Assignment to uninitialized variable '{}'", name), identifier.line);
                    Err(())
                },
            }
        }
    }

    pub fn get(&self, identifier: Token) -> Result<Literal, ()> {
        let name = &identifier.lexeme;
        let value = self.values.get(name);
        match value {
            Some(x) => Ok(x.clone()),
            None => {
                match &self.enclosing {
                    Some(env) => env.borrow().get(identifier.clone()),
                    None => {
                        crate::error("NameError", &format!("Undefined variable '{}'", name), identifier.line);
                        Err(())
                    }
                }
            }
        }
    }

    pub fn contains_key(&self, key: &String) -> bool {
        self.values.contains_key(key)
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.enclosing {
            None => write!(f, "{:?} | None", self.values),
            Some(env) => write!(f,"{:?} | ({})", self.values, env.borrow()),
        }
    }
}