use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

use crate::tokens::Token;
use crate::Object;
use crate::AST::Error;

#[derive(Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>, // for global scope this field is None
    pub symbol_table: HashMap<String, Object>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        let symbol_table: HashMap<String, Object> = HashMap::new();
        Environment {enclosing, symbol_table}
    }

    pub fn add(&mut self, identifier: Token, value: Object) -> Result<(), Error> {
        let name = &identifier.lexeme;
        self.symbol_table.insert(identifier.lexeme, value);
        Ok(())
    }

    pub fn assign(&mut self, identifier: Token, value: Object) -> Result<Object, Error> {
        let name = &identifier.lexeme;
        if self.symbol_table.contains_key(name) {
            self.symbol_table.insert(identifier.lexeme, value.clone());
            return Ok(value);
        } else {
            match &mut self.enclosing {
                Some(env) => env.borrow_mut().assign(identifier.clone(), value.clone()),
                None => {
                    crate::error("NameError", &format!("Assignment to uninitialized variable '{}'", name), identifier.line);
                    Err(Error::STRING(format!("NameError: assignment to uninitialized variable '{}'", name).into()))
                },
            }
        }
    }

    pub fn get(&self, identifier: Token) -> Result<Object, Error> {
        let name = &identifier.lexeme;
        let value = self.symbol_table.get(name);
        match value {
            Some(x) => Ok(x.clone()),
            None => {
                match &self.enclosing {
                    Some(env) => env.borrow().get(identifier.clone()),
                    None => {
                        crate::error("NameError", &format!("Undefined variable '{}'", name), identifier.line);
                        Err(Error::STRING(format!("NameError: undefined variable '{}'", name).into()))
                    }
                }
            }
        }
    }
}