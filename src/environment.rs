use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::tokens::Token;
use crate::types::{Type, NativeFunction, NativeClass};
use crate::AST::Error;

fn add_natives(env: &mut Environment) {
    env.symbol_table.insert("input".to_string(), Type::NATIVE(NativeFunction::INPUT));
    env.symbol_table.insert("read".to_string(), Type::NATIVE(NativeFunction::READ));
    env.symbol_table.insert("write".to_string(), Type::NATIVE(NativeFunction::WRITE));
    env.symbol_table.insert("time".to_string(), Type::NATIVE(NativeFunction::TIME));
    env.symbol_table.insert("list".to_string(), Type::NATIVEC(NativeClass::LIST));
    //env.symbol_table.insert("dict".to_string(), Type::NATIVEC(NativeClass::DICT));
}

#[derive(Debug)]
pub struct Environment {
    pub enclosing: Option<Rc<RefCell<Environment>>>, // for global scope this field is None
    pub symbol_table: HashMap<String, Type>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        let symbol_table: HashMap<String, Type> = HashMap::new();
        if enclosing.is_none() {
            let mut env = Environment {enclosing, symbol_table};
            add_natives(&mut env);
            return env;
        }
        Environment {enclosing, symbol_table}
    }

    pub fn add(&mut self, identifier: Token, value: Type) -> Result<(), Error> {
        self.symbol_table.insert(identifier.lexeme, value);
        Ok(())
    }

    pub fn assign(&mut self, identifier: Token, value: Type) -> Result<Type, Error> {
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

    pub fn get(&self, identifier: Token) -> Result<Type, Error> {
        let name = &identifier.lexeme;
        match self.symbol_table.get(name) {
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