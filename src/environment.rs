use std::collections::HashMap;

use crate::tokens::{Token, Literal};

pub struct Environment {
    enclosing: Option<Box<Environment>>, // for global scope this field is None
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
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
            self.values.insert(name.to_string(), value.clone());
            return Ok(value)
        } else {
            match (&mut self.enclosing) {
                Some(env) => env.assign(identifier.clone(), value.clone()),
                None => Err(()),
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
                    Some(env) => env.get(identifier.clone()),
                    None => {
                        crate::error("NameError", &format!("Undefined variable '{}'", name), identifier.line);
                        Err(())
                    }
                }
            }
        }
    }

    pub fn contains_key(&self, key: &Token) -> bool {
        self.values.contains_key(&key.lexeme)
    }
}