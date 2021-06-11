use std::collections::HashMap;

use crate::tokens::{Token, Literal};

pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        let values: HashMap<String, Literal> = HashMap::new();
        Environment {values}
    }

    pub fn add(&mut self, identifier: Token, value: Literal) {
        self.values.insert(identifier.lexeme, value);
    }

    pub fn get(&self, identifier: Token) -> Result<Literal, ()> {
        let name = identifier.lexeme;
        let value = self.values.get(&name);
        match value {
            Some(x) => Ok(x.clone()),
            None => {
                crate::error("NameError", &format!("Undefined variable {}", name), identifier.line);
                Err(())
            }
        }
    }
}