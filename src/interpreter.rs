use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::Stmt;
use crate::environment::Environment;
use crate::AST::Literal;
use crate::tokens::{TokenType, Token};

pub fn interpret(statements: &Vec<Box<dyn Stmt>>, mut env: Rc<RefCell<Environment>>) -> Result<(), ()> {
    let mut error_count: u32 = 0;

    for stmt in statements {
        match stmt.eval(env.clone()) {
            Err(_) => error_count += 1,
            Ok(_) => {},
        };
    }
    if error_count > 0 {
        Err(())
    } else {
        Ok(())
    }
}