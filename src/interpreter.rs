use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::{Eval, Error};
use crate::environment::Environment;
use crate::object::Object;

pub fn interpret(statements: &Vec<Box<dyn Eval>>, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
    let mut error_count: u32 = 0;
    let mut res = Object::NIL;

    for stmt in statements {
        match stmt.eval(env.clone()) {
            Err(e) => {
                match e {
                    Error::BREAK | Error::CONTINUE => return Err(e),
                    Error::RETURN(_) => return Err(e),
                    _ => {},
                }
                error_count += 1; 
            },
            Ok(x) => res = x,
        }
    }
    if error_count > 0 {
        Err(Error::STRING("ParserError".into()))
    } else {
        Ok(res)
    }
}