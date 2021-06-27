use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::Eval;
use crate::environment::Environment;
use crate::object::Object;

pub fn interpret(statements: &Vec<Box<dyn Eval>>, env: Rc<RefCell<Environment>>) -> Result<Object, String> {
    let mut error_count: u32 = 0;
    let mut res = Object::NIL;

    for stmt in statements {
        match stmt.eval(env.clone()) {
            Err(e) => {
                if e == String::from("'break' outside loop") || e == String::from("'continue' outside loop") {
                    return Err(e);
                }
                error_count += 1; 
            },
            Ok(x) => res = x,
        }
    }
    if error_count > 0 {
        Err("ParserError".into())
    } else {
        Ok(res)
    }
}