use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::Eval;
use crate::environment::Environment;
use crate::tokens::Literal;

pub fn interpret(statements: &Vec<Box<dyn Eval>>, env: Rc<RefCell<Environment>>) -> Result<Literal, ()> {
    let mut error_count: u32 = 0;
    let mut res = Literal::NIL;

    for stmt in statements {
        match stmt.eval(env.clone()) {
            Err(_) => error_count += 1,
            Ok(x) => res = x,
        }
    }
    if error_count > 0 {
        Err(())
    } else {
        Ok(res)
    }
}