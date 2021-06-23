use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::Eval;
use crate::environment::Environment;
use crate::tokens::Literal;

pub fn interpret(statements: &Vec<Box<dyn Eval>>, env: Rc<RefCell<Environment>>) -> Result<Literal, String> {
    let mut error_count: u32 = 0;
    let mut res = Literal::NIL;

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