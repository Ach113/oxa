use crate::AST::Stmt;
use crate::environment::Environment;

pub fn interpret(statements: &Vec<Box<dyn Stmt>>, mut env: &mut Environment) -> Result<Environment, ()> {
    let mut error_count: u32 = 0;

    for stmt in statements {
        match stmt.eval(&mut env) {
            Err(_) => error_count += 1,
            _ => {},
        };
    }
    
    if error_count == 0 {
        Ok(env.clone())
    } else {
        Err(())
    }
}