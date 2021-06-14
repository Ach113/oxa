use crate::AST::Stmt;
use crate::environment::Environment;

pub fn interpret(statements: &Vec<Box<dyn Stmt>>, mut env: &mut Environment) -> Result<(), ()> {
    let mut error: Result<(), ()> = Ok(()); // signals whether code execution was successful
    let mut error_count = 0;

    for stmt in statements {
        let res = stmt.eval(&mut env);
        if res.is_err() {
            error = Err(());
            error_count += 1;
        }
    }
    error
}