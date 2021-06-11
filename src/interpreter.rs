use crate::AST::Stmt;
use crate::environment::Environment;

pub fn interpret(statements: Vec<Box<dyn Stmt>>) -> Result<(), ()> {
    let mut error: Result<(), ()> = Ok(()); // signals whether code execution was successful
    let mut error_count = 0;

    let mut environment = Environment::new();

    for stmt in statements {
        let res = stmt.eval(&mut environment);
        if res.is_err() {
            error = Err(());
            error_count += 1;
        }
    }
    error
}