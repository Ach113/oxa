use std::env;
use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;

// modules made by yours truly
mod types;
mod tokens;
mod scanner;
mod AST;
mod parser;
mod environment;
mod interpreter;
mod tests;

use scanner::Scanner;
use parser::Parser;
use types::Type;
use interpreter::interpret;
use environment::Environment;

// executes given instruction/set of instructions
fn run(code: String, env: Rc<RefCell<Environment>>) -> Result<Type, String> {
    let mut scanner = Scanner::new(code);
    // scan scource code for tokens
    let tokens = scanner.scan_tokens();
    // construct parser with given tokens
    let mut parser = Parser::new(tokens);
    // get list of statements from the parser
    let statements = parser.parse()?;
    // execute the statements
    match interpret(&statements, env) {
        Ok(x) => Ok(x),
        Err(e) => {
            Err(e.to_string())
        },
    }
}

// function used to report errors to the user
fn error(error: &str, message: &str, line: u64) {
    println!("{} at line {}: {}", error, line, message);
}

// reads text from source file and runs it
fn runfile(filename: &str) -> Result<(), String> {
    match std::fs::read_to_string(filename) {
        Ok(code) => {
            let env = Rc::new(RefCell::new(Environment::new(None)));
            run(code, env)?;
            Ok(())
        },
        Err(_) => {
            println!("FileNotFound: file `{}` could not be found", filename);
            Err("FileNotFound".into())
        }
    }
    
}

// Read a line of input, Evaluate it, Print the result, then Loop
fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    loop {
        print!(">> ");
        std::io::stdout().flush()?; // necessary due to line-buffering of stdout
        let mut instruction = String::new();
        std::io::stdin().read_line(&mut instruction);
        if !instruction.trim().is_empty() {
            match run(instruction, env.clone()) {
                Err(_) => return Err("".into()),
                Ok(x) => {
                    match x {
                        Type::NIL => {},
                        _ => println!("{}", x),
                    }
                },
            }; 
        }
    }
    Ok(())
}

fn main() -> Result<(), String> {
    let args: Vec<_> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: oxa [filename]");
        // error code according to https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
        std::process::exit(64); 
    } else if args.len() == 2 {
        runfile(&args[1])?;
    } else {
        repl();
    }
    Ok(())
}
