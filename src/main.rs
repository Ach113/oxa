use std::env;
use std::fs::File;
use std::io::{BufRead, Write};
use std::error::Error;

// modules made by yours truly
mod tokens;
mod scanner;
mod AST;
mod parser;

use scanner::Scanner;

// executes given instruction/set of instructions
fn run(code: String) -> Result<(), Box<dyn Error>> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_tokens();
    
    Ok(())
}

// function used to report errors to the user
fn error(error: &str, message: &str, line: u64) {
    println!("{} at line {}: {}", error, line, message);
    // std::process::exit(65); 
}

// reads text from source file and runs it
fn runfile(filename: &str) -> Result<(), Box<dyn Error>> {
    let code = std::fs::read_to_string(filename).unwrap();
    run(code);
    Ok(())
}

// Read a line of input, Evaluate it, Print the result, then Loop
fn repl() -> Result<(), Box<dyn Error>> {
    loop {
        print!(">> ");
        std::io::stdout().flush()?; // necessary due to line-buffering of stdout
        let mut instruction = String::new();
        std::io::stdin().read_line(&mut instruction);
        if !instruction.trim().is_empty() {
            let response = run(instruction);
            if response.is_ok() {
                // println!("{:?}", response.unwrap());
            }
        }
    }
    Ok(())
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: oxa [filename]");
        // error code according to https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
        std::process::exit(64); 
    } else if args.len() == 2 {
        runfile(&args[1]);
    } else {
        repl();
    }
}
