#![allow(non_snake_case)]

use std::fmt;
use std::any::Any;

#[derive(Debug)]
pub enum TokenType {
    // single char tokens
    LEFT_PAREN(Token<()>),
    RIGHT_PAREN(Token<()>), 
    LEFT_BRACE(Token<()>), 
    RIGHT_BRACE(Token<()>),
    COMMA(Token<()>),
    DOT(Token<()>),
    MINUS(Token<()>), 
    PLUS(Token<()>), 
    SEMICOLON(Token<()>), 
    SLASH(Token<()>), 
    STAR(Token<()>),

    // One or two character tokens.
    BANG(Token<()>), 
    BANG_EQUAL(Token<()>),
    EQUAL(Token<()>), 
    EQUAL_EQUAL(Token<()>),
    GREATER(Token<()>), 
    GREATER_EQUAL(Token<()>),
    LESS(Token<()>), 
    LESS_EQUAL(Token<()>),

    // Literals.
    IDENTIFIER(Token<String>), 
    STRING(Token<String>), 
    NUMBER(Token<f64>),

    // Keywords.
    AND(Token<()>), 
    CLASS(Token<()>), 
    ELSE(Token<()>), 
    FALSE(Token<()>), 
    FUN(Token<()>), 
    FOR(Token<()>), 
    IF(Token<()>), 
    NIL(Token<()>), 
    OR(Token<()>),
    PRINT(Token<()>), 
    RETURN(Token<()>), 
    SUPER(Token<()>), 
    THIS(Token<()>), 
    TRUE(Token<()>), 
    VAR(Token<()>), 
    WHILE(Token<()>),

    EOF(Token<()>)
}

#[derive(Debug)]
pub struct Token<T> {
    lexeme: String,
    literal: T, // literals can be of any type (number or string in this case)
    line: u64
}

impl<T> Token<T> {
    pub fn new(lexeme: String, literal: T, line: u64) -> Token<T> {
        Token {lexeme, literal, line}
    }
}

impl <T: fmt::Display> fmt::Display for Token<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.lexeme, self.literal)
    }
}