#![allow(non_snake_case)]

use std::fmt;
use std::any::Any;

#[derive(Debug)]
pub enum TokenType {
    // single char tokens
    LEFT_PAREN,
    RIGHT_PAREN, 
    LEFT_BRACE, 
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS, 
    PLUS, 
    SEMICOLON, 
    SLASH, 
    STAR,

    // One or two character tokens.
    BANG, 
    BANG_EQUAL,
    EQUAL, 
    EQUAL_EQUAL,
    GREATER, 
    GREATER_EQUAL,
    LESS, 
    LESS_EQUAL,

    // Literals.
    IDENTIFIER, 
    STRING, 
    NUMBER,

    // Keywords.
    AND, 
    CLASS, 
    ELSE, 
    FALSE, 
    FUN, 
    FOR, 
    IF, 
    NIL, 
    OR,
    PRINT, 
    RETURN, 
    SUPER, 
    THIS, 
    TRUE, 
    VAR, 
    WHILE,

    EOF
}

#[derive(Debug)]
pub enum Literal {
    STRING(String),
    NUMERIC(f64),
    NIL,
}

#[derive(Debug)]
pub struct Token {
    lexeme: String,
    literal: Literal, // literals can be of any type (numbe, string or nil in this case)
    t: TokenType,
    line: u64,
}

impl Token {
    pub fn new(lexeme: String, literal: Literal, t: TokenType, line: u64) -> Token {
        Token {lexeme, literal, t, line}
    }
}

impl  fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} {:?}", self.lexeme, self.literal, self.t)
    }
}