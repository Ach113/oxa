#![allow(non_camel_case_types)]

use std::fmt;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
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
    PERCENT,
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
    SELF, 
    TRUE, 
    VAR, 
    WHILE,
    XOR,
    BREAK,
    CONTINUE,

    EOF
}

#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub literal: Type, // literals can be of type number, string, bool or nil
    pub t: TokenType,
    pub line: u64,
}

impl Token {
    pub fn new(lexeme: String, literal: Type, t: TokenType, line: u64) -> Token {
        Token {lexeme, literal, t, line}
    }
}

impl fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} {:?}", self.lexeme, self.literal, self.t)
    }
}