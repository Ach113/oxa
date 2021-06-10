#![allow(non_snake_case)]

use std::fmt;
use std::any::Any;
use core::ops::{Add, Sub, Mul, Div};

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

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Literal {
    STRING(String),
    NUMERIC(f64),
    BOOL(bool),
    NIL,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::STRING(x) => write!(f, "{}", x),
            Literal::NUMERIC(x) => write!(f, "{}", x),
            Literal::BOOL(x) => write!(f, "{}", x),
            Literal::NIL => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub literal: Literal, // literals can be of any type (number, string, bool or nil in this case)
    pub t: TokenType,
    pub line: u64,
}

impl Token {
    pub fn new(lexeme: String, literal: Literal, t: TokenType, line: u64) -> Token {
        Token {lexeme, literal, t, line}
    }
}

impl fmt::Display for Token{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} {:?}", self.lexeme, self.literal, self.t)
    }
}

impl Add for Literal {
    type Output = Result<Literal, String>;

    fn add(self, right: Literal) -> Result<Literal, String> {
        match (self, right) {
            (Literal::NUMERIC(a), Literal::NUMERIC(b)) => Ok(Literal::NUMERIC(a + b)),
            (Literal::STRING(a), Literal::STRING(b)) => Ok(Literal::STRING(format!("{}{}", a, b).to_string())),
            _ => Err(String::from("TypeError for operator +"))
        }
    }
}


impl Sub for Literal {
    type Output = Result<Literal, String>;

    fn sub(self, right: Literal) -> Result<Literal, String> {
        match (self, right) {
            (Literal::NUMERIC(a), Literal::NUMERIC(b)) => Ok(Literal::NUMERIC(a - b)),
            _ => Err(String::from("TypeError for operator -"))
        }
    }
}

impl Mul for Literal {
    type Output = Result<Literal, String>;

    fn mul(self, right: Literal) -> Result<Literal, String> {
        match (self, right) {
            (Literal::NUMERIC(a), Literal::NUMERIC(b)) => Ok(Literal::NUMERIC(a * b)),
            (Literal::NUMERIC(a), Literal::STRING(b)) => {
                Ok(Literal::STRING(b.repeat(a as usize)))
            },
            (Literal::STRING(a), Literal::NUMERIC(b)) => {
                Ok(Literal::STRING(a.repeat(b as usize)))
            },
            _ => Err(String::from("TypeError for operator *"))
        }
    }
}

impl Div for Literal {
    type Output = Result<Literal, String>;

    fn div(self, right: Literal) -> Result<Literal, String> {
        match (self, right) {
            (Literal::NUMERIC(a), Literal::NUMERIC(b)) => Ok(Literal::NUMERIC(a / b)),
            _ => Err(String::from("TypeError for operator /"))
        }
    }
}

/*
impl ToString for Literal {
    fn to_string(self) -> String {
        match self {
            Literal::NUMERIC(x) => String::from("numeric"),
            Literal::BOOL(x) => String::from("bool"),
            Literal::STRING(x) => String::from("string"),
            Literal::NIL => String::from("nil"),
        }
    }
}
*/

