use crate::tokens::{Token, TokenType};
use std::fmt;

// generic expression trait
pub trait Expr {
    fn eval(&self) -> String;
}

// binary expression
pub struct Binary {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    operator: Token,
}

// unary expression
pub struct Unary {
    expression: Box<dyn Expr>,
    operator: Token,
}

// literal expression
pub struct Literal {
    value: Token
}

// grouping expression
pub struct Grouping {
    expression: Box<dyn Expr>,
}

/*** eval implementations ***/

impl Expr for Binary {
    fn eval(&self) -> String {
        format!("({} {} {})", self.operator.lexeme, self.left.eval(), self.right.eval())
    }
}

impl Expr for Unary {
    fn eval(&self) -> String {
        format!("({} {})", self.operator.lexeme, self.expression.eval())
    }
}

impl Expr for Literal {
    fn eval(&self) -> String {
        let value = match &self.value.literal {
            crate::tokens::Literal::STRING(x) => x.clone(),
            crate::tokens::Literal::NUMERIC(x) => x.to_string(),
            crate::tokens::Literal::BOOL(x) => x.to_string(),
            crate::tokens::Literal::NIL => "".to_string(),
        };
        format!("{}", self.value.literal)
    }
}

impl Expr for Grouping {
    fn eval(&self) -> String {
        format!("({})", self.expression.eval())
    }
} 

// constructors

impl Binary {
    pub fn new (operator: Token, left: Box<dyn Expr>, right: Box<dyn Expr>) -> Option<Self> {
        match &operator.t {
            TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL | TokenType::LESS | TokenType::LESS_EQUAL | TokenType::GREATER | TokenType::GREATER_EQUAL |
            TokenType::PLUS | TokenType::MINUS | TokenType::STAR | TokenType::SLASH => {
                Some(Binary{operator, left, right})
            },
            _ => {
                crate::error("Parsing error!", &format!("Unexpected operator {} for binary expression", operator.lexeme), operator.line);
                None
            }
        }
    }
}

impl Unary {
    pub fn new(operator: Token, expression: Box<dyn Expr>) -> Option<Unary> {
        match &operator.t {
            TokenType::MINUS | TokenType::BANG => Some(Unary{operator, expression}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected operator {} for unary expression", operator.lexeme), operator.line);
                None
            }
        }
    }
}

impl Grouping {
    pub fn new(expression: Box<dyn Expr>) -> Grouping {
        Grouping{expression}
    }
}

impl Literal {
    pub fn new(value: Token) -> Option<Literal> {
        match &value.t {
            TokenType::IDENTIFIER | TokenType::NUMBER | TokenType::STRING | TokenType::NIL => Some(Literal{value}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected value {} for a literal", value.lexeme), value.line);
                None
            }
        }
    }
}
