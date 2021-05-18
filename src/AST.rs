use crate::tokens::{Token, TokenType};
use std::fmt;

// generic expression trait
pub trait Expr {
    fn eval(&self) -> String;
}

// binary expression
pub struct Binary<L, R> {
    left: L,
    right: R,
    operator: Token,
}

// unary expression
pub struct Unary<E> {
    expression: E,
    operator: Token,
}

// literal expression
pub struct Literal {
    value: Token
}

// grouping expression
pub struct Grouping<E> {
    expression: E,
}

/*** eval implementations ***/

impl <L: Expr, R: Expr> Expr for Binary<L, R> {
    fn eval(&self) -> String {
        format!("({} {} {})", self.operator.lexeme, self.left.eval(), self.right.eval())
    }
}

impl <E: Expr> Expr for Unary<E> {
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

impl <E: Expr> Expr for Grouping<E> {
    fn eval(&self) -> String {
        format!("({})", self.expression.eval())
    }
} 

// constructors

impl <L: Expr, R: Expr> Binary<L, R> {
    pub fn new (operator: Token, left: L, right: R) -> Option<Binary<L, R>> {
        match &operator.t {
            TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL | TokenType::LESS | TokenType::LESS_EQUAL | TokenType::GREATER | TokenType::GREATER_EQUAL |
            TokenType::PLUS | TokenType::MINUS | TokenType::STAR | TokenType::SLASH => {
                Some(Binary{operator, left, right})
            },
            _ => {
                crate::error("Parsing error!", &format!("Unexpected operator {} for binary expression", &operator.lexeme), operator.line);
                None
            }
        }
    }
}

impl <E: Expr> Unary<E> {
    pub fn new(operator: Token, expression: E) -> Option<Unary<E>> {
        match &operator.t {
            TokenType::MINUS | TokenType::BANG => Some(Unary{operator, expression}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected operator {} for unary expression", &operator.lexeme), operator.line);
                None
            }
        }
    }
}

impl <E: Expr> Grouping<E> {
    pub fn new(expression: E) -> Grouping<E> {
        Grouping{expression}
    }
}

impl Literal {
    pub fn new(value: Token) -> Option<Literal> {
        match &value.t {
            TokenType::IDENTIFIER | TokenType::NUMBER | TokenType::STRING | TokenType::NIL => Some(Literal{value}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected value {} for a literal", &value.lexeme), value.line);
                None
            }
        }
    }
}
