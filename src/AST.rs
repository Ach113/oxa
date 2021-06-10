use crate::tokens::{Token, TokenType};
use std::fmt;

// generic expression trait
pub trait Expr {
    fn eval(&self) -> crate::tokens::Literal;
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
    fn eval(&self) -> crate::tokens::Literal {
        let op = &self.operator.lexeme;
        let left = self.left.eval();
        let right = self.right.eval();
        match &**op {
            "==" => {
                crate::tokens::Literal::BOOL(left == right)
            },
            "!=" => {
                crate::tokens::Literal::BOOL(left != right)
            },
            ">=" => {
                crate::tokens::Literal::BOOL(left >= right)
            },
            "<=" => {
                crate::tokens::Literal::BOOL(left <= right)
            },
            ">" => {
                crate::tokens::Literal::BOOL(left > right)
            }
            "<" => {
                crate::tokens::Literal::BOOL(left < right)
            },
            "+" => {
                let res = left.clone() + right.clone();
                match res {
                    Ok(x) => x,
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for +: {}, {}", left.to_string(), right.to_string()), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            "-" => {
                let res = left.clone() - right.clone();
                match res {
                    Ok(x) => x,
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for -: {}, {}", left.to_string(), right.to_string()), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            "/" => {
                let res = left.clone() / right.clone();
                match res {
                    Ok(x) => x,
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for /: {}, {}", left.to_string(), right.to_string()), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            "*" => {
                let res = left.clone() * right.clone();
                match res {
                    Ok(x) => x,
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for *: {}, {}", left.to_string(), right.to_string()), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            _ => {
                crate::error("Parsing Error", &format!("Unsupported binary operator {}", op), self.operator.line);
                crate::tokens::Literal::NIL
            }
        }
    }
}

impl Expr for Unary {
    fn eval(&self) -> crate::tokens::Literal {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval();
        match &**op {
            "-" => {
                match expr {
                    crate::tokens::Literal::NUMERIC(x) => crate::tokens::Literal::NUMERIC(-x),
                    _ => {
                        crate::error("Parsing error", &format!("invalid right hand side expression for operator {}", op), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            "!" => {
                match expr {
                    crate::tokens::Literal::BOOL(x) => crate::tokens::Literal::BOOL(!x),
                    _ => {
                        crate::error("Parsing error", &format!("invalid right hand side expression for operator {}", op), self.operator.line);
                        crate::tokens::Literal::NIL
                    }
                }
            },
            _ =>  {
                crate::error("Parsing error", &format!("invalid unary operator {}", op), self.operator.line);
                crate::tokens::Literal::NIL
            }
        }
    }
}

impl Expr for Literal {
    fn eval(&self) -> crate::tokens::Literal {
        self.value.literal.clone()
    }
}

impl Expr for Grouping {
    fn eval(&self) -> crate::tokens::Literal {
        self.expression.eval()
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
                crate::error("Parsing error!", &&format!("Unexpected operator {} for binary expression", operator.lexeme), operator.line);
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
                crate::error("Parsing error!", &&format!("Unexpected operator {} for unary expression", operator.lexeme), operator.line);
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
                crate::error("Parsing error!", &&format!("Unexpected value {} for a literal", value.lexeme), value.line);
                None
            }
        }
    }
}
