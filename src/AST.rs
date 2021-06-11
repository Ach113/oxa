use crate::tokens::{Token, TokenType};
use crate::environment::Environment;
use std::fmt;

/* Expression */

// generic expression trait
pub trait Expr {
    fn eval(&self, env: &mut Environment) -> Result<crate::tokens::Literal, ()>;
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

// variable expression
pub struct Variable {
    identifier: Token
}

/*** eval implementations ***/

impl Expr for Binary {
    fn eval(&self, mut env: &mut Environment) -> Result<crate::tokens::Literal, ()> {
        let op = &self.operator.lexeme;
        let left = self.left.eval(&mut env);
        let right = self.right.eval(&mut env);
        if left.is_err() || right.is_err() {
            return Err(());
        }
        let left = left.unwrap();
        let right = right.unwrap();
        match &**op {
            "==" => {
                Ok(crate::tokens::Literal::BOOL(left == right))
            },
            "!=" => {
                Ok(crate::tokens::Literal::BOOL(left != right))
            },
            ">=" => {
                Ok(crate::tokens::Literal::BOOL(left >= right))
            },
            "<=" => {
                Ok(crate::tokens::Literal::BOOL(left <= right))
            },
            ">" => {
                Ok(crate::tokens::Literal::BOOL(left > right))
            }
            "<" => {
                Ok(crate::tokens::Literal::BOOL(left < right))
            },
            "+" => {
                let res = left.clone() + right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for +: {}, {}", left.get_type(), right.get_type()), self.operator.line);
                        Err(())
                    }
                }
            },
            "-" => {
                let res = left.clone() - right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for -: {}, {}", left.get_type(), right.get_type()), self.operator.line);
                        Err(())     
                    }
                }
            },
            "/" => {
                let res = left.clone() / right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        if x == String::from("ZeroDivisionError") {
                            crate::error("ZeroDivisionError", "", self.operator.line);
                        } else {
                            crate::error("TypeError", &format!("Invalid operand type(s) for /: {}, {}", left.get_type(), right.get_type()), self.operator.line);
                        }
                        Err(())                      
                    }
                }
            },
            "*" => {
                let res = left.clone() * right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        crate::error("TypeError", &format!("Invalid operand type(s) for *: {}, {}", left.get_type(), right.get_type()), self.operator.line);
                        Err(())      
                    }
                }
            },
            _ => {
                crate::error("Parsing Error", &format!("Unsupported binary operator {}", op), self.operator.line);
                Err(())
            }
        }
    }
}

impl Expr for Unary {
    fn eval(&self, mut env: &mut Environment) -> Result<crate::tokens::Literal, ()> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(&mut env);
        if expr.is_err() {
            return Err(());
        }
        let expr = expr.unwrap();
        match &**op {
            "-" => {
                match expr {
                    crate::tokens::Literal::NUMERIC(x) => Ok(crate::tokens::Literal::NUMERIC(-x)),
                    _ => {
                        crate::error("Parsing error", &format!("invalid right hand side expression for operator {}", op), self.operator.line);  
                        Err(())   
                    }
                }
            },
            "!" => {
                match expr {
                    crate::tokens::Literal::BOOL(x) => Ok(crate::tokens::Literal::BOOL(!x)),
                    _ => {
                        crate::error("Parsing error", &format!("invalid right hand side expression for operator {}", op), self.operator.line);   
                        Err(())  
                    }
                }
            },
            _ =>  {
                crate::error("Parsing error", &format!("invalid unary operator {}", op), self.operator.line);
                Err(())
            }
        }
    }
}

impl Expr for Literal {
    fn eval(&self, mut env: &mut Environment) -> Result<crate::tokens::Literal, ()> {
        Ok(self.value.literal.clone())
    }
}

impl Expr for Grouping {
    fn eval(&self, mut env: &mut Environment) -> Result<crate::tokens::Literal, ()> {
        self.expression.eval(&mut env)
    }
} 

impl Expr for Variable {
    fn eval(&self, mut env: &mut Environment) -> Result<crate::tokens::Literal, ()> {
        env.get(self.identifier.clone())
    }
}

// constructors and other implementations

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
            TokenType::NUMBER | TokenType::STRING | TokenType::NIL => Some(Literal{value}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected value {} for a literal", value.lexeme), value.line);
                None
            }
        }
    }
}

impl Variable {
    pub fn new(identifier: Token) -> Variable {
        Variable {identifier}
    }
}

/* Statements */

pub trait Stmt {
    fn eval(&self, env: &mut Environment) -> Result<(), ()>;
}

// An expression statement is one that evaluates an expression and ignores its result
pub struct ExprStmt {
    body: Box<dyn Expr>,
}

impl ExprStmt {
    pub fn new(expr: Box<dyn Expr>) -> ExprStmt {
        ExprStmt {body: expr}
    }
}

impl Stmt for ExprStmt {
    fn eval(&self, mut env: &mut Environment) -> Result<(), ()> {
        let res = self.body.eval(&mut env);
        if res.is_err() {
            Err(())
        } else {
            Ok(())
        }
    }
}

// print statement, prints the expression
pub struct PrintStmt {
    value: Box<dyn Expr>,
}

impl PrintStmt {
    pub fn new(expr: Box<dyn Expr>) -> PrintStmt {
        PrintStmt {value: expr}
    }
}

impl Stmt for PrintStmt {
    fn eval(&self, mut env: &mut Environment) -> Result<(), ()> {
        let val = self.value.eval(&mut env);
        if val.is_ok() {
            println!("{}", val.unwrap());
            Ok(())
        } else {
            Err(())
        }
    }
}

// declaration statement
pub struct VarDeclaration {
    identifier: Token,
    value: Box<dyn Expr>
}

impl VarDeclaration {
    pub fn new(identifier: Token, value: Box<dyn Expr>) -> VarDeclaration {
        VarDeclaration {identifier, value}
    }
}

impl Stmt for VarDeclaration {
    fn eval(&self, mut env: &mut Environment) -> Result<(), ()> {
        let value = self.value.eval(&mut env).unwrap();
        env.add(self.identifier.clone(), value);
        Ok(())
    }
}

// assignment 
pub struct Assignment {
    var: Variable,
    value: Box<dyn Expr>
}

impl Assignment {
    pub fn new(var: Variable, value: Box<dyn Expr>) -> Self {
        Assignment {var, value}
    }
}

impl Stmt for Assignment {
    fn eval(&self, mut env: &mut Environment) -> Result<(), ()> {
        if !env.contains_key(&self.var.identifier) {
            crate::error("NameError", &format!("Undefined variable {}", self.var.identifier.lexeme), self.var.identifier.line);
            Err(())
        } else {
            let value = self.value.eval(&mut env).unwrap();
            env.add(self.var.identifier.clone(), value);
            Ok(())
        }
    }
}


