use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::tokens::{Token, TokenType};
use crate::environment::Environment;
use crate::interpreter::interpret;

/* Expression */

// generic expression trait
pub trait Eval {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()>;
}

// binary expression
pub struct Binary {
    left: Box<dyn Eval>,
    right: Box<dyn Eval>,
    operator: Token,
}

// unary expression
pub struct Unary {
    expression: Box<dyn Eval>,
    operator: Token,
}

// literal expression
pub struct Literal {
    value: crate::tokens::Literal
}

// grouping expression
pub struct Grouping {
    expression: Box<dyn Eval>,
}

// variable expression
pub struct Variable {
    identifier: Token
}

// variable assignment
pub struct Assignment {
    var: Variable,
    value: Box<dyn Eval>
}


/*** eval implementations ***/

impl Eval for Binary {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let op = &self.operator.lexeme;
        let left = self.left.eval(env.clone());
        let right = self.right.eval(env.clone());
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

impl Eval for Unary {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(env.clone());
        if expr.is_err() {
            return Err(());
        }
        let Eval = expr.unwrap();
        match &**op {
            "-" => {
                match Eval {
                    crate::tokens::Literal::NUMERIC(x) => Ok(crate::tokens::Literal::NUMERIC(-x)),
                    _ => {
                        crate::error("Parsing error", &format!("invalid right hand side expression for operator {}", op), self.operator.line);  
                        Err(())   
                    }
                }
            },
            "!" => {
                match Eval {
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

impl Eval for Literal {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        Ok(self.value.clone())
    }
}

impl Eval for Grouping {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        self.expression.eval(env)
    }
} 

impl Eval for Variable {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        env.borrow().get(self.identifier.clone())
    }
}

impl Eval for Assignment {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => env.borrow_mut().assign(self.var.identifier.lexeme.clone(), x.clone()),
            Err(e) => Err(e)
        }
    }
}

// constructors and other implementations

impl Binary {
    pub fn new (operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Option<Self> {
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
    pub fn new(operator: Token, expression: Box<dyn Eval>) -> Option<Unary> {
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
    pub fn new(expression: Box<dyn Eval>) -> Grouping {
        Grouping{expression}
    }
}

impl Literal {
    pub fn new(token: Token) -> Option<Literal> {
        match &token.t {
            TokenType::NUMBER | TokenType::STRING | TokenType::NIL => Some(Literal{value: token.literal}),
            _ => {
                crate::error("Parsing error!", &format!("Unexpected value {} for a literal", token.lexeme), token.line);
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

impl Assignment {
    pub fn new(var: Variable, value: Box<dyn Eval>) -> Self {
        Assignment {var, value}
    }
}

/* Statements */

// An expression statement is one that evaluates an expression and ignores its result
pub struct ExprStmt {
    body: Box<dyn Eval>,
}

impl ExprStmt {
    pub fn new(expr: Box<dyn Eval>) -> ExprStmt {
        ExprStmt {body: expr}
    }
}

impl Eval for ExprStmt {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        match self.body.eval(env) {
            Err(_) => Err(()),
            Ok(res) => Ok(res),
        }
    }
}

// print statement, prints the expression
pub struct PrintStmt {
    value: Box<dyn Eval>,
}

impl PrintStmt {
    pub fn new(expr: Box<dyn Eval>) -> PrintStmt {
        PrintStmt {value: expr}
    }
}

impl Eval for PrintStmt {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let val = self.value.eval(env);
        if val.is_ok() {
            println!("{}", val.unwrap());
            Ok(crate::tokens::Literal::NIL)
        } else {
            Err(())
        }
    }
}

// declaration statement
pub struct VarDeclaration {
    identifier: Token,
    value: Box<dyn Eval>
}

impl VarDeclaration {
    pub fn new(identifier: Token, value: Box<dyn Eval>) -> VarDeclaration {
        VarDeclaration {identifier, value}
    }
}

impl Eval for VarDeclaration {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => {
                match env.borrow_mut().add(self.identifier.clone(), x) {
                    Err(e) => Err(e),
                    Ok(_) => Ok(crate::tokens::Literal::NIL),
                }
            },
            Err(e) => Err(e)
        }
    }
}

// block statement
pub struct BlockStmt {
    statements: Vec<Box<dyn Eval>>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Box<dyn Eval>>) -> Self {
        BlockStmt {statements}
    }
}

impl Eval for BlockStmt {
    fn eval(&self, mut env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, ()> {
        let mut enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        interpret(&self.statements, enclosing)
    }
}




