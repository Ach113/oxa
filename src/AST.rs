use std::cell::RefCell;
use std::rc::Rc;

use crate::tokens::{Token, TokenType};
use crate::environment::Environment;
use crate::interpreter::interpret;

/* Expression */

// generic expression trait
pub trait Eval {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String>;
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let op = &self.operator.lexeme;
        let left = self.left.eval(env.clone())?;
        let right = self.right.eval(env.clone())?;

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
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for +: {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())
                    }
                }
            },
            "-" => {
                let res = left.clone() - right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for -: {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())
                    }
                }
            },
            "/" => {
                let res = left.clone() / right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        if x == String::from("ZeroDivisionError") {
                            crate::error("ZeroDivisionError", "division by zero", self.operator.line);
                            Err("ZeroDivisionError".into())
                        } else {
                            let error_message = format!("Invalid operand type(s) for /: {}, {}", left.get_type(), right.get_type());
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(error_message.into())      
                        }               
                    }
                }
            },
            "%" => {
                let res = left.clone() % right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        if x == String::from("ZeroDivisionError") {
                            crate::error("ZeroDivisionError", "modulo division by zero", self.operator.line);
                            Err("ZeroDivisionError".into())
                        } else {
                            let error_message = format!("Invalid operand type(s) for %: {}, {}", left.get_type(), right.get_type());
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(error_message.into())      
                        }               
                    }
                }
            },
            "*" => {
                let res = left.clone() * right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for *: {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())   
                    }
                }
            },
            _ => {
                crate::error("Parsing Error", &format!("Unsupported binary operator {}", op.clone()), self.operator.line);
                Err(format!("Unsupported binary operator {}", op).into())
            }
        }
    }
}

impl Eval for Unary {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(env.clone())?;
        match &**op {
            "-" => {
                match expr {
                    crate::tokens::Literal::NUMERIC(x) => Ok(crate::tokens::Literal::NUMERIC(-x)),
                    _ => {
                        let error_message = format!("invalid right hand side expression for operator {}", op);
                        crate::error("Parsing error", &error_message, self.operator.line);  
                        Err(error_message.into())   
                    }
                }
            },
            "!" => {
                match expr {
                    crate::tokens::Literal::BOOL(x) => Ok(crate::tokens::Literal::BOOL(!x)),
                    _ => {
                        let error_message = format!("invalid right hand side expression for operator {}", op);
                        crate::error("Parsing error", &error_message, self.operator.line);  
                        Err(error_message.into())   
                    }
                }
            },
            _ =>  {
                crate::error("Parsing error", &format!("invalid unary operator '{}'", op.clone()), self.operator.line);
                Err(format!("invalid unary operator '{}'", op).into())
            }
        }
    }
}

impl Eval for Literal {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        Ok(self.value.clone())
    }
}

impl Eval for Grouping {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        self.expression.eval(env)
    }
} 

impl Eval for Variable {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        env.borrow().get(self.identifier.clone())
    }
}

impl Eval for Assignment {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => env.borrow_mut().assign(self.var.identifier.clone(), x.clone()),
            Err(_) => Err("".into())
        }
    }
}

// constructors and other implementations

impl Binary {
    pub fn new (operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Option<Self> {
        match &operator.t {
            TokenType::EQUAL_EQUAL | TokenType::BANG_EQUAL | TokenType::LESS | TokenType::LESS_EQUAL | TokenType::GREATER | TokenType::GREATER_EQUAL |
            TokenType::PLUS | TokenType::MINUS | TokenType::STAR | TokenType::SLASH | TokenType::PERCENT => {
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
            TokenType::NUMBER | TokenType::STRING | TokenType::NIL | TokenType::TRUE | TokenType::FALSE => Some(Literal{value: token.literal}),
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

// logical expressions
pub struct LogicalExpr {
    operator: Token,
    left: Box<dyn Eval>,
    right: Box<dyn Eval>,
}

impl LogicalExpr {
    pub fn new(operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Result<Self, String> {
        match &operator.t {
            TokenType::OR | TokenType::AND | TokenType::XOR => {},
            _ => {
                crate::error("SyntaxError", &format!("Invalid logical operator '{}'", operator), operator.line);
                return Err(format!("SyntaxError: invalid logical operator '{}'", operator).into());
            },
        }
        Ok(LogicalExpr {operator, left, right})
    }
}

impl Eval for LogicalExpr {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let left = self.left.eval(env.clone())?;
        let right = self.right.eval(env.clone())?;
        let op = &self.operator.t;

        match &op {
            TokenType::OR => {
                match left.clone() | right.clone() {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for 'or': {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())
                    }
                }
            },
            TokenType::AND => {
                match left.clone() & right.clone() {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for 'and': {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())
                    }
                }
            },
            TokenType::XOR => {
                match left.clone() ^ right.clone() {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for 'xor': {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(error_message.into())
                    }
                }
            },
            _ => {
                crate::error("SyntaxError", &format!("Invalid logical operator '{}'", self.operator), self.operator.line);
                Err(format!("SyntaxError: invalid operator '{}'", self.operator).into())
            }
        }
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        match self.body.eval(env) {
            Err(e) => Err(e),
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        match self.value.eval(env) {
            Ok(val) => {
                println!("{}", val);
                Ok(crate::tokens::Literal::NIL)
            },
            Err(e) => Err(e),
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        interpret(&self.statements, enclosing)
    }
}

// if statement
pub struct IfStmt {
    condition: Box<dyn Eval>,
    expr: Box<dyn Eval>,
    else_stmt: Option<Box<dyn Eval>>,
}

impl IfStmt {
    pub fn new(condition: Box<dyn Eval>, expr: Box<dyn Eval>, else_stmt: Option<Box<dyn Eval>>) -> Self {
        IfStmt {condition, expr, else_stmt}
    }
}

impl Eval for IfStmt {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        if self.condition.eval(env.clone()).unwrap() == crate::tokens::Literal::BOOL(true) {
            return self.expr.eval(env);
        }
        match &self.else_stmt {
            Some(stmt) => stmt.eval(env),
            None => Ok(crate::tokens::Literal::NIL), 
        }
    }
}

// while loop
pub struct WhileLoop {
    condition: Box<dyn Eval>,
    body: BlockStmt,
}

impl WhileLoop {
    pub fn new(condition: Box<dyn Eval>, body: BlockStmt) -> Self {
        WhileLoop {condition, body}
    }
}

impl Eval for WhileLoop {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        let mut ret: Result<crate::tokens::Literal, String> = Ok(crate::tokens::Literal::NIL);
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        while self.condition.eval(env.clone())? == crate::tokens::Literal::BOOL(true) {
            for stmt in &self.body.statements {
                let ret = stmt.eval(enclosing.clone());
                match ret {
                    Ok(_) => {},
                    Err(e) => {
                        if e == String::from("'break' outside loop") {
                            return Ok(crate::tokens::Literal::NIL);
                        } else if e == String::from("'continue' outside loop") {
                            break;
                        } else {
                            return Err(e);
                        }
                    },
                }
            }
        }
        Ok(crate::tokens::Literal::NIL)
    }
}

// break statement
/* very hacky implementation, calling eval() on break returns Err(), which causes loop to terminate (without calling crate::error) */
pub struct Break {
    
}

impl Break {
    pub fn new() -> Self {
        Break {}
    }
}

impl Eval for Break {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        Err("'break' outside loop".into())
    }
}

// continue statement
pub struct Continue {

}

impl Continue {
    pub fn new() -> Self {
        Continue {}
    }
}

impl Eval for Continue {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<crate::tokens::Literal, String> {
        Err("'continue' outside loop".into())
    }
}







