use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

use crate::object::{Object, Function, Callable};
use crate::tokens::{Token, TokenType};
use crate::environment::Environment;
use crate::interpreter::interpret;

// AST error type
#[derive(Debug)]
pub enum Error {
    STRING(String),
    BREAK,
    CONTINUE,
    RETURN(Object),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::STRING(x) => write!(f, "{}", x),
            Error::BREAK => write!(f, "continue"),
            Error::CONTINUE => write!(f, "break"),
            Error::RETURN(x) => write!(f, "return {}", x),
        }
    }
}

/* Expression */

// generic expression trait
pub trait Eval {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error>;
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
    value: Object
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let op = &self.operator.lexeme;
        let left = self.left.eval(env.clone())?;
        let right = self.right.eval(env.clone())?;

        match &**op {
            "==" => {
                Ok(Object::BOOL(left == right))
            },
            "!=" => {
                Ok(Object::BOOL(left != right))
            },
            ">=" => {
                Ok(Object::BOOL(left >= right))
            },
            "<=" => {
                Ok(Object::BOOL(left <= right))
            },
            ">" => {
                Ok(Object::BOOL(left > right))
            }
            "<" => {
                Ok(Object::BOOL(left < right))
            },
            "+" => {
                let res = left.clone() + right.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for +: {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::STRING(error_message.into()))
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
                        Err(Error::STRING(error_message.into()))
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
                            Err(Error::STRING("ZeroDivisionError".into()))
                        } else {
                            let error_message = format!("Invalid operand type(s) for /: {}, {}", left.get_type(), right.get_type());
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(Error::STRING(error_message.into()))
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
                            Err(Error::STRING("ZeroDivisionError".into()))
                        } else {
                            let error_message = format!("Invalid operand type(s) for %: {}, {}", left.get_type(), right.get_type());
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(Error::STRING(error_message.into()))
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
                        Err(Error::STRING(error_message.into()))
                    }
                }
            },
            _ => {
                crate::error("Parsing Error", &format!("Unsupported binary operator {}", op.clone()), self.operator.line);
                Err(Error::STRING(format!("Unsupported binary operator {}", op).into()))
            }
        }
    }
}

impl Eval for Unary {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(env.clone())?;
        match &**op {
            "-" => {
                match expr {
                    Object::NUMERIC(x) => Ok(Object::NUMERIC(-x)),
                    _ => {
                        let error_message = format!("invalid right hand side expression for operator {}", op);
                        crate::error("Parsing error", &error_message, self.operator.line);  
                        Err(Error::STRING(error_message.into())) 
                    }
                }
            },
            "!" => {
                match expr {
                    Object::BOOL(x) => Ok(Object::BOOL(!x)),
                    _ => {
                        let error_message = format!("invalid right hand side expression for operator {}", op);
                        crate::error("Parsing error", &error_message, self.operator.line);  
                        Err(Error::STRING(error_message.into()))   
                    }
                }
            },
            _ =>  {
                crate::error("Parsing error", &format!("invalid unary operator '{}'", op.clone()), self.operator.line);
                Err(Error::STRING(format!("invalid unary operator '{}'", op).into()))
            }
        }
    }
}

impl Eval for Literal {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        Ok(self.value.clone())
    }
}

impl Eval for Grouping {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        self.expression.eval(env)
    }
} 

impl Eval for Variable {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        env.borrow().get(self.identifier.clone())
    }
}

impl Eval for Assignment {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => env.borrow_mut().assign(self.var.identifier.clone(), x.clone()),
            Err(_) => Err(Error::STRING("".into()))
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
    pub fn new(operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Result<Self, Error> {
        match &operator.t {
            TokenType::OR | TokenType::AND | TokenType::XOR => {},
            _ => {
                crate::error("SyntaxError", &format!("Invalid logical operator '{}'", operator), operator.line);
                return Err(Error::STRING(format!("SyntaxError: invalid logical operator '{}'", operator)));
            },
        }
        Ok(LogicalExpr {operator, left, right})
    }
}

impl Eval for LogicalExpr {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
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
                        Err(Error::STRING(error_message.into()))
                    }
                }
            },
            TokenType::AND => {
                match left.clone() & right.clone() {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for 'and': {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::STRING(error_message.into()))
                    }
                }
            },
            TokenType::XOR => {
                match left.clone() ^ right.clone() {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!("Invalid operand type(s) for 'xor': {}, {}", left.get_type(), right.get_type());
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::STRING(error_message.into()))
                    }
                }
            },
            _ => {
                crate::error("SyntaxError", &format!("Invalid logical operator '{}'", self.operator), self.operator.line);
                Err(Error::STRING(format!("SyntaxError: invalid operator '{}'", self.operator).into()))
            }
        }
    }
}

// function call
pub struct FunctionCall {
    callee: Box<dyn Eval>,
    paren: Token,
    arguments: Vec<Box<dyn Eval>>
}

impl FunctionCall {
    pub fn new(callee: Box<dyn Eval>, paren: Token, arguments: Vec<Box<dyn Eval>>) -> Self {
        FunctionCall {callee, paren, arguments}
    }
}

impl Eval for FunctionCall {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let f = match self.callee.eval(env.clone()) {
            Ok(obj) => {
                match obj {
                    Object::FUN(f) => f,
                    _ => {
                        crate::error("TypeError", &format!("type '{}' is not callable", obj.get_type()), self.paren.line);
                        return Err(Error::STRING("type not callable".into()));
                    },
                }
            },
            Err(e) => return Err(e),
        };
        let mut args: Vec<Object> = Vec::new();
        // evaluate the list of arguments
        for arg in &self.arguments {
           args.push(arg.eval(env.clone())?);
        }
        f.call(args, env.clone(), self.paren.clone())
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        match self.value.eval(env) {
            Ok(val) => {
                println!("{}", val);
                Ok(Object::NIL)
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => {
                match env.borrow_mut().add(self.identifier.clone(), x) {
                    Err(e) => Err(e),
                    Ok(_) => Ok(Object::NIL),
                }
            },
            Err(e) => Err(e)
        }
    }
}

// block statement
pub struct BlockStmt {
    pub statements: Vec<Box<dyn Eval>>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Box<dyn Eval>>) -> Self {
        BlockStmt {statements}
    }
}

impl Eval for BlockStmt {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        if self.condition.eval(env.clone()).unwrap() == Object::BOOL(true) {
            return self.expr.eval(env);
        }
        match &self.else_stmt {
            Some(stmt) => stmt.eval(env),
            None => Ok(Object::NIL), 
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        let mut ret: Result<Object, Error> = Ok(Object::NIL);
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        while self.condition.eval(env.clone())? == Object::BOOL(true) {
            for stmt in &self.body.statements {
                let ret = stmt.eval(enclosing.clone());
                match ret {
                    Ok(_) => {},
                    Err(e) => {
                        match e {
                            Error::BREAK => return Ok(Object::NIL),
                            Error::CONTINUE => break,
                            _ => return Err(e),
                        }
                    },
                }
            }
        }
        Ok(Object::NIL)
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        Err(Error::BREAK)
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        Err(Error::CONTINUE)
    }
}

// function declaration

pub struct FunDeclaration {
    identifier: Token,
    f: Function,
}

impl FunDeclaration {
    pub fn new(identifier: Token, args: Vec<Token>, body: BlockStmt) -> Self {
        let args: Vec<String> = args.iter().map(|x| x.lexeme.clone()).collect();
        let f = Function::new(identifier.lexeme.clone(), identifier.line, Rc::new(RefCell::new(body)), args);
        FunDeclaration {identifier, f}
    }
}

impl Eval for FunDeclaration {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        // add function declaration to the symbol table
        env.borrow_mut().add(self.identifier.clone(), Object::FUN(self.f.clone()))?;
        Ok(Object::NIL)
    }
}

// return statement
pub struct Return {
    expr: Box<dyn Eval>
}

impl Return {
    pub fn new(expr:Box<dyn Eval>) -> Self {
        Return {expr}
    }
}

impl Eval for Return {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, Error> {
        Err(Error::RETURN(self.expr.eval(env.clone())?))
    }
}