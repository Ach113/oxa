use std::fmt;
use core::ops::{Add, Sub, Mul, Div, BitOr, BitAnd, BitXor, Rem};
use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::RefCell;

use crate::AST::{BlockStmt, Error};
use crate::interpreter::interpret;
use crate::tokens::{Token, TokenType};
use crate::environment::Environment;

#[derive(Debug, Clone)]
pub enum Object {
    STRING(String),
    NUMERIC(f64),
    BOOL(bool),
    FUN(Function),
    NIL,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::STRING(x) => write!(f, "{}", x),
            Object::NUMERIC(x) => write!(f, "{}", x),
            Object::BOOL(x) => write!(f, "{}", x),
            Object::FUN(x) => write!(f, "<fun {}>", x.name),
            Object::NIL => write!(f, ""),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => a == b,
            (Object::STRING(a), Object::STRING(b)) => a == b,
            (Object::NIL, Object::NIL) => true,
            (Object::BOOL(a), Object::BOOL(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => a.partial_cmp(&b),
            (Object::STRING(a), Object::STRING(b)) => a.partial_cmp(&b),
            (Object::NIL, Object::NIL) => Some(Ordering::Equal),
            (Object::BOOL(a), Object::BOOL(b)) => a.partial_cmp(&b),
            _ => None,
        }
    }
}

impl Add for Object {
    type Output = Result<Object, String>;

    fn add(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => Ok(Object::NUMERIC(a + b)),
            (Object::STRING(a), Object::STRING(b)) => Ok(Object::STRING(format!("{}{}", a, b).to_string())),
            _ => Err(String::from("TypeError for operator +"))
        }
    }
}


impl Sub for Object {
    type Output = Result<Object, String>;

    fn sub(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => Ok(Object::NUMERIC(a - b)),
            _ => Err(String::from("TypeError for operator -"))
        }
    }
}

impl Mul for Object {
    type Output = Result<Object, String>;

    fn mul(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => Ok(Object::NUMERIC(a * b)),
            (Object::NUMERIC(a), Object::STRING(b)) => {
                Ok(Object::STRING(b.repeat(a as usize)))
            },
            (Object::STRING(a), Object::NUMERIC(b)) => {
                Ok(Object::STRING(a.repeat(b as usize)))
            },
            _ => Err(String::from("TypeError for operator *"))
        }
    }
}

impl Div for Object {
    type Output = Result<Object, String>;

    fn div(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Object::NUMERIC(a / b))
                }
            },
            _ => Err(String::from("TypeError for operator /"))
        }
    }
}

impl Rem for Object {
    type Output = Result<Object, String>;

    fn rem(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::NUMERIC(a), Object::NUMERIC(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Object::NUMERIC(a % b))
                }
            },
            _ => Err(String::from("TypeError for operator %"))
        }
    }
}

impl BitOr for Object {
    type Output = Result<Object, String>;

    fn bitor(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::BOOL(a), Object::BOOL(b)) => Ok(Object::BOOL(a | b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitAnd for Object {
    type Output = Result<Object, String>;

    fn bitand(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::BOOL(a), Object::BOOL(b)) => Ok(Object::BOOL(a & b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitXor for Object {
    type Output = Result<Object, String>;

    fn bitxor(self, right: Object) -> Result<Object, String> {
        match (self, right) {
            (Object::BOOL(a), Object::BOOL(b)) => Ok(Object::BOOL(a ^ b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Object {
    pub fn get_type(self) -> String {
        match self {
            Object::NUMERIC(_) => "numeric".to_string(),
            Object::STRING(_) => "string".to_string(),
            Object::BOOL(_) => "bool".to_string(),
            Object::FUN(_) => "function".to_string(),
            Object::NIL => "nil".to_string(),
        }
    }
}

/*** FUNCTION ***/

pub trait Callable {
    fn call(&self, args: Vec<Object>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Object, Error>;
}

#[derive(Clone)]
pub struct Function {
    arity: usize, // number of arguments function takes
    address: u64, // line where function is declared
    pub name: String, // function identifier
    body: Rc<RefCell<BlockStmt>>, // executable body of function
    args: Vec<String>, // name of accepted parameters
}

impl Function {
    pub fn new(name: String, address: u64, body: Rc<RefCell<BlockStmt>>, args: Vec<String>) -> Self {
        Function {arity: args.len(), name, address, body, args}
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Function")
           .field("identifier", &self.name)
           .field("address", &self.address)
           .finish()
    }
}

impl Callable for Function {
    fn call(&self, args: Vec<Object>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Object, Error> {
        if args.len() != self.arity {
            crate::error("TypeError", &format!("{}() takes {} positional arguments, {} were provided", self.name, self.arity, args.len()), callee.line);
            return Err(Error::STRING("function arity error".into()));
        }
        // new scope for the function
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        // insert function arguments into function scope
        for (identifier, value) in self.args.iter().zip(args.iter()) {
            let t = Token::new(identifier.clone(), Object::NIL, TokenType::IDENTIFIER, self.address);
            enclosing.borrow_mut().add(t, value.clone());
        }
        match interpret(&(self.body.borrow().statements), enclosing) {
            Ok(x) => Ok(x),
            Err(e) => {
                match e {
                    Error::RETURN(x) => Ok(x),
                    _ => Err(e),
                }
            }
        }
    }
}