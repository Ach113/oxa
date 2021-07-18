use std::fmt;
use core::ops::{Add, Sub, Mul, Div, BitOr, BitAnd, BitXor, Rem};
use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::AST::{BlockStmt, Error};
use crate::interpreter::interpret;
use crate::tokens::{Token, TokenType};
use crate::environment::Environment;

#[derive(Debug, Clone)]
pub enum Type {
    STRING(String),
    NUMERIC(f64),
    BOOL(bool),
    FUN(Function),
    METHOD(Function),
    CLASS(Class),
    OBJECT(Object),
    NIL,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::STRING(x) => write!(f, "{}", x),
            Type::NUMERIC(x) => write!(f, "{}", x),
            Type::BOOL(x) => write!(f, "{}", x),
            Type::FUN(x) => write!(f, "<fun {}>", x.name),
            Type::METHOD(x) => write!(f, "<method {}>", x.name),
            Type::CLASS(x) => write!(f, "<class {}>", x.name),
            Type::OBJECT(x) => write!(f, "<instance of {}>", x.class.name),
            Type::NIL => write!(f, ""),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => a == b,
            (Type::STRING(a), Type::STRING(b)) => a == b,
            (Type::NIL, Type::NIL) => true,
            (Type::BOOL(a), Type::BOOL(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => a.partial_cmp(&b),
            (Type::STRING(a), Type::STRING(b)) => a.partial_cmp(&b),
            (Type::NIL, Type::NIL) => Some(Ordering::Equal),
            (Type::BOOL(a), Type::BOOL(b)) => a.partial_cmp(&b),
            _ => None,
        }
    }
}

impl Add for Type {
    type Output = Result<Type, String>;

    fn add(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => Ok(Type::NUMERIC(a + b)),
            (Type::STRING(a), Type::STRING(b)) => Ok(Type::STRING(format!("{}{}", a, b).to_string())),
            _ => Err(String::from("TypeError for operator +"))
        }
    }
}


impl Sub for Type {
    type Output = Result<Type, String>;

    fn sub(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => Ok(Type::NUMERIC(a - b)),
            _ => Err(String::from("TypeError for operator -"))
        }
    }
}

impl Mul for Type {
    type Output = Result<Type, String>;

    fn mul(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => Ok(Type::NUMERIC(a * b)),
            (Type::NUMERIC(a), Type::STRING(b)) => {
                Ok(Type::STRING(b.repeat(a as usize)))
            },
            (Type::STRING(a), Type::NUMERIC(b)) => {
                Ok(Type::STRING(a.repeat(b as usize)))
            },
            _ => Err(String::from("TypeError for operator *"))
        }
    }
}

impl Div for Type {
    type Output = Result<Type, String>;

    fn div(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Type::NUMERIC(a / b))
                }
            },
            _ => Err(String::from("TypeError for operator /"))
        }
    }
}

impl Rem for Type {
    type Output = Result<Type, String>;

    fn rem(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::NUMERIC(a), Type::NUMERIC(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Type::NUMERIC(a % b))
                }
            },
            _ => Err(String::from("TypeError for operator %"))
        }
    }
}

impl BitOr for Type {
    type Output = Result<Type, String>;

    fn bitor(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::BOOL(a), Type::BOOL(b)) => Ok(Type::BOOL(a | b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitAnd for Type {
    type Output = Result<Type, String>;

    fn bitand(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::BOOL(a), Type::BOOL(b)) => Ok(Type::BOOL(a & b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitXor for Type {
    type Output = Result<Type, String>;

    fn bitxor(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::BOOL(a), Type::BOOL(b)) => Ok(Type::BOOL(a ^ b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Type {
    pub fn get_type(self) -> String {
        match self {
            Type::NUMERIC(_) => "numeric".to_string(),
            Type::STRING(_) => "string".to_string(),
            Type::BOOL(_) => "bool".to_string(),
            Type::FUN(_) => "function".to_string(),
            Type::METHOD(_) => "method".to_string(),
            Type::CLASS(_) => "class".to_string(),
            Type::OBJECT(_) => "object".to_string(),
            Type::NIL => "nil".to_string(),
        }
    }
}

/*** FUNCTION ***/

pub trait Callable {
    fn call(&self, args: Vec<Type>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Type, Error>;
}

#[derive(Clone)]
pub struct Function {
    pub self_: Option<Object>,
    self_id: Option<String>,
    arity: usize, // number of arguments function takes
    address: u64, // line where function is declared
    pub name: String, // function identifier
    body: Rc<RefCell<BlockStmt>>, // executable body of function
    pub args: Vec<String>, // name of accepted parameters
}

impl Function {
    pub fn new(name: String, address: u64, body: Rc<RefCell<BlockStmt>>, args: Vec<String>) -> Self {
        Function {self_: None, self_id: None, arity: args.len(), name, address, body, args}
    }

    pub fn bind_self(&mut self, obj: Object, obj_id: Option<String>) {
        self.self_ = Some(obj);
        self.self_id = obj_id;
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
    fn call(&self, args: Vec<Type>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Type, Error> {

        if args.len() != self.arity {
            crate::error("TypeError", &format!("{}() takes {} positional arguments, {} were provided", self.name, self.arity, args.len()), callee.line);
            return Err(Error::STRING("function arity error".into()));
        }
        // new scope for the function
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        // insert function arguments into function scope
        for (identifier, value) in self.args.iter().zip(args.iter()) {
            let t = Token::new(identifier.clone(), Type::NIL, TokenType::IDENTIFIER, self.address);
            enclosing.borrow_mut().add(t, value.clone());
        }
        
        let res = match interpret(&(self.body.borrow().statements), enclosing.clone()) {
            Ok(x) => Ok(x),
            Err(e) => {
                match e {
                    Error::RETURN(x) => Ok(x),
                    _ => Err(e),
                }
            }
        };
        if let Some(id) = &self.self_id {
            let t = Token::new(self.args[0].clone(), Type::NIL, TokenType::IDENTIFIER, self.address);
            let value = enclosing.borrow().get(t)?;
            let id = Token::new(id.to_string(), Type::NIL, TokenType::IDENTIFIER, self.address);
            enclosing.borrow_mut().enclosing.as_ref().unwrap().borrow_mut().assign(id, value);
        }
        res
    }
}

/*** CLASS ***/
#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(name: String, methods: Vec<Function>) -> Self {
        let mut map: HashMap<String, Function> = HashMap::new();
        for method in methods {
            map.insert(method.name.clone(), method);
        }
        Class {name, methods: map}
    }

    pub fn arity(&self) -> usize {
        0
        /*
        match self.methods.get("init") {
            Some(f) => f.arity,
            None => 0,
        }
        */
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Class")
           .field("name", &self.name)
           .finish()
    }
}

impl Callable for Class {
    // class constructor
    fn call(&self, args: Vec<Type>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Type, Error> {
        if args.len() != self.arity() {
            crate::error("TypeError", &format!("{}() takes {} positional arguments, {} were provided", self.name, self.arity(), args.len()), callee.line);
            return Err(Error::STRING("constructor arity error".into()));
        }
        // new scope for the function
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        // class fields
        let mut fields: HashMap<String, Box<Type>> = HashMap::new();
        for (key, value) in &self.methods {
            fields.insert(key.clone(), Box::new(Type::METHOD(value.clone())));
        }
        Ok(Type::OBJECT(Object::new(self.clone(), fields)))
    }
}

#[derive(Clone)]
pub struct Object {
    class: Class,
    pub fields: HashMap<String, Box<Type>>,
}

impl Object {
    pub fn new(class: Class, fields: HashMap<String, Box<Type>>) -> Self {
        Object {class, fields}
    }

    pub fn get(&self, name: &Token) -> Result<Type, Error> {
        match self.fields.get(&name.lexeme) {
            Some(x) => Ok((**x).clone()),
            None => {
                crate::error("TypeError", &format!("{} has no attribute '{}'", self.class.name, name.lexeme), name.line);
                Err(Error::STRING(format!("{} has no attribute '{}'", self.class.name, name)))
            }
        }
    }

    pub fn set(&mut self, name: String, value: Type) -> Result<Type, Error> {
        self.fields.insert(name, Box::new(value.clone()));
        Ok(Type::OBJECT(self.clone()))
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Object")
           .field("class", &self.class.name)
           .finish()
    }
}