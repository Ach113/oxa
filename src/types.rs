use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use core::ops::{Add, Sub, Mul, Div, BitOr, BitAnd, BitXor, Rem};
use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::io::prelude::*;

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
    NATIVE(NativeFunction),
    NATIVEC(NativeClass),
    NIL,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::STRING(x) => write!(f, "{}", x),
            Type::NUMERIC(x) => write!(f, "{}", x),
            Type::BOOL(x) => write!(f, "{}", x),
            Type::FUN(x) => write!(f, "<fun {}>", x.name),
            Type::NATIVE(x) => write!(f, "<fun {:?}>", x),
            Type::METHOD(x) => write!(f, "<method {}>", x.name),
            Type::CLASS(x) => write!(f, "<class {}>", x.name),
            Type::OBJECT(x) => write!(f, "<instance of {}>", x.class.name),
            Type::NATIVEC(x) => write!(f, "<class {:?}>", x),
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
            Type::NATIVE(_) => "function".to_string(),
            Type::METHOD(_) => "method".to_string(),
            Type::CLASS(_) => "class".to_string(),
            Type::NATIVEC(_) => "class".to_string(),
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

// Native functions
#[derive(Debug, Clone)]
pub enum NativeFunction {
    INPUT,
    READ,
    WRITE,
    TIME,
    // native methods for <list>
    INDEX(Rc<RefCell<Vec<Type>>>),
    ADD(Rc<RefCell<Vec<Type>>>),
    LEN(Rc<RefCell<Vec<Type>>>),
    REMOVE(Rc<RefCell<Vec<Type>>>),
    SET(Rc<RefCell<Vec<Type>>>),
    NEXT(Rc<RefCell<Vec<Type>>>, Rc<Cell<usize>>),
}

impl Callable for NativeFunction {
    fn call(&self, args: Vec<Type>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Type, Error> {
        match self {
            NativeFunction::INPUT => {
                if args.len() != 0 {
                    crate::error("TypeError", &format!("read takes 0 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).expect("IOError: failed to read from stdin");
                match input.trim().parse::<f64>() {
                    Ok(x) => return Ok(Type::NUMERIC(x)),
                    Err(_) => return Ok(Type::STRING(input.trim().to_string())),
                }
                Ok(Type::NIL)
            },
            NativeFunction::READ => {
                if args.len() == 1 {
                    if let Type::STRING(filename) = &args[0] {
                        match std::fs::read_to_string(&filename) {
                            Ok(s) => {
                                return Ok(Type::STRING(s));
                            },
                            Err(_) => {
                                crate::error("FileNotFound", &format!("file `{}` could not be found", filename), callee.line);
                                return Err(Error::STRING("FileNotFound".into()));
                            }
                        }
                    } else {
                        crate::error("TypeError", &format!("invalid file handle {}", args[0].clone().get_type()), callee.line);
                        return Err(Error::STRING("TypeError".into()));
                    }
                } else {
                    crate::error("TypeError", &format!("read takes 1 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::WRITE => {
                if args.len() == 2 {
                    match (args[0].clone(), args[1].clone()) {
                        (Type::STRING(filename), Type::STRING(value)) => {
                            let mut file = std::fs::File::create(&filename).expect(&format!("IOError: could not create {}", &filename));
                        file.write_all(value.as_bytes()).expect(&format!("IOError: could not write to {}", &filename));
                        return Ok(Type::NIL);
                        },
                        _ => {
                            crate::error("TypeError", &format!("expected (String, String), got ({}, {})", args[0].clone().get_type(), args[1].clone().get_type()), callee.line);
                            return Err(Error::STRING("TypeError".into()));
                        },
                    }
                    
                } else {
                    crate::error("TypeError", &format!("write takes 2 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("TypeError".into()));
                }
            },
            NativeFunction::TIME => {
                if args.len() != 0 {
                    crate::error("TypeError", &format!("read takes 0 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
                return Ok(Type::NUMERIC(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as f64));
            },
            // <list> methods
            NativeFunction::INDEX(list) => {
                if args.len() == 1 {
                    match &args[0] {
                        Type::NUMERIC(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "list index out of range", callee.line);
                                return Err(Error::STRING("IndexError".into()));
                            }
                            return Ok(list.borrow()[index].clone());
                        },
                        _ => {
                            crate::error("TypeError", "invalid index type", callee.line);
                            return Err(Error::STRING("TypeError".into()));
                        }
                    }
                } else {
                    crate::error("TypeError", &format!("`index` takes 1 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::ADD(list) => {
                if args.len() == 1 {
                    list.borrow_mut().push(args[0].clone());
                    return Ok(Type::NIL);
                } else {
                    crate::error("TypeError", &format!("`add` takes 1 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::LEN(list) => {
                if args.len() == 0 {
                    return Ok(Type::NUMERIC(list.borrow_mut().len() as f64));
                } else {
                    crate::error("TypeError", &format!("`len` takes 0 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::REMOVE(list) => {
                if args.len() == 1 {
                    match &args[0] {
                        Type::NUMERIC(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "list index out of range", callee.line);
                                return Err(Error::STRING("IndexError".into()));
                            }
                            let ret = list.borrow_mut().remove(index);
                            return Ok(ret);
                        },
                        _ => {
                            crate::error("TypeError", "invalid index type", callee.line);
                            return Err(Error::STRING("TypeError".into()));
                        }
                    }
                } else {
                    crate::error("TypeError", &format!("`index` takes 1 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::SET(list) => {
                if args.len() == 2 {
                    match &args[0] {
                        Type::NUMERIC(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "list index out of range", callee.line);
                                return Err(Error::STRING("IndexError".into()));
                            }
                            list.borrow_mut()[index] = args[1].clone();
                            return Ok(Type::NIL);
                        },
                        _ => {
                            crate::error("TypeError", "invalid index type", callee.line);
                            return Err(Error::STRING("TypeError".into()));
                        }
                    }
                } else {
                    crate::error("TypeError", &format!("`index` takes 2 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
            NativeFunction::NEXT(list, i) => {
                if args.len() == 0 {
                    let index = i.get();
                    if index == list.borrow().len() {
                        i.set(0);
                        return Err(Error::STOPITERATION);
                    }
                    i.set(index + 1);
                    return Ok(list.borrow()[index].clone());
                } else {
                    crate::error("TypeError", &format!("`next` takes 0 positional arguments, {} were provided", args.len()), callee.line);
                    return Err(Error::STRING("function arity error".into()));
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum NativeClass {
    LIST,
}

impl Callable for NativeClass {
    fn call(&self, args: Vec<Type>, env: Rc<RefCell<Environment>>, callee: Token) -> Result<Type, Error> {
        match self {
            NativeClass::LIST => {
                let list = Rc::new(RefCell::new(args.clone()));
                let class = Class::new("list".to_string(), vec![].into());
                let mut fields: HashMap<String, Box<Type>> = HashMap::new();
                fields.insert("index".to_string(), Box::new(Type::NATIVE(NativeFunction::INDEX(list.clone()))));
                fields.insert("add".to_string(), Box::new(Type::NATIVE(NativeFunction::ADD(list.clone()))));
                fields.insert("len".to_string(), Box::new(Type::NATIVE(NativeFunction::LEN(list.clone()))));
                fields.insert("remove".to_string(), Box::new(Type::NATIVE(NativeFunction::REMOVE(list.clone()))));
                fields.insert("set".to_string(), Box::new(Type::NATIVE(NativeFunction::SET(list.clone()))));
                let i = Rc::new(Cell::new(0usize));
                fields.insert("next".to_string(), Box::new(Type::NATIVE(NativeFunction::NEXT(list.clone(), i.clone()))));
                Ok(Type::OBJECT(Object::new(class, fields)))
            }
        }
    }
}