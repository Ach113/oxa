use std::fmt;
use core::ops::{Add, Sub, Mul, Div, BitOr, BitAnd, BitXor, Rem};

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Object {
    STRING(String),
    NUMERIC(f64),
    BOOL(bool),
    NIL,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::STRING(x) => write!(f, "{}", x),
            Object::NUMERIC(x) => write!(f, "{}", x),
            Object::BOOL(x) => write!(f, "{}", x),
            Object::NIL => write!(f, ""),
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
            Object::NIL => "nil".to_string(),
        }
    }
}

