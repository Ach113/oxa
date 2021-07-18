use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;
use std::collections::HashMap;

use crate::types::{Type, Function, Callable, Class, Object};
use crate::tokens::{Token, TokenType};
use crate::environment::Environment;
use crate::interpreter::interpret;

// AST error type
#[derive(Debug)]
pub enum Error {
    STRING(String),
    BREAK,
    CONTINUE,
    RETURN(Type),
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error>;

    fn get_type(&self) -> String;
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
    value: Type
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
    identifier: Token,
    value: Box<dyn Eval>
}


/*** eval implementations ***/

impl Eval for Binary {
    
    fn get_type(&self) -> String {
        String::from("BinExpr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let op = &self.operator.lexeme;
        let left = self.left.eval(env.clone())?;
        let right = self.right.eval(env.clone())?;

        match &**op {
            "==" => {
                Ok(Type::BOOL(left == right))
            },
            "!=" => {
                Ok(Type::BOOL(left != right))
            },
            ">=" => {
                Ok(Type::BOOL(left >= right))
            },
            "<=" => {
                Ok(Type::BOOL(left <= right))
            },
            ">" => {
                Ok(Type::BOOL(left > right))
            }
            "<" => {
                Ok(Type::BOOL(left < right))
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

    fn get_type(&self) -> String {
        String::from("UnaryExpr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(env.clone())?;
        match &**op {
            "-" => {
                match expr {
                    Type::NUMERIC(x) => Ok(Type::NUMERIC(-x)),
                    _ => {
                        let error_message = format!("invalid right hand side expression for operator {}", op);
                        crate::error("Parsing error", &error_message, self.operator.line);  
                        Err(Error::STRING(error_message.into())) 
                    }
                }
            },
            "!" => {
                match expr {
                    Type::BOOL(x) => Ok(Type::BOOL(!x)),
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

    fn get_type(&self) -> String {
        String::from("Literal")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Ok(self.value.clone())
    }
}

impl Eval for Grouping {
    fn get_type(&self) -> String {
        String::from("Grouping")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        self.expression.eval(env)
    }
} 

impl Eval for Variable {
    fn get_type(&self) -> String {
        format!("Variable: {}", self.identifier.lexeme)
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        env.borrow().get(self.identifier.clone())
    }
}

impl Eval for Assignment {
    fn get_type(&self) -> String {
        String::from("Assignment")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let value = self.value.eval(env.clone())?; 
        println!("{}", self.identifier.clone());
        env.borrow_mut().assign(self.identifier.clone(), value.clone())
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
    pub fn new(identifier: Token, value: Box<dyn Eval>) -> Self {
        Assignment {identifier, value}
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
    fn get_type(&self) -> String {
        String::from("LogicalExpr")
    }
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
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
pub struct Call {
    identifier: Box<dyn Eval>, // function identifier
    paren: Token,
    arguments: Vec<Box<dyn Eval>>
}

impl Call {
    pub fn new(identifier: Box<dyn Eval>, paren: Token, arguments: Vec<Box<dyn Eval>>) -> Self {
        Call {identifier, paren, arguments}
    }
}

impl Eval for Call {
    fn get_type(&self) -> String {
        String::from("Call")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let mut args: Vec<Type> = Vec::new();
        // evaluate the list of arguments
        for arg in &self.arguments {
           args.push(arg.eval(env.clone())?);
        }
        let callable: Box<dyn Callable> = match self.identifier.eval(env.clone()) {
            Ok(obj) => {
                match obj {
                    Type::FUN(f) => Box::new(f),
                    Type::CLASS(c) => Box::new(c),
                    Type::METHOD(m) => {
                        if let Some(obj) = &m.self_ {
                            args.insert(0, Type::OBJECT(obj.clone()));
                        }
                        Box::new(m)
                    },
                    _ => {
                        crate::error("TypeError", &format!("type '{}' is not callable", obj.get_type()), self.paren.line);
                        return Err(Error::STRING("type not callable".into()));
                    },
                }
            },
            Err(e) => return Err(e),
        };
        (*callable).call(args, env.clone(), self.paren.clone())
    }
}

// class property access
pub struct Get {
    pub name: Token,
    object: Box<dyn Eval>, 
}

impl Get {
    pub fn new(name: Token, object: Box<dyn Eval>) -> Self {
        Get {name, object}
    }
}

impl Eval for Get {
    fn get_type(&self) -> String {
        String::from("Getter")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        // "get" the field
        match self.object.eval(env.clone()) {
            Ok(obj) => {
                match obj {
                    Type::OBJECT(x) => {
                        match x.get(&self.name)? {
                            // bind "self" to the method
                            // if self.object evaluates to variable, it can be modified, otherwise its read only (all changes get discarded)
                            Type::METHOD(mut f) => {
                                if self.object.get_type().contains("Variable") {
                                    f.bind_self(x, Some(self.object.get_type()[10..].to_string()));
                                } else {
                                    f.bind_self(x, None);
                                }
                                Ok(Type::METHOD(f))
                            },
                            _ => x.get(&self.name),
                        }
                    },
                    _ => {
                        crate::error("TypeError", &format!("type '{}' does not have attributes", obj.get_type()), self.name.line);
                        Err(Error::STRING("type has no attributes".into()))
                    },
                }
            },
            Err(e) => Err(e),
        }
    }
}

// class property modifier
pub struct Set {
    name: Token,
    object_id: Token,
    object: Box<dyn Eval>,
    value: Box<dyn Eval>,
}

impl Set {
    pub fn new(name: Token, object: Box<dyn Eval>, value: Box<dyn Eval>) -> Self {
        let object_id = &object.get_type()[10..];
        let object_id = Token::new(object_id.to_string(), Type::STRING(object_id.to_string()), TokenType::IDENTIFIER, name.line);
        Set {name, object_id, object, value}
    }
}

impl Eval for Set {
    fn get_type(&self) -> String {
        String::from("Setter")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.object.eval(env.clone()) {
            Ok(mut obj) => {
                match obj {
                    Type::OBJECT(mut x) => {
                        let value = self.value.eval(env.clone())?;
                        env.borrow_mut().assign(self.object_id.clone(), x.set(self.name.lexeme.clone(), value.clone())?);
                        Ok(value)
                    },
                    _ => {
                        crate::error("TypeError", &format!("type '{}' does not have attributes", obj.get_type()), self.name.line);
                        return Err(Error::STRING("type has no attributes".into()));
                    },
                }
            },
            Err(e) => return Err(e),
        }
    }
}

/* self */
pub struct Self_ {
    identifier: Token
}

impl Self_ {
    pub fn new(identifier: Token) -> Self {
        Self_{identifier}
    }
}

impl Eval for Self_ {
    fn get_type(&self) -> String {
        format!("Variable: {}", self.identifier.lexeme)
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match &env.borrow().enclosing {
            Some(env) => env.borrow().get(self.identifier.clone()),
            None => Err(Error::STRING("'self' outside of class body".to_string())),
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
    fn get_type(&self) -> String {
        String::from("ExprStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
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
    fn get_type(&self) -> String {
        String::from("PrintStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.value.eval(env) {
            Ok(val) => {
                println!("{}", val);
                Ok(Type::NIL)
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
    fn get_type(&self) -> String {
        String::from("VarDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => {
                match env.borrow_mut().add(self.identifier.clone(), x) {
                    Err(e) => Err(e),
                    Ok(_) => Ok(Type::NIL),
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
    fn get_type(&self) -> String {
        String::from("BlockStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
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
    fn get_type(&self) -> String {
        String::from("IfStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        if self.condition.eval(env.clone()).unwrap() == Type::BOOL(true) {
            return self.expr.eval(env);
        }
        match &self.else_stmt {
            Some(stmt) => stmt.eval(env),
            None => Ok(Type::NIL), 
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
    fn get_type(&self) -> String {
        String::from("WhileLoop")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let mut ret: Result<Type, Error> = Ok(Type::NIL);
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        while self.condition.eval(env.clone())? == Type::BOOL(true) {
            for stmt in &self.body.statements {
                ret = stmt.eval(enclosing.clone());
                match ret {
                    Ok(_) => {},
                    Err(e) => {
                        match e {
                            Error::BREAK => return Ok(Type::NIL),
                            Error::CONTINUE => break,
                            _ => return Err(e),
                        }
                    },
                }
            }
        }
        Ok(Type::NIL)
    }
}

// break statement
pub struct Break {
    
}

impl Break {
    pub fn new() -> Self {
        Break {}
    }
}

impl Eval for Break {
    fn get_type(&self) -> String {
        String::from("BreakStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
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
    fn get_type(&self) -> String {
        String::from("ContinueStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Err(Error::CONTINUE)
    }
}

// function declaration

pub struct FunDeclaration {
    identifier: Token,
    pub f: Function,
}

impl FunDeclaration {
    pub fn new(identifier: Token, args: Vec<Token>, body: BlockStmt) -> Self {
        let args: Vec<String> = args.iter().map(|x| x.lexeme.clone()).collect();
        let f = Function::new(identifier.lexeme.clone(), identifier.line, Rc::new(RefCell::new(body)), args);
        FunDeclaration {identifier, f}
    }
}

impl Eval for FunDeclaration {
    fn get_type(&self) -> String {
        String::from("FunDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        // add function declaration to the symbol table
        env.borrow_mut().add(self.identifier.clone(), Type::FUN(self.f.clone()))?;
        Ok(Type::NIL)
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
    fn get_type(&self) -> String {
        String::from("ReturnStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Err(Error::RETURN(self.expr.eval(env.clone())?))
    }
}

// class declaration
pub struct ClassDeclr {
    name: Token,
    class: Class,
}

impl ClassDeclr {
    pub fn new(name: Token, methods: Vec<Function>) -> Self {
        let class = Class::new(name.lexeme.clone(), methods);
        ClassDeclr {name: name.clone(), class}
    }
}

impl Eval for ClassDeclr {
    fn get_type(&self) -> String {
        String::from("ClassDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        env.borrow_mut().add(self.name.clone(), Type::CLASS(self.class.clone()))?;
        Ok(Type::NIL)
    }
}

// import statement
pub struct Import {
    module: Token
}

impl Import {
    pub fn new(module: Token) -> Self {
        Import {module}
    }
}

impl Eval for Import {
    fn get_type(&self) -> String {
        String::from("Import")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let module_name = format!("{}.oxa", self.module.lexeme);
        match std::fs::read_to_string(module_name.clone()) {
            Ok(code) => {
                // create class <module>
                let methods: Vec<Function> = Vec::new();
                let class = Class::new(String::from("__MODULE__"), methods);
                let t = Token::new(String::from("__MODULE__"), Type::NIL, TokenType::NIL, self.module.line);
                env.borrow_mut().add(t, Type::CLASS(class.clone()))?;
                // create new environment for the module and execute module code
                let module_env = Rc::new(RefCell::new(Environment::new(None)));
                match crate::run(code, module_env.clone()) {
                    Ok(_) => {},
                    Err(e) => {
                        return Err(Error::STRING(e));
                    }
                }
                // bring symbol table of module inside current scope
                let mut fields: HashMap<String, Box<Type>> = HashMap::new();
                for (identifier, value) in module_env.borrow().symbol_table.iter() {
                    fields.insert(identifier.clone(), Box::new(value.clone()));
                }
                let object = Type::OBJECT(Object::new(class, fields));
                let t = Token::new(self.module.lexeme.clone(), Type::NIL, TokenType::NIL, self.module.line);
                env.borrow_mut().add(t, object);
                Ok(Type::NIL)
            },
            Err(_) => {
                crate::error("FileNotFound", &format!("file `{}` could not be found", module_name), self.module.line);
                Err(Error::STRING("FileNotFound".into()))
            }
        }
    }
}