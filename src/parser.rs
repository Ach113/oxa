use std::any::type_name;

use crate::tokens::{Token, TokenType};
use crate::AST;
use crate::AST::Eval;
use crate::types::{Type, Function};

// operators supported by each type of expression
const EQUALITIES: [TokenType; 2] = [TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL];
const COMPARISONS: [TokenType; 4] = [TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL];
const TERMS: [TokenType; 2] = [TokenType::PLUS, TokenType::MINUS];
const FACTORS: [TokenType; 3] = [TokenType::STAR, TokenType::SLASH, TokenType::PERCENT];
const UNARIES: [TokenType; 2] = [TokenType::BANG, TokenType::MINUS];

fn type_of<T>(_: T) -> &'static str {
    type_name::<T>()
}

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
    loop_counter: u32,
    function_counter: u32,
}

impl Parser {
    // constructor
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {tokens: tokens, current: 0, loop_counter: 0, function_counter: 0}
    }
    /*** helper functions ***/

    // checks if parser has reached the end
    fn at_end(&self) -> bool {
        (self.current as usize) >= self.tokens.len()
    }

    // returns token at index "current"
    fn peek(&self) -> Token {
        self.tokens[self.current as usize].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[(self.current - 1) as usize].clone()
    }

    // checks if passed tokentype matches next token
    fn next(&self, t: TokenType) -> bool {
        if (self.current as usize) >= self.tokens.len() - 1 {
            false
        } else {
            self.tokens[(self.current + 1) as usize].t == t
        }
    }

    // returns current token, increments index
    fn advance(&mut self) -> Token {
        //println!("{}", self.peek());
        if !self.at_end() {
            self.current += 1;
        }
        self.previous()
    }

    // check if passed token type matches that of a token at current index
    fn check_type(&self, t: &TokenType) -> bool {
        if self.at_end() {
            return false;
        }
        self.peek().t == *t
    }

    /*** Statements ***/

    pub fn parse(&mut self) -> Result<Vec<Box<dyn Eval>>, String> {
        let mut stmt_vec: Vec<Box<dyn Eval>> = Vec::new();
        let mut error = false;
        // iterate until EOF
        while !self.at_end() {
            // declarations have highest precendance
            match self.declaration() {
                Ok(stmt) => stmt_vec.push(stmt),
                Err(_) => {
                    error = true
                },
            }
        }
        if error {
            return Err("ParsingError".into());
        }
        Ok(stmt_vec)
    }

    // variable, function or class declarations
    // otherwise matches other statement types
    fn declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        let keyword = self.peek().t;
        match keyword {
            TokenType::VAR => self.var_declaration(),
            TokenType::FUN => {
                match self.function_declaration() {
                    Err(e) => Err(e),
                    Ok(f) => Ok(Box::new(f)),
                }
            },
            TokenType::CLASS => self.class_declaration(),
            _ => self.statement()
        }
    }

    // "class" identifier "{" function* "}"
    fn class_declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume "class" token
        let identifier = self.consume(TokenType::IDENTIFIER, "Expect identifier after class declaration")?;
        self.consume(TokenType::LEFT_BRACE, "Expect '{' before class body")?;

        let mut methods: Vec<Function> = Vec::new();
        while !self.check_type(&TokenType::RIGHT_BRACE) {
            methods.push((self.function_declaration()?).f.clone());
        }
        self.consume(TokenType::RIGHT_BRACE, "Expect '}' after class body")?;
        Ok(Box::new(AST::ClassDeclr::new(identifier, methods)))
    }

    // "fun" identifier "(" args[] ")" block_stmt
    fn function_declaration(&mut self) -> Result<AST::FunDeclaration, String> {
        self.advance(); // consume "fun" token
        self.function_counter += 1;
        let identifier = self.consume(TokenType::IDENTIFIER, "Expect identifier after function declaration")?;
        self.consume(TokenType::LEFT_PAREN, "Expect '(' after function declaration")?;

        let mut arguments: Vec<Token> = Vec::new();
        // for functions with no arguments, this statement is skipped
        while !self.check_type(&TokenType::RIGHT_PAREN) {
            if arguments.len() >= (255 as usize) {
                crate::error("ParsingError", "Max argument len reached (255)", self.peek().line);
                return Err("Max argument len reached (255)".into());
            }
            arguments.push(self.advance());
            while self.check_type(&TokenType::COMMA) {
                self.advance(); // consume ','
                arguments.push(self.advance());
            }
        }
        self.consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments")?;
        let fbody = self.block_statement()?;
        self.function_counter -= 1;
        
        Ok(AST::FunDeclaration::new(identifier, arguments, fbody))
    }

    // var identifier = expr; | var identifier;
    fn var_declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume var token
        let identifier = self.consume(TokenType::IDENTIFIER, "Expect identifier")?;

        // initial value of the variable (null)
        let mut value: Box<dyn Eval> = Box::new(AST::Literal::new(Token::new("nil".to_string(), Type::NIL, TokenType::NIL, self.previous().line)).unwrap());
        // check if any assignment is being performed
        if self.check_type(&TokenType::EQUAL) {
            self.advance(); // consume "=" token
            let expr = self.expression();
            if expr.is_err() {
                crate::error("SyntaxError", "Invalid variable declaration", self.previous().line);
                return Err("Invalid variable declaration".into());
            } else {
                value = expr.unwrap();
            }
        }
        // check for semicolon
        match self.consume(TokenType::SEMICOLON, "Expect ';' after expression") {
            Ok(res) => Ok(Box::new(AST::VarDeclaration::new(identifier, value))),
            Err(e) => Err(e),
        }
    }

    fn statement(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.peek().t {
            TokenType::IF => self.if_statement(),
            TokenType::PRINT => self.print_statement(),
            TokenType::RETURN => self.return_statement(),
            TokenType::WHILE => self.while_loop(),
            TokenType::LEFT_BRACE => {
                match self.block_statement() {
                    Ok(block) => Ok(Box::new(block)),
                    Err(e) => Err(e),
                }
            },
            _ => self.expression_stmt()
        }
    }

    // "while" expression "{" statement "}"
    fn while_loop(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume "while" token
        // condition
        let cond = self.expression();
        // loop body
        self.loop_counter += 1;
        let mut stmt: Option<AST::BlockStmt> = None;
        if self.check_type(&TokenType::LEFT_BRACE) {
            match self.block_statement() {
                Ok(x) => stmt = Some(x),
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            }
        } else {
            crate::error("SyntaxError", "Expected '{' after 'while' condition", self.previous().line);
            self.synchronize();
            return Err("Expected '{' after 'while' condition".into());
        }
        self.loop_counter -= 1;
        match cond {
            Ok(x) => Ok(Box::new(AST::WhileLoop::new(x, stmt.unwrap()))),
            _ => {
                crate::error("SyntaxError", "Invalid condition for while loop", self.peek().line);
                self.synchronize();
                Err("Invalid condition for while loop".into())
            }
        }
    }

    // if expression statement (else statement)?
    fn if_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume 'if' token
        // condition
        let cond = self.expression();
        // statement if true
        let mut stmt: Option<Box<dyn Eval>> = None;
        if self.check_type(&TokenType::LEFT_BRACE) {
            match self.statement() {
                Ok(x) => stmt = Some(x),
                _ => {
                    return Err("invalid statement in 'if' block".into());
                }
            }
        } else {
            crate::error("SyntaxError", "Expected '{' after 'if' condition", self.previous().line);
            return Err("Expected '{' after 'if' condition".into());
        }
        // ELSE statement
        let mut else_stmt: Option<Box<dyn Eval>> = None;
        if self.check_type(&TokenType::ELSE) {
            self.advance(); // consume 'else' token
            if self.check_type(&TokenType::LEFT_BRACE) {
                match self.statement() {
                    Ok(x) => else_stmt = Some(x),
                    _ => {
                        return Err("invalid statement in 'else' block".into());
                    }
                }
            } else {
                crate::error("SyntaxError", "Expected '{' after 'else' keyword", self.previous().line);
                return Err("Expected '{' after 'else' keyword".into());
            }
        }
        // error check
        if cond.is_err() || stmt.is_none() {
            crate::error("ParserError", "invalid if statement", self.peek().line);
            return Err("invalid if statement".into());
        }
        Ok(Box::new(AST::IfStmt::new(cond.unwrap(), stmt.unwrap(), else_stmt)))
    }

    // { declaration* }
    fn block_statement(&mut self) -> Result<AST::BlockStmt, String> {
        self.advance(); // consume '{'

        let mut statements:Vec<Box<dyn Eval>> = Vec::new();

        while !(self.at_end() || self.check_type(&TokenType::RIGHT_BRACE)) {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }

        match self.consume(TokenType::RIGHT_BRACE, "Expect '}' after block") {
            Ok(_) => Ok(AST::BlockStmt::new(statements)),
            Err(e) => Err(e)
        }
    }

    // print expr;
    fn print_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume print token
        let expr = self.expression().unwrap();
        match self.consume(TokenType::SEMICOLON, "Expect ';' after expression") {
            Ok(_) => Ok(Box::new(AST::PrintStmt::new(expr))),
            Err(e) => Err(e),
        }
    }

    fn return_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance(); // consume 'return' token
        if self.function_counter < 1 {
            crate::error("SyntaxError", "return statement outside of function statement", self.peek().line);
            self.synchronize();
            return Err("return statement outside of function".into());
        }
        // default return value nil
        let mut value: Box<dyn Eval> = Box::new(AST::Literal::new(Token::new("nil".to_string(), Type::NIL, TokenType::NIL, self.previous().line)).unwrap());
        if !self.check_type(&TokenType::SEMICOLON) {
            value = self.expression()?;
        }
        self.consume(TokenType::SEMICOLON, "Expect ';' after expression")?;
        Ok(Box::new(AST::Return::new(value)))
    }

    fn expression_stmt(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.expression() {
            Err(e) => Err(e),
            Ok(expr) => {
                if self.check_type(&TokenType::SEMICOLON) {
                    self.advance();
                }
                Ok(Box::new(AST::ExprStmt::new(expr)))
            },
        }
    }

    /*** Expressions ***/

    pub fn expression(&mut self) -> Result<Box<dyn Eval>, String> {
        self.assignment()
    }

    // expression with lowest precendence
    fn assignment(&mut self) -> Result<Box<dyn Eval>, String> {
        let t = self.peek();
        let ret_index = self.current; // index to jump to

        let expr = self.logical_or()?;
        if self.check_type(&TokenType::EQUAL) {
            let equals = self.advance(); // '='

            if expr.get_type() == String::from("Variable") {
                match self.assignment() {
                    Err(e) => {
                        crate::error("SyntaxError", "invalid rhs for assignment", equals.line);
                        return Err("invalid rhs for assignment".into());
                    },
                    Ok(rhs) => { 
                        return Ok(Box::new(AST::Assignment::new(t, rhs)));
                    },
                }
            } else if expr.get_type() == String::from("Getter") {
                self.current = ret_index;
                let expr = self.primary()?;
                self.advance(); // consume '.'
                let name = self.consume(TokenType::IDENTIFIER, "Expect identifier after '.'")?;
                self.advance(); // consume '='
                match self.assignment() {
                    Err(e) => {
                        crate::error("SyntaxError", "invalid rhs for assignment", equals.line);
                        return Err("invalid rhs for assignment".into());
                    },
                    Ok(rhs) => { 
                        return Ok(Box::new(AST::Set::new(name, expr, rhs)));
                    },
                }
            } else {
                crate::error("SyntaxError", "invalid target variable for assignment", equals.line);
                return Err("invalid target variable for assignment".into());
            }
        } else {
            return Ok(expr);
        }
    }

    fn logical_or(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.logical_and() {
            Ok(mut expr) => {
                while self.check_type(&TokenType::OR) || self.check_type(&TokenType::XOR) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.logical_and() {
                        Ok(right) => Box::new(AST::LogicalExpr::new(operator, expr, right).unwrap()),
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn logical_and(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.equality() {
            Ok(mut expr) => {
                while self.check_type(&TokenType::AND) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.equality() {
                        Ok(right) => Box::new(AST::LogicalExpr::new(operator, expr, right).unwrap()),
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    
    fn equality(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.comparison() {
            Ok(mut expr) => {
                while EQUALITIES.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.comparison() {
                        Ok(right) => Box::new(AST::Binary::new(operator, expr, right).unwrap()),
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    // since term expressions are also binary, this code looks identical to that of equality(), albeit with lower precendence
    fn comparison(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.term() {
            Ok(mut expr) => {
                while COMPARISONS.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.term() {
                        Ok(right) => Box::new(AST::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn term(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.factor() {
            Ok(mut expr) => {
                while TERMS.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.factor() {
                        Ok(right) => Box::new(AST::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn factor(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.unary() {
            Ok(mut expr) => {
                while FACTORS.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.unary() {
                        Ok(right) => Box::new(AST::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn unary(&mut self) -> Result<Box<dyn Eval>, String> {
        if UNARIES.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator = self.previous();
            match self.unary() {
                Ok(right) => return Ok(Box::new(AST::Unary::new(operator, right).unwrap())),
                Err(right) => return Err(right),
            }
        }
        match self.function_call() {
            Err(e) => {
                self.synchronize();
                Err(e)
            },
            Ok(expr) => Ok(expr),
        }
    }

    // grammar supports curried functions
    fn function_call(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.primary() {
            Ok(mut expr) => {
                loop {
                    if self.check_type(&TokenType::LEFT_PAREN) {
                        self.advance(); // consume left parenthesis
                        // arg vec
                        let mut arguments: Vec<Box<dyn Eval>> = Vec::new();
                        // for functions with no arguments, this statement is skipped
                        if !self.check_type(&TokenType::RIGHT_PAREN) {
                            if arguments.len() >= (255 as usize) {
                                crate::error("ParsingError", "Max argument len reached (255)", self.peek().line);
                                return Err("Max argument len reached (255)".into());
                            }
                            arguments.push(self.expression()?);
                            while self.check_type(&TokenType::COMMA) {
                                self.advance();
                                arguments.push(self.expression()?);
                            }
                        }
                        let paren = self.consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments")?;
                        expr = Box::new(AST::FunctionCall::new(expr, paren, arguments));
                    } else if self.check_type(&TokenType::DOT) {
                        self.advance(); // consume '.'
                        let name = self.consume(TokenType::IDENTIFIER, "Expect identifier after '.'")?;
                        expr = Box::new(AST::Get::new(name, expr));
                    } else {
                        break;
                    }
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    // either returns literal value of the expression or another expression wrapped in parentheses
    fn primary(&mut self) -> Result<Box<dyn Eval>, String> {
        // literal types
        if vec![TokenType::NUMBER, TokenType::STRING, TokenType::NIL, TokenType::TRUE, TokenType::FALSE].iter().any(|x| self.check_type(x)) {
            return Ok(Box::new(AST::Literal::new(self.advance()).unwrap()));
        }
        // variable
        if self.check_type(&TokenType::IDENTIFIER) {
            return Ok(Box::new(AST::Variable::new(self.advance())));
        }

        // braces
        if self.check_type(&TokenType::LEFT_BRACE) {
            match self.block_statement() {
                Ok(block) => return Ok(Box::new(block)),
                Err(_) => return Err("Invalid Expression".into()),
            }
        }

        // parenthesis
        if self.check_type(&TokenType::LEFT_PAREN) {
            self.advance();
            match self.expression() {
                Ok(expr) => {
                    match self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression") {
                        Err(e) => return Err(e),
                        _ => return Ok(Box::new(AST::Grouping::new(expr))),
                    };
                },
                Err(e) => return Err(e)
            }
        }
        // break statement
        if self.check_type(&TokenType::BREAK) {
            self.advance(); // consume break token
            match self.consume(TokenType::SEMICOLON, "Expected ';' after statement") {
                Err(e) => return Err(e),
                _ => {
                    if self.loop_counter == 0 {
                        crate::error("SyntaxError", "'break' outside loop", self.previous().line);
                        return Err("'break' outside loop".into());
                    }
                    return Ok(Box::new(AST::Break::new()));
                },
            };
        }

        // continue statement
        if self.check_type(&TokenType::CONTINUE) {
            self.advance(); // consume continue token
            match self.consume(TokenType::SEMICOLON, "Expected ';' after statement") {
                Err(e) => return Err(e),
                _ => {
                    if self.loop_counter == 0 {
                        crate::error("SyntaxError", "'continue' outside loop", self.previous().line);
                        return Err("'continue' outside loop".into());
                    }
                    return Ok(Box::new(AST::Continue::new()));
                },
            };
        }
        // this line is not supposed to execute
        crate::error("SyntaxError", &format!("expected expression, got {}", self.peek()), self.peek().line);
        Err(format!("Expected expression, got {}", self.peek()).into())
    }

    /*** methods for error handling ***/

    fn consume(&mut self, t: TokenType, message: &str) -> Result<Token, String> {
        if self.check_type(&t) {
            Ok(self.advance())
        } else {
            crate::error("Syntax error", message, self.previous().line);
            Err(message.into())
        }
    }

    // when error is encountered, consumes all tokens until the start of new statement, to avoid redundant errors
    fn synchronize(&mut self) {
        while !self.at_end() {
            // start of new statement is indicated by following token types
            match self.peek().t {
                TokenType::CLASS | TokenType::FUN | TokenType::VAR | TokenType::FOR | TokenType::IF | TokenType::WHILE | TokenType::PRINT | TokenType::RETURN => {
                    return;
                },
                _ => { self.advance(); },
            }
        }
    }
}