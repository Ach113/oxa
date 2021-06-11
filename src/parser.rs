use crate::tokens::{Token, TokenType};
use crate::AST::{Expr, Binary, Unary, Grouping, Literal, Variable, Stmt, ExprStmt, PrintStmt, VarDeclaration};

// operators supported by each type of expression
const equalities: [TokenType; 2] = [TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL];
const comparisons: [TokenType; 4] = [TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL];
const terms: [TokenType; 2] = [TokenType::PLUS, TokenType::MINUS];
const factors: [TokenType; 2] = [TokenType::STAR, TokenType::SLASH];
const unaries: [TokenType; 2] = [TokenType::BANG, TokenType::MINUS];

type Error = Box<dyn std::error::Error>;

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

impl Parser {
    // constructor
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {tokens: tokens, current: 0}
    }
    /*** helper functions ***/

    // checks if parser has reached the end
    fn at_end(&self) -> bool {
        (self.current as usize) == self.tokens.len() 
    }

    // returns token at index "current"
    fn peek(&self) -> Token {
        self.tokens[self.current as usize].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[(self.current - 1) as usize].clone()
    }

    // returns current token, increments index
    fn advance(&mut self) -> Token {
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

    pub fn parse(&mut self) -> Vec<Box<dyn Stmt>> {
        let mut stmt_vec: Vec<Box<dyn Stmt>> = Vec::new();
        // iterate until EOF
        while !self.at_end() {
            let stmt = self.declaration();
            if stmt.is_ok() {
                stmt_vec.push(stmt.unwrap());
            }
        }
        stmt_vec
    }

    // variable, function or class declarations
    fn declaration(&mut self) -> Result<Box<dyn Stmt>, ()> {
        let keyword = self.peek().t;
        let dec = match keyword {
            TokenType::VAR => self.var_declaration(),
            _ => self.statement()
        };
        if dec.is_err() {
            self.synchronize();
            Err(())
        } else {
            dec
        }
    }

    // var identifier = expr;
    fn var_declaration(&mut self) -> Result<Box<dyn Stmt>, ()> {
        self.advance(); // consume var token
        let identifier = self.consume(TokenType::IDENTIFIER, "Expect identifier");
        if identifier.is_err() {
            return Err(());
        }
        // initial value of the variable (null)
        let mut value: Box<dyn Expr> = Box::new(Literal::new(Token::new("nil".to_string(), crate::tokens::Literal::NIL, TokenType::NIL, self.previous().line)).unwrap());
        // check if any assignment is being performed
        let equal_sign = self.consume(TokenType::EQUAL, "Expect assignment");
        if equal_sign.is_ok() {
            let expr = self.expression();
            if expr.is_err() {
                crate::error("SyntaxError", "Invalid variable assignment", self.previous().line);
                return Err(());
            } else {
                value = expr.unwrap();
            }
        }
        // check for semicolon
        let res = self.consume(TokenType::SEMICOLON, "Expect ';' after expression");
        if res.is_ok() {
            Ok(Box::new(VarDeclaration::new(identifier.unwrap(), value)))
        } else {
            Err(())
        }
    }

    fn statement(&mut self) -> Result<Box<dyn Stmt>, ()> {
        let token = self.peek().t;
        match token {
            TokenType::PRINT => self.print_statement(),
            _ => self.expression_stmt()
        }
    }

    // print expr;
    fn print_statement(&mut self) -> Result<Box<dyn Stmt>, ()> {
        self.advance(); // consume print token
        let expr = self.expression().unwrap();
        let res = self.consume(TokenType::SEMICOLON, "Expect ';' after expression");
        if res.is_ok() {
            Ok(Box::new(PrintStmt::new(expr)))
        } else {
            Err(())
        }
    }

    fn expression_stmt(&mut self) -> Result<Box<dyn Stmt>, ()> {
        let expr = self.expression().unwrap();
        let res = self.consume(TokenType::SEMICOLON, "Expect ';' after expression");
        if res.is_ok() {
            Ok(Box::new(ExprStmt::new(expr)))
        } else {
            Err(())
        }
    }

    /*** Expressions ***/

    pub fn expression(&mut self) -> Result<Box<dyn Expr>, Error> {
        self.equality()
    }

    // expression with lowest precendence
    fn equality(&mut self) -> Result<Box<dyn Expr>, Error> {
        match self.comparison() {
            Ok(mut expr) => {
                while equalities.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.comparison() {
                        Ok(right) => Box::new(Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    // since term expressions are also binary, this code looks identical to that of equality(), albeit with lower precendence
    fn comparison(&mut self) -> Result<Box<dyn Expr>, Error> {
        match self.term() {
            Ok(mut expr) => {
                while comparisons.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.term() {
                        Ok(right) => Box::new(Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn term(&mut self) -> Result<Box<dyn Expr>, Error> {
        match self.factor() {
            Ok(mut expr) => {
                while terms.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.factor() {
                        Ok(right) => Box::new(Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn factor(&mut self) -> Result<Box<dyn Expr>, Error> {
        match self.unary() {
            Ok(mut expr) => {
                while factors.iter().any(|x| self.check_type(x)) {
                    self.advance();
                    let operator = self.previous();
                    expr = match self.unary() {
                        Ok(right) => Box::new(Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            },
            Err(e) => Err(e),
        }
    }

    fn unary(&mut self) -> Result<Box<dyn Expr>, Error> {
        if unaries.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator = self.previous();
            match self.unary() {
                Ok(right) => return Ok(Box::new(Unary::new(operator, right).unwrap())),
                Err(right) => return Err(right),
            }
        }
        match self.primary() {
            Ok(x) => return Ok(x),
            Err(x) => return Err(x),
        } 
    }

    // either returns literal value of the expression or another expression wrapped in parentheses
    fn primary(&mut self) -> Result<Box<dyn Expr>, Error> {
        // literal types
        if vec![TokenType::NUMBER, TokenType::STRING, TokenType::NIL].iter().any(|x| self.check_type(x)) {
            return Ok(Box::new(Literal::new(self.advance()).unwrap()));
        }
        // variable
        if self.check_type(&TokenType::IDENTIFIER) {
            return Ok(Box::new(Variable::new(self.advance())));
        }

        if self.check_type(&TokenType::LEFT_PAREN) {
            self.advance();
            match self.expression() {
                Ok(expr) => {
                    match self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression") {
                        Err(e) => return Err(e),
                        _ => return Ok(Box::new(Grouping::new(expr))),
                    };
                },
                Err(e) => return Err(e)
            }
            
        }
        // this line is not supposed to execute
        Err("Expected expression".into())
    }

    /*** methods for error handling ***/

    fn consume(&mut self, t: TokenType, message: &str) -> Result<Token, Error> {
        if self.check_type(&t) {
            Ok(self.advance())
        } else {
            crate::error("Syntax error", message, self.previous().line);
            Err(message.into())
        }
    }

    // when error is encountered, consumes all tokens until the start of new statement, to avoid redundant errors
    fn synchronize(&mut self) {
        self.advance();

        while !self.at_end() {
            if self.previous().t == TokenType::SEMICOLON { return; }

            // start of new statement is indicated by following token types
            match (self.peek().t) {
                TokenType::CLASS | TokenType::FUN | TokenType::VAR | TokenType::FOR | TokenType::IF | TokenType::WHILE | TokenType::PRINT | TokenType::RETURN => {
                    return;
                },
                _ => { self.advance(); }
            }
        }
    }
}