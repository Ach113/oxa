use crate::tokens::{Token, TokenType};
use crate::AST::{Expr, Binary, Unary, Grouping, Literal};

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
        if vec![TokenType::IDENTIFIER, TokenType::NUMBER, TokenType::STRING, TokenType::NIL].iter().any(|x| self.check_type(x)) {
            return Ok(Box::new(Literal::new(self.advance()).unwrap()));
        }

        if self.check_type(&TokenType::LEFT_PAREN) {
            self.advance();
            match self.expression() {
                Ok(expr) => {
                    self.advance();
                    self.consume(TokenType::RIGHT_PAREN, "Expected ')' after expression");
                    return Ok(Box::new(Grouping::new(expr)));
                },
                Err(e) => return Err(e)
            }
            
        }
        // this line is not supposed to execute
        Err("Expected expression".into())
    }

    /*** methods for error handling ***/

    fn consume(&mut self, t: TokenType, message: &str) {
        if self.check_type(&t) {
            self.advance();
        } else {
            crate::error("Syntax error", message, self.previous().line);
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