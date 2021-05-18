use crate::tokens::{Token, TokenType};
use crate::AST::{Expr, Binary, Unary, Grouping, Literal};

// operators supported by each type of expression
const equalities: [TokenType; 2] = [TokenType::BANG_EQUAL, TokenType::EQUAL_EQUAL];
const comparisons: [TokenType; 4] = [TokenType::GREATER, TokenType::GREATER_EQUAL, TokenType::LESS, TokenType::LESS_EQUAL];
const terms: [TokenType; 2] = [TokenType::PLUS, TokenType::MINUS];
const factors: [TokenType; 2] = [TokenType::STAR, TokenType::SLASH];
const unaries: [TokenType; 2] = [TokenType::BANG, TokenType::MINUS];

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
}

impl Parser {
    /*** helper functions ***/

    // checks if parser has reached the end
    fn is_end(&self) -> bool {
        (self.current as usize) == self.tokens.len() 
    }

    // returns token at index "current"
    fn peek(&self) -> Token {
        self.tokens[self.current as usize]
    }

    fn previous(&self) -> Token {
        self.tokens[(self.current - 1) as usize]
    }

    // returns current token, increments index
    fn advance(&mut self) -> Token {
        if !self.is_end() {
            self.current += 1;
        }
        self.previous()
    }

    // check if passed token type matches that of a token at current index
    fn check_type(&self, t: &TokenType) -> bool {
        if self.is_end() {
            return false;
        }
        self.peek().t == *t
    }

    fn consume(&self, t: TokenType, message: &str) {
        todo!();
    }

    /*** Expressions ***/

    pub fn expression(&self) -> Box<dyn Expr> {
        self.equality()
    }

    // expression with lowest precendence
    fn equality(&self) -> Box<dyn Expr> {
        // by default an expression of lower precendence is returned
        let mut expr = self.comparison();

        // entering this loop means that parser has encountered an equality operation
        // while current token is "==" or "!="
        while equalities.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator: Token = self.previous();
            let right = self.comparison();
            expr = Box::new(Binary::new(operator, *expr, *right).unwrap());
        }
        expr
    }

    fn comparison(&self) -> Box<dyn Expr> {
        todo!();
    }

    /*
    // since term expressions are also binary, this code looks identical to that of equality(), albeit with lower precendence
    fn comparison(&self) -> Box<dyn Expr> {
        let mut expr = self.term();

        while comparisons.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator: Token = self.previous();
            let right = self.term();
            expr = Box::new(Binary::new(operator, expr, right).unwrap());
        }
        expr
    }

    fn term(&self) -> Box<dyn Expr> {
        let mut expr = self.factor();

        while terms.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator: Token = self.previous();
            let right = self.factor();
            expr = Box::new(Binary::new(operator, expr, right).unwrap());
        }
        expr
    }

    fn factor(&self) -> Box<dyn Expr> {
        let mut expr = self.unary();

        while factors.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator: Token = self.previous();
            let right = self.unary();
            expr = Box::new(Binary::new(operator, expr, right).unwrap());
        }
        expr
    }

    fn unary(&self) -> Box<dyn Expr> {
        while unaries.iter().any(|x| self.check_type(x)) {
            self.advance();
            let operator: Token = self.previous();
            let right = self.unary();
            return Box::new(Unary::new(operator, right).unwrap());
        }
        self.primary().unwrap()
    }

    // either returns literal value of the expression or another expression wrapped in parentheses
    fn primary(&self) -> Option<Box<dyn Expr>> {
        if vec![TokenType::IDENTIFIER, TokenType::NUMBER, TokenType::STRING, TokenType::NIL].iter().any(|x| self.check_type(x)) {
            return Some(Box::new(Literal::new(self.advance()).unwrap()));
        }

        if self.check_type(&TokenType::LEFT_PAREN) {
            let mut expr = self.expression();
            self.consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
            return Some(Box::new(Grouping::new(*expr)));
        }
        // this line is not supposed to execute
        Box::new(None)
    }
    */
}