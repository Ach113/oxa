use crate::tokens::{TokenType, Token};
use crate::types::Type;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    line: u64, // current line no
    start: u64, // start of token which is being scanned
    current: u64, // index of token which is being scanned 
    multi_line_comment: u32, // keeps track of multi-line comments
}

impl Scanner {
    /**  helper functions **/
    // returns char at "current"
    pub fn cell(&self) -> char {
        self.source[(self.current) as usize]
    }

    // checks if lexer has reached source eof
    pub fn is_eof(&self) -> bool {
        self.current >= self.source.len() as u64
    }

    // checks if passed char is next char (char at index "current + 1")
    pub fn next(&self, next_char: char) -> bool {
        if self.is_eof() {
            false
        } else {
            self.source[(self.current + 1) as usize] == next_char
        }
    }

    pub fn get_identifier(&mut self) -> Token {
        // identifiers are allowed to contain alphabetic, numeric character and '_' char
        while !self.is_eof() && (self.cell().is_alphabetic() || self.cell().is_digit(10) || self.cell() == '_') {
            self.current += 1;
        }

        self.current -= 1;

        let start = (self.start) as usize;
        let end = (self.current + 1) as usize;
        let identifier: String = self.source[start..end].iter().collect();
        // match the identifier with existing keywords
        match identifier.as_str() {
            "while" => Token::new(identifier.to_string(), Type::NIL, TokenType::WHILE, self.line),
            "and" => Token::new(identifier.to_string(), Type::NIL, TokenType::AND, self.line),
            "or" => Token::new(identifier.to_string(), Type::NIL, TokenType::OR, self.line),
            "class" => Token::new(identifier.to_string(), Type::NIL, TokenType::CLASS, self.line),
            "fun" => Token::new(identifier.to_string(), Type::NIL, TokenType::FUN, self.line),
            "for" => Token::new(identifier.to_string(), Type::NIL, TokenType::FOR, self.line),
            "if" => Token::new(identifier.to_string(), Type::NIL, TokenType::IF, self.line),
            "else" => Token::new(identifier.to_string(), Type::NIL, TokenType::ELSE, self.line),
            "return" => Token::new(identifier.to_string(), Type::NIL, TokenType::RETURN, self.line),
            "true" => Token::new(identifier.to_string(), Type::BOOL(true), TokenType::TRUE, self.line),
            "false" => Token::new(identifier.to_string(), Type::BOOL(false), TokenType::FALSE, self.line),
            "self" => Token::new(identifier.to_string(), Type::NIL, TokenType::SELF, self.line),
            "var" => Token::new(identifier.to_string(), Type::NIL, TokenType::VAR, self.line),
            "super" => Token::new(identifier.to_string(), Type::NIL, TokenType::SUPER, self.line),
            "print" => Token::new(identifier.to_string(), Type::NIL, TokenType::PRINT, self.line),
            "nil" => Token::new(identifier.to_string(), Type::NIL, TokenType::NIL, self.line),
            "xor" => Token::new(identifier.to_string(), Type::NIL, TokenType::XOR, self.line),
            "break" => Token::new(identifier.to_string(), Type::NIL, TokenType::BREAK, self.line),
            "continue" => Token::new(identifier.to_string(), Type::NIL, TokenType::CONTINUE, self.line),
            "import" => Token::new(identifier.to_string(), Type::NIL, TokenType::IMPORT, self.line),
            "as" => Token::new(identifier.to_string(), Type::NIL, TokenType::AS, self.line),
            "from" => Token::new(identifier.to_string(), Type::NIL, TokenType::FROM, self.line),
            _ => {
                Token::new(identifier.to_string(), Type::NIL, TokenType::IDENTIFIER, self.line)
            },
        }
    }

    pub fn get_number(&mut self) -> Option<f64> {
        
        while !self.is_eof() && (self.cell().is_digit(10) || self.cell() == '.') {
            self.current += 1;
        }

        self.current -= 1;

        // index the string
        let start = (self.start) as usize;
        let end = (self.current + 1) as usize;
        let s: String = self.source[start..end].iter().collect();
        // check if numeric string can be parsed
        let n = s.trim_end().parse();
        if n.is_err() || s.trim_end().ends_with('.') {
            crate::error("Syntax error", &format!("Invalid numeral {}", s), self.line);
            None
        } else {
            Some(n.unwrap())
        }
    }

    pub fn get_string(&mut self) -> Option<String> {
        // at function call "current" points to first char of string literal
        while !self.next('"') && !self.is_eof() {
            if self.cell() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        // if eof is reached while string has still not been terminated
        if self.is_eof() {
            crate::error("Syntax error", "EOL while scanning string literal", self.line);
            return None;
        } 

        self.current += 1; // advance further to `consume` end of string literal ('"')

        // index the string
        let start = (self.start + 1) as usize;
        let end = (self.current) as usize;
        let string: String = self.source[start..end].iter().collect();
        Some(string)
    }

    // scans for an individual token at start location (at function call start == current)
    // throws syntax error if finds an unexpected character
    pub fn scan_token(&mut self) {

        // handles multi-line comments
        while self.multi_line_comment > 0 {
            if self.cell() == '*' && self.next('/') {
                self.multi_line_comment -= 1;
                self.current += 1;
            } else if self.cell() == '/' && self.next('*') {
                self.multi_line_comment += 1;
                self.current += 1;
            } else if self.cell() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_eof() { return; }
        
        let c = self.cell();
        
        match c {
            // single char tokens
            '(' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::LEFT_PAREN, self.line)),
            ')' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::RIGHT_PAREN, self.line)),
            '{' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::LEFT_BRACE, self.line)),
            '}' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::RIGHT_BRACE, self.line)),
            ',' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::COMMA, self.line)),
            '.' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::DOT, self.line)),
            '-' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::MINUS, self.line)),
            '+' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::PLUS, self.line)),
            ';' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::SEMICOLON, self.line)),
            '*' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::STAR, self.line)),
            '%' => self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::PERCENT, self.line)),
            // two char tokens
            '!' => self.tokens.push(
                if self.next('=') {
                    self.current += 1;
                    Token::new("!=".to_string(), Type::NIL, TokenType::BANG_EQUAL, self.line)
                } else {
                    Token::new(c.to_string(), Type::NIL, TokenType::BANG, self.line)
                }
            ),
            '<' => self.tokens.push(
                if self.next('=') {
                    self.current += 1;
                    Token::new("<=".to_string(), Type::NIL, TokenType::LESS_EQUAL, self.line)
                } else {
                    Token::new(c.to_string(), Type::NIL, TokenType::LESS, self.line)
                }
            ),
            '>' => self.tokens.push(
                if self.next('=') {
                    self.current += 1;
                    Token::new(">=".to_string(), Type::NIL, TokenType::GREATER_EQUAL, self.line)
                } else {
                    Token::new(c.to_string(), Type::NIL, TokenType::GREATER, self.line)
                }
            ),
            '=' => self.tokens.push(
                if self.next('=') {
                    self.current += 1;
                    Token::new("==".to_string(), Type::NIL, TokenType::EQUAL_EQUAL, self.line)
                } else {
                    Token::new(c.to_string(), Type::NIL, TokenType::EQUAL, self.line)
                }
            ),
            // special case: '/' stands for division, while // stands for comment
            '/' => {
                if self.next('/') {
                    while !(self.is_eof() || self.cell() == '\n') { 
                        self.current += 1;
                    }
                    self.line += 1;
                } else if self.next('*') {
                    self.multi_line_comment += 1;
                    self.current += 1;
                } else {
                    self.tokens.push(Token::new(c.to_string(), Type::NIL, TokenType::SLASH, self.line));
                }
            },
            // strings
            '"' => {
                if let Some(s) = self.get_string() {
                    self.tokens.push(Token::new(s.clone(), Type::STRING(s.clone()), TokenType::STRING, self.line));
                }
            },
            '\n' => self.line += 1,
            '\r' | ' ' | '\t' => {},
            // default 
            _ => {
                if c.is_digit(10) {
                    if let Some(n) = self.get_number() {
                        self.tokens.push(Token::new(n.to_string(), Type::NUMERIC(n), TokenType::NUMBER, self.line));
                    }
                } else if c.is_alphabetic() {
                    let t = self.get_identifier();
                    self.tokens.push(t);
                } else {
                    crate::error("Syntax error", &format!("Unexpected character {}", c), self.line);
                }
            },
        }
        self.current += 1; // advance to next char in source code
    }

    // scans for tokens in source file
    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_eof() { 
            self.start = self.current;
            self.scan_token();
        }
        self.tokens.clone()
    }

    // constructor
    pub fn new(source: String) -> Scanner {
        let chars: Vec<char> = source.chars().collect();
        let tokens: Vec<Token> = Vec::new();
        Scanner {source: chars, tokens: tokens, line: 1, start: 0, current: 0, multi_line_comment: 0}
    }
}