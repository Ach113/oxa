use std::error::Error;

use crate::tokens::{TokenType, Token};

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<TokenType>,
    line: u64, // current line no
    start: u64, // start of token which is being scanned
    current: u64, // index of token which is being scanned 
}

impl Scanner {
    /**  helper functions **/
    // returns char at "current"
    pub fn cell(&self) -> String {
        self.source[(self.current - 1) as usize].to_string()
    }

    // checks if lexer has reached source eof
    pub fn is_eof(&self) -> bool {
        self.current >= self.source.len() as u64
    }

    // compares char at index "current" to "current + 1"
    pub fn next(&self, next_char: char) -> bool {
        if self.is_eof() {
            false
        } else {
            self.source[self.current as usize] == next_char
        }
    }

    pub fn get_identifier(&mut self) -> TokenType {
        // identifiers are allowed to contain alphabetic, numeric character and '_' char
        while (self.cell().chars().all(|x| x.is_alphabetic() || x.is_digit(10) || x == '_')) {
            self.current += 1;
            if self.is_eof() { break; }
        }

        self.current -= 1;

        let start = (self.start) as usize;
        let end = (self.current) as usize;
        let identifier: String = self.source[start..end].iter().collect();
        // match the identifier with existing keywords
        match identifier.as_str() {
            "while" => TokenType::WHILE(Token::new(identifier.to_string(), (), self.line)),
            "and" => TokenType::AND(Token::new(identifier.to_string(), (), self.line)),
            "or" => TokenType::OR(Token::new(identifier.to_string(), (), self.line)),
            "class" => TokenType::CLASS(Token::new(identifier.to_string(), (), self.line)),
            "fun" => TokenType::FUN(Token::new(identifier.to_string(), (), self.line)),
            "for" => TokenType::FOR(Token::new(identifier.to_string(), (), self.line)),
            "if" => TokenType::IF(Token::new(identifier.to_string(), (), self.line)),
            "else" => TokenType::ELSE(Token::new(identifier.to_string(), (), self.line)),
            "return" => TokenType::RETURN(Token::new(identifier.to_string(), (), self.line)),
            "true" => TokenType::TRUE(Token::new(identifier.to_string(), (), self.line)),
            "false" => TokenType::FALSE(Token::new(identifier.to_string(), (), self.line)),
            "this" => TokenType::THIS(Token::new(identifier.to_string(), (), self.line)),
            "var" => TokenType::VAR(Token::new(identifier.to_string(), (), self.line)),
            "super" => TokenType::SUPER(Token::new(identifier.to_string(), (), self.line)),
            "print" => TokenType::PRINT(Token::new(identifier.to_string(), (), self.line)),
            "nil" => TokenType::NIL(Token::new(identifier.to_string(), (), self.line)),
            _ => {
                TokenType::IDENTIFIER(Token::new(identifier.to_string(), "".to_string(), self.line))
            },
        }
    }

    pub fn get_number(&mut self) -> Option<f64> {
        
        while (self.cell().chars().all(|x| x.is_digit(10) || x == '.')) {
            self.current += 1;
            if self.is_eof() {
                break;
            }
        }

        self.current -= 1;
       
        // index the string
        let start = (self.start) as usize;
        let end = (self.current) as usize;
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
        while (!self.next('"') && !self.is_eof()) {
            if self.cell() == "\n" {
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
        let end = (self.current - 1) as usize;
        let string: String = self.source[start..end].iter().collect();
        Some(string)
    }

    pub fn test(&self) {
        for token in &self.tokens {
            println!("{:?}", token);
        }
    }

    // scans for an individual token at start location (at function call start == current)
    // throws syntax error if finds an unexpected character
    pub fn scan_token(&mut self) {
        let c = self.source[self.current as usize];
        self.current += 1;

        match (c) {
          
            '=' => self.tokens.push(
                if self.next('=') {
                    self.current += 1;
                    TokenType::EQUAL(Token::new(self.cell(), (), self.line))
                } else {
                    TokenType::EQUAL_EQUAL(Token::new(self.cell(), (), self.line))
                }
            ),
            // special case: '/' stands for division, while // stands for comment
            '/' => {
                if (self.next('/')) {
                    while !(self.is_eof() || self.next('\n')) { 
                        self.current += 1;
                    }
                } else {
                    self.tokens.push(TokenType::SLASH(Token::new(self.cell(), (), self.line)))
                }
            },
            // strings
            '"' => {
                if let Some(s) = self.get_string() {
                    self.tokens.push(TokenType::STRING(Token::new(s.clone(), s.clone(), self.line)));
                }
            },
            '\n' => self.line += 1,
            '\r' | ' ' => {},
            // default
            _ => {
                if c.is_digit(10) {
                    if let Some(n) = self.get_number() {
                        self.tokens.push(TokenType::NUMBER(Token::new(n.to_string(), n, self.line)));
                    }
                } else if c.is_alphabetic() {
                    let t = self.get_identifier();
                    self.tokens.push(t);
                } else {
                    crate::error("Syntax error", &format!("Unexpected character {}", c), self.line);
                }
            },
        }
    }

    // scans for tokens in source file
    pub fn scan_tokens(&mut self) {
        while !self.is_eof() { // while not eof
            self.start = self.current;
            self.scan_token();
        }
    }

    // constructor
    pub fn new(source: String) -> Scanner {
        let chars: Vec<char> = source.chars().collect();
        let tokens: Vec<TokenType> = Vec::new();
        //println!("chars: {:?}", chars);
        Scanner {source: chars, tokens: tokens, line: 1, start: 0, current: 0}
    }
}