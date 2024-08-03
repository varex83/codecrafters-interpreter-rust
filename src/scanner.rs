#![allow(dead_code)]

use std::fmt::{Display, Formatter};

const RESERVED: [&'static str; 16] = [
    "and", "class", "else", "false", "fun", "for", "if", "nil", "or", "print", "return", "super",
    "this", "true", "var", "while",
];

pub struct Scanner<'a> {
    buffer: Buffer<'a>,
    line_number: u32,
    pub errors: Vec<ScannerError>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            buffer: Buffer::new(source),
            line_number: 1,
            errors: vec![],
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.buffer.peek(0) {
            match c {
                // String literals
                '"' => {
                    self.buffer.advance();
                    if let Some(token) = self.scan_string() {
                        tokens.push(token)
                    }
                }

                // Numbers
                c if c.is_ascii_digit() => {
                    if let Some(token) = self.scan_number() {
                        tokens.push(token)
                    }
                }

                // Idents or Reserved words
                c if c.is_alphabetic() || c == '_' => {
                    if let Some(token) = self.scan_ident_or_reserved() {
                        tokens.push(token)
                    }
                }

                // Simple tokens:
                c => {
                    self.buffer.advance();
                    match c {
                        '(' => tokens.push(Token::new(TokenKind::LeftParen, "(".to_string())),
                        ')' => tokens.push(Token::new(TokenKind::RightParen, ")".to_string())),
                        '{' => tokens.push(Token::new(TokenKind::LeftBrace, "{".to_string())),
                        '}' => tokens.push(Token::new(TokenKind::RightBrace, "}".to_string())),
                        ',' => tokens.push(Token::new(TokenKind::Comma, ",".to_string())),
                        '.' => tokens.push(Token::new(TokenKind::Dot, ".".to_string())),
                        '-' => tokens.push(Token::new(TokenKind::Minus, "-".to_string())),
                        '+' => tokens.push(Token::new(TokenKind::Plus, "+".to_string())),
                        ';' => tokens.push(Token::new(TokenKind::Semicolon, ";".to_string())),
                        '*' => tokens.push(Token::new(TokenKind::Star, "*".to_string())),
                        '!' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::BangEqual, "!=".to_string()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Bang, "!".to_string()));
                            }
                        }
                        '=' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::EqualEqual, "==".to_string()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Equal, "=".to_string()));
                            }
                        }
                        '<' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::LessEqual, "<=".to_string()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Less, "<".to_string()));
                            }
                        }
                        '>' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::GreaterEqual, ">=".to_string()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Greater, ">".to_string()));
                            }
                        }
                        '/' => {
                            if self.buffer.matches("/") {
                                self.buffer.advance_while(|c| c != '\n');
                            } else {
                                tokens.push(Token::new(TokenKind::Slash, "/".to_string()));
                            }
                        }
                        ch => {
                            if ch == ' ' || ch == '\t' || ch == '\r' {
                                continue;
                            } else if ch == '\n' {
                                self.line_number += 1;
                            } else {
                                self.errors.push(ScannerError {
                                    loc: Loc {
                                        line: self.line_number,
                                    },
                                    message: format!("Unexpected character: {}", ch),
                                });
                            }
                        }
                    }
                }
            }
        }

        tokens.push(Token::new(TokenKind::EOF, "".to_string()));

        tokens
    }

    fn scan_string(&mut self) -> Option<Token> {
        let mut str_literal_buffer = String::new();
        let mut is_finished = false;

        while let Some(c) = self.buffer.advance() {
            if c == '"' {
                is_finished = true;
                break;
            }

            str_literal_buffer.push(c);
        }

        if !is_finished {
            self.errors.push(ScannerError {
                loc: Loc {
                    line: self.line_number,
                },
                message: "Unterminated string.".to_string(),
            });
            None
        } else {
            Some(Token::new_str(str_literal_buffer))
        }
    }

    fn scan_number(&mut self) -> Option<Token> {
        let mut lexeme = String::new();
        let mut floating = false;
        while let Some(value) = self.buffer.peek(0) {
            if value.is_ascii_digit() {
                lexeme.push(value);
                self.buffer.advance();
            } else if value == '.' {
                if floating {
                    break;
                }

                let next = self.buffer.peek(1);

                if next.is_none() {
                    break;
                }

                let next = next.unwrap();

                if !next.is_ascii_digit() {
                    break;
                }

                floating = true;
                lexeme.push(value);
                self.buffer.advance();
            } else {
                break;
            }

            if value == '.' {
                floating = true;
            }
        }

        Some(Token::new_num(lexeme, floating))
    }

    fn scan_ident_or_reserved(&mut self) -> Option<Token> {
        let mut lexeme = String::new();

        while let Some(value) = self.buffer.peek(0) {
            // We can suppose that this function is triggered only from letter or underscore
            if value.is_alphabetic() || value == '_' || value.is_ascii_digit() {
                lexeme.push(value);
                self.buffer.advance();
            } else {
                break;
            }
        }

        // Check for reserved words
        if RESERVED.contains(&lexeme.as_str()) {
            return match lexeme.as_str() {
                "and" => Some(Token::new(TokenKind::And, lexeme)),
                "class" => Some(Token::new(TokenKind::Class, lexeme)),
                "else" => Some(Token::new(TokenKind::Else, lexeme)),
                "false" => Some(Token::new(TokenKind::False, lexeme)),
                "fun" => Some(Token::new(TokenKind::Fun, lexeme)),
                "for" => Some(Token::new(TokenKind::For, lexeme)),
                "if" => Some(Token::new(TokenKind::If, lexeme)),
                "nil" => Some(Token::new(TokenKind::Nil, lexeme)),
                "or" => Some(Token::new(TokenKind::Or, lexeme)),
                "print" => Some(Token::new(TokenKind::Print, lexeme)),
                "return" => Some(Token::new(TokenKind::Return, lexeme)),
                "super" => Some(Token::new(TokenKind::Super, lexeme)),
                "this" => Some(Token::new(TokenKind::This, lexeme)),
                "true" => Some(Token::new(TokenKind::True, lexeme)),
                "var" => Some(Token::new(TokenKind::Var, lexeme)),
                "while" => Some(Token::new(TokenKind::While, lexeme)),
                c => panic!("Unknown reserved word: {}", c),
            };
        }

        Some(Token::new_ident(lexeme))
    }
}

pub struct Token {
    pub loc: Loc,
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Literal,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String) -> Self {
        Self {
            loc: Default::default(),
            kind,
            lexeme,
            literal: Literal::none(),
        }
    }

    pub fn new_str(lexeme: String) -> Self {
        Self {
            loc: Default::default(),
            kind: TokenKind::String,
            lexeme: format!("\"{}\"", lexeme),
            literal: Literal::some(lexeme),
        }
    }

    pub fn new_num(lexeme: String, floating: bool) -> Self {
        let mut literal = lexeme.clone();

        if floating {
            while let Some(ch) = literal.pop() {
                if ch != '0' {
                    literal.push(ch);
                    break;
                }
            }

            if literal.ends_with('.') {
                literal += "0";
            }
        } else {
            literal += ".0";
        }

        Self {
            loc: Default::default(),
            kind: TokenKind::Number,
            lexeme: lexeme.clone(),
            literal: Literal::some(literal),
        }
    }

    pub fn new_ident(lexeme: String) -> Self {
        Self {
            loc: Default::default(),
            kind: TokenKind::Identifier,
            lexeme,
            literal: Literal::none(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.kind, self.lexeme, self.literal)
    }
}

pub struct Literal(Option<String>);

impl Literal {
    pub fn none() -> Self {
        Self(None)
    }

    pub fn some(value: String) -> Self {
        Self(Some(value))
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            None => {
                write!(f, "null")
            }
            Some(literal) => {
                write!(f, "{}", literal)
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct Loc {
    pub line: u32,
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}", self.line)
    }
}

pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParen => {
                write!(f, "LEFT_PAREN")
            }
            TokenKind::RightParen => {
                write!(f, "RIGHT_PAREN")
            }
            TokenKind::LeftBrace => {
                write!(f, "LEFT_BRACE")
            }
            TokenKind::RightBrace => {
                write!(f, "RIGHT_BRACE")
            }
            TokenKind::Comma => {
                write!(f, "COMMA")
            }
            TokenKind::Dot => {
                write!(f, "DOT")
            }
            TokenKind::Minus => {
                write!(f, "MINUS")
            }
            TokenKind::Plus => {
                write!(f, "PLUS")
            }
            TokenKind::Semicolon => {
                write!(f, "SEMICOLON")
            }
            TokenKind::Slash => {
                write!(f, "SLASH")
            }
            TokenKind::Star => {
                write!(f, "STAR")
            }
            TokenKind::Bang => {
                write!(f, "BANG")
            }
            TokenKind::BangEqual => {
                write!(f, "BANG_EQUAL")
            }
            TokenKind::Equal => {
                write!(f, "EQUAL")
            }
            TokenKind::EqualEqual => {
                write!(f, "EQUAL_EQUAL")
            }
            TokenKind::Greater => {
                write!(f, "GREATER")
            }
            TokenKind::GreaterEqual => {
                write!(f, "GREATER_EQUAL")
            }
            TokenKind::Less => {
                write!(f, "LESS")
            }
            TokenKind::LessEqual => {
                write!(f, "LESS_EQUAL")
            }
            TokenKind::Identifier => {
                write!(f, "IDENTIFIER")
            }
            TokenKind::String => {
                write!(f, "STRING")
            }
            TokenKind::Number => {
                write!(f, "NUMBER")
            }
            TokenKind::And => {
                write!(f, "AND")
            }
            TokenKind::Class => {
                write!(f, "CLASS")
            }
            TokenKind::Else => {
                write!(f, "ELSE")
            }
            TokenKind::False => {
                write!(f, "FALSE")
            }
            TokenKind::Fun => {
                write!(f, "FUN")
            }
            TokenKind::For => {
                write!(f, "FOR")
            }
            TokenKind::If => {
                write!(f, "IF")
            }
            TokenKind::Nil => {
                write!(f, "NIL")
            }
            TokenKind::Or => {
                write!(f, "OR")
            }
            TokenKind::Print => {
                write!(f, "PRINT")
            }
            TokenKind::Return => {
                write!(f, "RETURN")
            }
            TokenKind::Super => {
                write!(f, "SUPER")
            }
            TokenKind::This => {
                write!(f, "THIS")
            }
            TokenKind::True => {
                write!(f, "TRUE")
            }
            TokenKind::Var => {
                write!(f, "VAR")
            }
            TokenKind::While => {
                write!(f, "WHILE")
            }
            TokenKind::EOF => {
                write!(f, "EOF")
            }
        }
    }
}

#[derive(Debug)]
pub struct ScannerError {
    pub loc: Loc,
    pub message: String,
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] Error: {}", self.loc, self.message)
    }
}

pub struct Buffer<'a> {
    source: &'a str,
}

impl<'a> Buffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn advance(&mut self) -> Option<char> {
        let elem = self.source.chars().next();

        if elem.is_some() {
            let elem_size = elem.unwrap().len_utf8();

            self.source = &self.source[elem_size..];
        }

        elem
    }

    pub fn peek(&self, index: usize) -> Option<char> {
        self.source.chars().nth(index)
    }

    pub fn matches(&self, expected: &str) -> bool {
        if self.source.starts_with(expected) {
            true
        } else {
            false
        }
    }

    pub fn advance_n(&mut self, n: usize) {
        self.source = &self.source[n..];
    }

    pub fn advance_while<F>(&mut self, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(c) = self.peek(0) {
            if !f(c) {
                break;
            } else {
                self.advance();
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }
}
