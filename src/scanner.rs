#![allow(dead_code)]

use std::fmt::{Display, Formatter};

pub struct Scanner<'a> {
    buffer: Buffer<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            buffer: Buffer::new(source),
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, Vec<ScannerError>) {
        let mut scanner_errors = Vec::new();
        let mut tokens = Vec::new();
        let mut line_number = 1;

        while let Some(c) = self.buffer.peek() {
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
                        self.buffer.peek();
                    } else {
                        tokens.push(Token::new(TokenKind::Bang, "!".to_string()));
                    }
                }
                '=' => {
                    if self.buffer.matches("=") {
                        tokens.push(Token::new(TokenKind::EqualEqual, "==".to_string()));
                        self.buffer.peek();
                    } else {
                        tokens.push(Token::new(TokenKind::Equal, "=".to_string()));
                    }
                }
                '<' => {
                    if self.buffer.matches("=") {
                        tokens.push(Token::new(TokenKind::LessEqual, "<=".to_string()));
                        self.buffer.peek();
                    } else {
                        tokens.push(Token::new(TokenKind::Less, "<".to_string()));
                    }
                }
                '>' => {
                    if self.buffer.matches("=") {
                        tokens.push(Token::new(TokenKind::GreaterEqual, ">=".to_string()));
                        self.buffer.peek();
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
                        line_number += 1;
                    } else {
                        scanner_errors.push(ScannerError {
                            loc: Loc { line: line_number },
                            message: format!("Unexpected character: {}", ch),
                        });
                    }
                }
            }
        }

        tokens.push(Token::new(TokenKind::EOF, "".to_string()));

        (tokens, scanner_errors)
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

    pub fn peek(&mut self) -> Option<char> {
        let elem = self.source.chars().next();

        if elem.is_some() {
            let elem_size = elem.unwrap().len_utf8();

            self.source = &self.source[elem_size..];
        }

        elem
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
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }
        }
    }
}
