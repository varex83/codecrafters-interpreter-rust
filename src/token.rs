use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Token {
    loc: Loc,
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Literal,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, loc: Loc) -> Self {
        Self {
            loc,
            kind,
            lexeme: lexeme.to_string(),
            literal: Literal::none(),
        }
    }

    pub fn new_str(lexeme: String, loc: Loc) -> Self {
        Self {
            loc,
            kind: TokenKind::String,
            lexeme: format!("\"{}\"", lexeme),
            literal: Literal::some(lexeme),
        }
    }

    pub fn new_num(lexeme: &str, floating: bool, loc: Loc) -> Self {
        let mut literal = lexeme.to_string();

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
            loc,
            kind: TokenKind::Number,
            lexeme: lexeme.to_string(),
            literal: Literal::some(literal),
        }
    }

    pub fn new_ident(lexeme: String, loc: Loc) -> Self {
        Self {
            loc,
            kind: TokenKind::Identifier,
            lexeme,
            literal: Literal::none(),
        }
    }
}

impl Locate for Token {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.kind, self.lexeme, self.literal)
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Loc {
    pub line: u32,
    pub col: u32,
}

impl Loc {
    pub fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }

    pub fn move_forward(&mut self) {
        self.col += 1;
    }

    pub fn move_down(&mut self) {
        self.line += 1;
        self.col = 0;
    }
}

impl Locate for Loc {
    fn loc(&self) -> Loc {
        *self
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}, col: {}", self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
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

    Eof,
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
            TokenKind::Eof => {
                write!(f, "EOF")
            }
        }
    }
}

pub fn ident_to_reserved_word(lexeme: &str, loc: Loc) -> Token {
    match lexeme {
        "and" => Token::new(TokenKind::And, lexeme, loc),
        "class" => Token::new(TokenKind::Class, lexeme, loc),
        "else" => Token::new(TokenKind::Else, lexeme, loc),
        "false" => Token::new(TokenKind::False, lexeme, loc),
        "fun" => Token::new(TokenKind::Fun, lexeme, loc),
        "for" => Token::new(TokenKind::For, lexeme, loc),
        "if" => Token::new(TokenKind::If, lexeme, loc),
        "nil" => Token::new(TokenKind::Nil, lexeme, loc),
        "or" => Token::new(TokenKind::Or, lexeme, loc),
        "print" => Token::new(TokenKind::Print, lexeme, loc),
        "return" => Token::new(TokenKind::Return, lexeme, loc),
        "super" => Token::new(TokenKind::Super, lexeme, loc),
        "this" => Token::new(TokenKind::This, lexeme, loc),
        "true" => Token::new(TokenKind::True, lexeme, loc),
        "var" => Token::new(TokenKind::Var, lexeme, loc),
        "while" => Token::new(TokenKind::While, lexeme, loc),
        c => panic!("Unknown reserved word: {}", c),
    }
}

pub trait Locate {
    fn loc(&self) -> Loc;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::consts::RESERVED;

    #[test]
    fn converts_all_reserved_words() {
        RESERVED.iter().for_each(|&word| {
            let loc = Loc::new(0, 0);
            // Should not panic.
            let _ = ident_to_reserved_word(word, loc);
        });
    }
}
