use crate::consts::RESERVED;
use crate::token::{ident_to_reserved_word, Loc, Locate, Token, TokenKind};
use std::fmt::{Display, Formatter};
use std::str::Chars;

pub struct Scanner<'a> {
    buffer: Buffer<'a>,
    pub errors: Vec<ScannerError>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            buffer: Buffer::new(source),
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
                        '(' => tokens.push(Token::new(TokenKind::LeftParen, "(", self.loc())),
                        ')' => tokens.push(Token::new(TokenKind::RightParen, ")", self.loc())),
                        '{' => tokens.push(Token::new(TokenKind::LeftBrace, "{", self.loc())),
                        '}' => tokens.push(Token::new(TokenKind::RightBrace, "}", self.loc())),
                        ',' => tokens.push(Token::new(TokenKind::Comma, ",", self.loc())),
                        '.' => tokens.push(Token::new(TokenKind::Dot, ".", self.loc())),
                        '-' => tokens.push(Token::new(TokenKind::Minus, "-", self.loc())),
                        '+' => tokens.push(Token::new(TokenKind::Plus, "+", self.loc())),
                        ';' => tokens.push(Token::new(TokenKind::Semicolon, ";", self.loc())),
                        '*' => tokens.push(Token::new(TokenKind::Star, "*", self.loc())),
                        '!' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::BangEqual, "!=", self.loc()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Bang, "!", self.loc()));
                            }
                        }
                        '=' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::EqualEqual, "==", self.loc()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Equal, "=", self.loc()));
                            }
                        }
                        '<' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::LessEqual, "<=", self.loc()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Less, "<", self.loc()));
                            }
                        }
                        '>' => {
                            if self.buffer.matches("=") {
                                tokens.push(Token::new(TokenKind::GreaterEqual, ">=", self.loc()));
                                self.buffer.advance();
                            } else {
                                tokens.push(Token::new(TokenKind::Greater, ">", self.loc()));
                            }
                        }
                        '/' => {
                            if self.buffer.matches("/") {
                                self.buffer.advance_while(|c| c != '\n');
                            } else {
                                tokens.push(Token::new(TokenKind::Slash, "/", self.loc()));
                            }
                        }
                        ch => {
                            if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
                                continue;
                            } else {
                                self.errors.push(ScannerError {
                                    loc: self.buffer.loc(),
                                    message: format!("Unexpected character: {}", ch),
                                });
                            }
                        }
                    }
                }
            }
        }

        tokens.push(Token::new(TokenKind::Eof, "", self.loc()));

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
                loc: self.loc(),
                message: "Unterminated string.".to_string(),
            });
            None
        } else {
            Some(Token::new_str(str_literal_buffer, self.loc()))
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

        Some(Token::new_num(&lexeme, floating, self.loc()))
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
            return Some(ident_to_reserved_word(&lexeme, self.loc()));
        }

        Some(Token::new_ident(lexeme, self.loc()))
    }
}

impl Locate for Scanner<'_> {
    fn loc(&self) -> Loc {
        self.buffer.loc()
    }
}

#[derive(Debug)]
pub struct ScannerError {
    pub loc: Loc,
    pub message: String,
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}] Error: {}", self.loc, self.message)
    }
}

pub struct Buffer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    loc: Loc,
}

impl<'a> Buffer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            loc: Default::default(),
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        let elem = self.chars.next();

        if let Some(elem) = elem {
            let elem_size = elem.len_utf8();

            self.loc.move_forward();

            if elem == '\n' {
                self.loc.move_down();
            }

            self.source = &self.source[elem_size..];
        }

        elem
    }

    pub fn peek(&self, index: usize) -> Option<char> {
        self.source.chars().nth(index)
    }

    pub fn matches(&self, expected: &str) -> bool {
        self.source.starts_with(expected)
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

impl Locate for Buffer<'_> {
    fn loc(&self) -> Loc {
        self.loc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_addition() {
        let source = "1 + 2 + 3";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new_num("1", false, Loc::new(1, 0)),
                Token::new(TokenKind::Plus, "+", Loc::new(1, 2)),
                Token::new_num("2", false, Loc::new(1, 4)),
                Token::new(TokenKind::Plus, "+", Loc::new(1, 6)),
                Token::new_num("3", false, Loc::new(1, 8)),
                Token::new(TokenKind::Eof, "", Loc::new(1, 9)),
            ]
        );
    }

    #[test]
    fn test_addition_multiple_line() {
        let source = r#"
            1 + 2 + 3 ;
            4 + 5 + 6
        "#;

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new_num("1", false, Loc::new(2, 12)),
                Token::new(TokenKind::Plus, "+", Loc::new(2, 14)),
                Token::new_num("2", false, Loc::new(2, 16)),
                Token::new(TokenKind::Plus, "+", Loc::new(2, 18)),
                Token::new_num("3", false, Loc::new(2, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(2, 22)),
                Token::new_num("4", false, Loc::new(3, 12)),
                Token::new(TokenKind::Plus, "+", Loc::new(3, 14)),
                Token::new_num("5", false, Loc::new(3, 16)),
                Token::new(TokenKind::Plus, "+", Loc::new(3, 18)),
                Token::new_num("6", false, Loc::new(3, 20)),
                Token::new(TokenKind::Eof, "", Loc::new(3, 21)),
            ]
        )
    }

    #[test]
    fn test_idents() {
        let source = "var a = 10; var b = 20;";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Var, "var", Loc::new(1, 0)),
                Token::new_ident("a".to_string(), Loc::new(1, 4)),
                Token::new(TokenKind::Equal, "=", Loc::new(1, 6)),
                Token::new_num("10", true, Loc::new(1, 8)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(1, 10)),
                Token::new(TokenKind::Var, "var", Loc::new(1, 12)),
                Token::new_ident("b".to_string(), Loc::new(1, 16)),
                Token::new(TokenKind::Equal, "=", Loc::new(1, 18)),
                Token::new_num("20", true, Loc::new(1, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(1, 22)),
                Token::new(TokenKind::Eof, "", Loc::new(1, 23)),
            ]
        );
    }

    #[test]
    fn test_with_comments() {
        let source = r#"
            // This is a comment
            var a = 10; // This is another comment
            var b = 20;
        "#;

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Var, "var", Loc::new(2, 12)),
                Token::new_ident("a".to_string(), Loc::new(2, 16)),
                Token::new(TokenKind::Equal, "=", Loc::new(2, 18)),
                Token::new_num("10", true, Loc::new(2, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(2, 22)),
                Token::new(TokenKind::Var, "var", Loc::new(3, 12)),
                Token::new_ident("b".to_string(), Loc::new(3, 16)),
                Token::new(TokenKind::Equal, "=", Loc::new(3, 18)),
                Token::new_num("20", true, Loc::new(3, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(3, 22)),
                Token::new(TokenKind::Eof, "", Loc::new(3, 23)),
            ]
        );
    }

    #[test]
    fn test_with_comments_utf_8() {
        let source = r#"
            // This is a comment
            var a = 10; // This ƒ∂åß∂åß∂œ∑∂œ∑∂åß∂ comment
            var b = 20;
        "#;

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Var, "var", Loc::new(2, 12)),
                Token::new_ident("a".to_string(), Loc::new(2, 16)),
                Token::new(TokenKind::Equal, "=", Loc::new(2, 18)),
                Token::new_num("10", true, Loc::new(2, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(2, 22)),
                Token::new(TokenKind::Var, "var", Loc::new(3, 12)),
                Token::new_ident("b".to_string(), Loc::new(3, 16)),
                Token::new(TokenKind::Equal, "=", Loc::new(3, 18)),
                Token::new_num("20", true, Loc::new(3, 20)),
                Token::new(TokenKind::Semicolon, ";", Loc::new(3, 22)),
                Token::new(TokenKind::Eof, "", Loc::new(3, 23)),
            ]
        );
    }

    #[test]
    fn test_zero_col() {
        let source = "abc literal";

        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        assert_eq!(
            tokens,
            vec![
                Token::new_ident("abc".to_string(), Loc::new(1, 0)),
                Token::new_ident("literal".to_string(), Loc::new(1, 4)),
                Token::new(TokenKind::Eof, "", Loc::new(1, 11)),
            ]
        );
    }

    #[test]
    fn test_reserved_words() {
        let source = RESERVED.join(", ");

        let mut scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        let mut expected = Vec::new();

        let mut loc = Loc::new(1, 0);

        for word in RESERVED.iter() {
            expected.push(ident_to_reserved_word(word, loc));
            loc.col += word.len() as u32;

            expected.push(Token::new(TokenKind::Comma, ",", loc));
            loc.col += 2;
        }

        expected.pop();
        expected.push(Token::new(TokenKind::Eof, "", loc));

        assert_eq!(tokens, expected);
    }
}
