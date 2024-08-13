//
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
// | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
// | "(" expression ")" ;
//

use crate::token::{Loc, Locate, Token, TokenKind};
use anyhow::bail;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Grouping(Grouping),
    Unary(Unary),
}

impl Locate for Expr {
    fn loc(&self) -> Loc {
        match self {
            Expr::Literal(literal) => literal.loc(),
            Expr::Binary(binary) => binary.loc(),
            Expr::Grouping(grouping) => grouping.loc(),
            Expr::Unary(unary) => unary.loc(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Token),
    String(Token),
    True(Token),
    False(Token),
    Nil(Token),
}

impl Locate for Literal {
    fn loc(&self) -> Loc {
        self.reduce_to_token().loc()
    }
}

impl Literal {
    pub fn reduce_to_token(&self) -> Token {
        match self {
            Literal::Number(t) => t,
            Literal::String(t) => t,
            Literal::True(t) => t,
            Literal::False(t) => t,
            Literal::Nil(t) => t,
        }
        .clone()
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Locate for Grouping {
    fn loc(&self) -> Loc {
        self.expr.loc()
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Locate for Unary {
    fn loc(&self) -> Loc {
        self.operator.loc()
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Locate for Binary {
    fn loc(&self) -> Loc {
        self.operator.loc()
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

type ParserResult = anyhow::Result<Expr>;
type StmtResult = anyhow::Result<Vec<Expr>>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, cursor: 0 }
    }

    pub fn new_parse(tokens: Vec<Token>) -> ParserResult {
        let mut parser = Self::new(tokens);

        let expr = parser.parse()?.clone();

        Ok(expr.first().unwrap().clone())
    }

    pub fn parse(&mut self) -> StmtResult {
        let mut stmts = vec![];

        while !self.is_at_end() && self.peek().map(|e| e.kind) != Some(TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }

    pub fn parse_stmt(&mut self) -> ParserResult {
        let expr = self.expr()?;

        let token = self.peek().unwrap();

        if token.kind != TokenKind::Semicolon {
            bail!("expected \";\" at {}, got: {:?}", token.loc(), token.lexeme)
        } else {
            self.advance();
        }

        Ok(expr)
    }

    fn expr(&mut self) -> ParserResult {
        self.equality()
    }

    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.match_tokens(&[TokenKind::BangEqual, TokenKind::EqualEqual])
        {
            let rhs = self.comparison()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            })
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.term()?;

        while let Some(operator) = self.match_tokens(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let rhs = self.term()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            })
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParserResult {
        let mut expr = self.factor()?;

        while let Some(operator) = self.match_tokens(&[TokenKind::Plus, TokenKind::Minus]) {
            let rhs = self.factor()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            })
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        while let Some(operator) = self.match_tokens(&[TokenKind::Star, TokenKind::Slash]) {
            let rhs = self.unary()?;

            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            })
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if let Some(operator) = self.match_tokens(&[TokenKind::Bang, TokenKind::Minus]) {
            let rhs = self.unary()?;

            Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(rhs),
            }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParserResult {
        // todo: handle error
        let token = self.peek().unwrap();

        match token.kind.clone() {
            TokenKind::Number => {
                self.advance();
                Ok(Expr::Literal(Literal::Number(token)))
            }
            TokenKind::String => {
                self.advance();
                Ok(Expr::Literal(Literal::String(token)))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Literal(Literal::True(token)))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Literal(Literal::False(token)))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expr::Literal(Literal::Nil(token)))
            }
            TokenKind::LeftParen => {
                self.advance();

                let result = self.expr()?;
                if self.peek().unwrap().kind != TokenKind::RightParen {
                    bail!("unclosed parenthesis at {}", token.loc())
                } else {
                    self.advance();
                }

                Ok(Expr::Grouping(Grouping {
                    expr: Box::new(result),
                }))
            }
            _ => bail!("unexpected token <{}> at {}", token.kind, token.loc()),
        }
    }
}

// Private
impl Parser {
    pub fn is_at_end(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).cloned()
    }

    pub fn advance(&mut self) -> Option<Token> {
        if self.is_at_end() {
            None
        } else {
            let result = self.peek();
            self.cursor += 1;
            result
        }
    }

    pub fn match_tokens(&mut self, tokens: &[TokenKind]) -> Option<Token> {
        if tokens.contains(&self.peek()?.kind) {
            self.advance()
        } else {
            None
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal) => {
                write!(f, "{}", literal)
            }
            Expr::Binary(binary) => {
                write!(f, "({})", binary)
            }
            Expr::Grouping(grouping) => {
                write!(f, "({})", grouping)
            }
            Expr::Unary(unary) => {
                write!(f, "({})", unary)
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let token = self.reduce_to_token();

        write!(
            f,
            "{}",
            match self {
                Literal::Number(_) | Literal::String(_) => {
                    format!("{}", token.literal)
                }
                Literal::True(_) | Literal::False(_) | Literal::Nil(_) => {
                    token.lexeme
                }
            }
        )
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.operator.lexeme, self.left, self.right)
    }
}

impl Display for Grouping {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "group {}", self.expr)
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.operator.lexeme, self.right)
    }
}
