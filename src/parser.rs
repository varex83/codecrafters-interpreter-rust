use crate::token::{Loc, Locate, Token, TokenKind};
use anyhow::bail;
use anyhow::Result;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Block(Block),
    Print(Print),
    If(If),
}

impl Locate for Stmt {
    fn loc(&self) -> Loc {
        match self {
            Stmt::Expr(expr) => expr.loc(),
            Stmt::Print(print) => print.loc(),
            Stmt::If(if_stmt) => if_stmt.loc(),
            Stmt::Block(block) => block.loc(),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{};", expr),
            Stmt::Print(print) => write!(f, "print {};", print.expr),
            Stmt::If(if_stmt) => write!(f, "{}", if_stmt),
            Stmt::Block(block_stmt) => write!(f, "{}", block_stmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Print {
    pub print_token: Token,
    pub expr: Box<Expr>,
}

impl Locate for Print {
    fn loc(&self) -> Loc {
        self.expr.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub if_token: Token,
    pub condition: Box<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

impl Locate for If {
    fn loc(&self) -> Loc {
        self.if_token.loc()
    }
}

impl Display for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(ref else_br) = self.else_branch {
            write!(
                f,
                "if {} then {:?} else {:?}",
                self.condition, self.then_branch, else_br,
            )
        } else {
            write!(f, "if {} then {:?}", self.condition, self.then_branch)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Token),
    String(Token),
    True(Token),
    False(Token),
    Nil(Token),
    Ident(Token),
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
            Literal::Ident(t) => t,
        }
            .clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Locate for Grouping {
    fn loc(&self) -> Loc {
        self.expr.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Locate for Unary {
    fn loc(&self) -> Loc {
        self.operator.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub left_brace: Token,
    pub right_brace: Token,
}

impl Locate for Block {
    fn loc(&self) -> Loc {
        self.left_brace.loc()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for stmt in &self.stmts {
            write!(f, "{}", stmt)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

type ParserResult = Result<Expr>;
type StmtResult = Result<Stmt>;
type StmtVecResult = Result<Vec<Stmt>>;
type ProgramResult = Result<Program>;

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, cursor: 0 }
    }

    pub fn new_parse(tokens: Vec<Token>) -> ProgramResult {
        let mut parser = Self::new(tokens);

        let expr = parser.parse_program()?;

        Ok(expr)
    }

    pub fn parse_program(&mut self) -> ProgramResult {
        let stmts = self.parse_stmt_chain()?;

        Ok(Program { stmts })
    }

    pub fn parse_stmt_chain(&mut self) -> StmtVecResult {
        let mut stmts = vec![];

        while !self.is_at_end()
            && ![Some(TokenKind::Eof), Some(TokenKind::RightBrace)]
            .contains(&self.peek().map(|e| e.kind))
        {
            stmts.push(self.stmt()?);
        }

        Ok(stmts)
    }

    pub fn parse_block(&mut self) -> Result<Block> {
        let left_brace = if let Some(left_brace) = self.match_tokens(&[TokenKind::LeftBrace]) {
            left_brace
        } else {
            bail!("expected \"{{\" at {}", self.peek().unwrap().loc())
        };

        let stmts = self.parse_stmt_chain()?;

        let right_brace = if self.peek().unwrap().kind != TokenKind::RightBrace {
            bail!("expected \"}}\" at {}", self.peek().unwrap().loc())
        } else {
            self.advance().unwrap()
        };

        Ok(Block {
            stmts,
            left_brace,
            right_brace,
        })
    }

    fn if_stmt(&mut self) -> Result<If> {
        let if_token = if let Some(if_token) = self.match_tokens(&[TokenKind::If]) {
            if_token
        } else {
            bail!("expected \"if\" at {}", self.peek().unwrap().loc())
        };

        let condition = self.expr()?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.match_tokens(&[TokenKind::Else]).is_some() {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(If {
            if_token,
            condition: Box::new(condition),
            then_branch,
            else_branch,
        })
    }

    fn stmt(&mut self) -> StmtResult {
        if self.peek_tokens(&[TokenKind::Print]).is_some() {
            Ok(Stmt::Print(self.print_stmt()?))
        } else if let Some(_) = self.peek_tokens(&[TokenKind::If]) {
            Ok(Stmt::If(self.if_stmt()?))
        } else if let Some(_) = self.peek_tokens(&[TokenKind::LeftBrace]) {
            Ok(Stmt::Block(self.parse_block()?))
        } else {
            self.expr_stmt()
        }
    }

    pub fn print_stmt(&mut self) -> Result<Print> {
        let print_token = if let Some(print_token) = self.match_tokens(&[TokenKind::Print]) {
            print_token
        } else {
            bail!("expected \"print\" at {}", self.peek().unwrap().loc())
        };

        let expr = self.expr()?;

        let token = self.peek().unwrap();

        if token.kind != TokenKind::Semicolon {
            bail!("expected \";\" at {}, got: {:?}", token.loc(), token.lexeme)
        } else {
            self.advance();
        }

        Ok(Print {
            print_token,
            expr: Box::new(expr),
        })
    }

    pub fn expr_stmt(&mut self) -> StmtResult {
        let expr = self.expr()?;

        let token = self.peek().unwrap();

        if token.kind != TokenKind::Semicolon {
            bail!("expected \";\" at {}, got: {:?}", token.loc(), token.lexeme)
        } else {
            self.advance();
        }

        Ok(Stmt::Expr(expr))
    }

    pub fn expr(&mut self) -> ParserResult {
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
            TokenKind::Identifier => {
                self.advance();
                Ok(Expr::Literal(Literal::Ident(token)))
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

    pub fn peek_tokens(&self, tokens: &[TokenKind]) -> Option<Token> {
        if tokens.contains(&self.peek()?.kind) {
            self.peek()
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
                Literal::Ident(_) => {
                    format!("{}", token.lexeme)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn print_stmt() {
        let tokens = vec![
            Token::new(TokenKind::Print, "print", Loc::new(1, 1)),
            Token::new(TokenKind::Number, "1", Loc::new(1, 7)),
            Token::new(TokenKind::Semicolon, ";", Loc::new(1, 8)),
            Token::new(TokenKind::Eof, "", Loc::new(1, 9)),
        ];

        let stmts = Parser::new_parse(tokens).unwrap().stmts;

        assert_eq!(
            stmts,
            vec![Stmt::Print(Print {
                print_token: Token::new(TokenKind::Print, "print", Loc::new(1, 1)),
                expr: Box::new(Expr::Literal(Literal::Number(Token::new(
                    TokenKind::Number,
                    "1",
                    Loc::new(1, 7)
                ))))
            })]
        );
    }
}
