use crate::token::{Loc, Locate, Token, TokenKind};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        let flag = self.stmts.len() > 1;

        if flag {
            write!(f, "\n")?;
        }

        for stmt in &self.stmts {
            write!(f, "{}{}", if flag { "\t" } else { "" }, stmt)?;
        }

        if flag {
            write!(f, "\n")?;
        }

        write!(f, "]")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Block(Block),
    Print(Print),
    If(If),
    Declaration(Declaration),
    Assign(Assign),
}

impl Locate for Stmt {
    fn loc(&self) -> Loc {
        match self {
            Stmt::Expr(expr) => expr.loc(),
            Stmt::Print(print) => print.loc(),
            Stmt::If(if_stmt) => if_stmt.loc(),
            Stmt::Block(block) => block.loc(),
            Stmt::Declaration(declaration) => declaration.loc(),
            Stmt::Assign(assign) => assign.loc(),
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
            Stmt::Declaration(declaration) => write!(
                f,
                "var {}{};",
                declaration.name.lexeme,
                declaration
                    .initializer
                    .as_ref()
                    .map_or("".to_string(), |e| format!(" = {}", e))
            ),
            Stmt::Assign(assign) => {
                write!(f, "{} = {};", assign.name.lexeme, assign.value)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub var_token: Token,
    pub name: Token,
    pub assign_token: Option<Token>,
    pub initializer: Option<Expr>,
}

impl Declaration {
    pub fn new(
        var_token: Token,
        name: Token,
        assign_token: Option<Token>,
        initializer: Option<Expr>,
    ) -> Self {
        Declaration {
            var_token,
            name,
            assign_token,
            initializer,
        }
    }

    pub fn new_var(name: Token) -> Self {
        Declaration {
            var_token: Token::new(TokenKind::Var, "var", name.loc()),
            name,
            assign_token: None,
            initializer: None,
        }
    }
}

impl Locate for Declaration {
    fn loc(&self) -> Loc {
        self.name.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Token,
    pub assign_token: Token,
    pub value: Expr,
}

impl Locate for Assign {
    fn loc(&self) -> Loc {
        self.name.loc()
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
