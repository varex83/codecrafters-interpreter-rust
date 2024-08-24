use crate::parser::{Binary, Block, Expr, Grouping, If, Literal, Print, Program, Stmt, Unary};
use crate::token::TokenKind;
use anyhow::bail;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(Debug)]
pub enum EvalResult {
    Bool(bool),
    String(String),
    Number(f32),
    Nil,
}

impl Display for EvalResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{}", x),
            Self::String(x) => write!(f, "{}", x),
            Self::Number(x) => write!(f, "{}", x),
            Self::Nil => write!(f, "nil"),
        }
    }
}

type EvalRuntimeResult = anyhow::Result<EvalResult>;

impl Add for EvalResult {
    type Output = EvalRuntimeResult;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x + y),
            (Self::String(x), Self::String(y)) => Self::String(x + &y),
            (x, y) => bail!("RUNTIME_ERR: Can't add {:?} and {:?}", x, y),
        })
    }
}

impl Sub for EvalResult {
    type Output = EvalRuntimeResult;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x - y),
            (x, y) => bail!("RUNTIME_ERR: Can't subtract {:?} and {:?}", x, y),
        })
    }
}

impl Mul for EvalResult {
    type Output = EvalRuntimeResult;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x * y),
            (x, y) => bail!("RUNTIME_ERR: Can't multiply {:?} and {:?}", x, y),
        })
    }
}

impl Div for EvalResult {
    type Output = EvalRuntimeResult;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Self::Number(x), Self::Number(y)) => Self::Number(x / y),
            (x, y) => bail!("RUNTIME_ERR: Can't divide {:?} and {:?}", x, y),
        })
    }
}

impl PartialEq<Self> for EvalResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::String(x), Self::String(y)) => x == y,
            (Self::Number(x), Self::Number(y)) => x == y,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd<Self> for EvalResult {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

impl Not for EvalResult {
    type Output = EvalRuntimeResult;

    fn not(self) -> Self::Output {
        Ok(match self {
            Self::Bool(x) => Self::Bool(!x),
            Self::Number(x) => Self::Bool(x == 0.0),
            Self::Nil => Self::Bool(true),
            x => bail!("RUNTIME_ERR: Can't negate {:?}", x),
        })
    }
}

impl Neg for EvalResult {
    type Output = EvalRuntimeResult;

    fn neg(self) -> Self::Output {
        Ok(match self {
            Self::Number(x) => Self::Number(-x),
            x => bail!("RUNTIME_ERR: Can't negate {:?}", x),
        })
    }
}

pub trait Evaluate {
    fn eval(&self) -> EvalRuntimeResult;
}

impl Evaluate for Expr {
    fn eval(&self) -> EvalRuntimeResult {
        match self {
            Expr::Literal(literal) => literal.eval(),
            Expr::Binary(binary) => binary.eval(),
            Expr::Grouping(grouping) => grouping.eval(),
            Expr::Unary(unary) => unary.eval(),
        }
    }
}

impl Evaluate for Literal {
    fn eval(&self) -> EvalRuntimeResult {
        Ok(match self {
            Literal::Number(number) => EvalResult::Number({
                let literal = number.literal.to_string();

                literal.parse::<f32>().unwrap()
            }),
            Literal::String(string) => EvalResult::String(string.literal.to_string()),
            Literal::True(_) => EvalResult::Bool(true),
            Literal::False(_) => EvalResult::Bool(false),
            Literal::Nil(_) => EvalResult::Nil,
        })
    }
}

impl Evaluate for bool {
    fn eval(&self) -> EvalRuntimeResult {
        Ok(EvalResult::Bool(*self))
    }
}

impl Evaluate for Binary {
    fn eval(&self) -> EvalRuntimeResult {
        let left = self.left.eval()?;
        let right = self.right.eval()?;

        match self.operator.kind {
            TokenKind::Plus => left + right,
            TokenKind::Minus => left - right,
            TokenKind::Star => left * right,
            TokenKind::Slash => left / right,
            TokenKind::Greater => {
                let res = left.partial_cmp(&right);

                match res {
                    Some(Ordering::Greater) => Ok(EvalResult::Bool(true)),
                    Some(Ordering::Less) | Some(Ordering::Equal) => Ok(EvalResult::Bool(false)),
                    None => bail!(
                        "RUNTIME_ERR: Invalid comparison between {:?} and {:?}",
                        left,
                        right
                    ),
                }
            }
            TokenKind::GreaterEqual => {
                let res = left.partial_cmp(&right);

                match res {
                    Some(Ordering::Greater) | Some(Ordering::Equal) => Ok(EvalResult::Bool(true)),
                    Some(Ordering::Less) => Ok(EvalResult::Bool(false)),
                    None => bail!(
                        "RUNTIME_ERR: Invalid comparison between {:?} and {:?}",
                        left,
                        right
                    ),
                }
            }
            TokenKind::Less => {
                let res = left.partial_cmp(&right);

                match res {
                    Some(Ordering::Less) => Ok(EvalResult::Bool(true)),
                    Some(Ordering::Greater) | Some(Ordering::Equal) => Ok(EvalResult::Bool(false)),
                    None => bail!(
                        "RUNTIME_ERR: Invalid comparison between {:?} and {:?}",
                        left,
                        right
                    ),
                }
            }
            TokenKind::LessEqual => {
                let res = left.partial_cmp(&right);

                match res {
                    Some(Ordering::Less) | Some(Ordering::Equal) => Ok(EvalResult::Bool(true)),
                    Some(Ordering::Greater) => Ok(EvalResult::Bool(false)),
                    None => bail!(
                        "RUNTIME_ERR: Invalid comparison between {:?} and {:?}",
                        left,
                        right
                    ),
                }
            }
            TokenKind::EqualEqual => (left == right).eval(),
            TokenKind::BangEqual => (left != right).eval(),
            _ => bail!("RUNTIME_ERR: Invalid binary operator {:?}", self.operator),
        }
    }
}

impl Evaluate for Unary {
    fn eval(&self) -> EvalRuntimeResult {
        let right = self.right.eval()?;
        match self.operator.kind {
            TokenKind::Minus => -right,
            TokenKind::Bang => !right,
            _ => bail!("RUNTIME_ERR: Invalid unary operator {:?}", self.operator),
        }
    }
}

impl Evaluate for Grouping {
    fn eval(&self) -> EvalRuntimeResult {
        self.expr.eval()
    }
}

impl Evaluate for Stmt {
    fn eval(&self) -> EvalRuntimeResult {
        match self {
            Stmt::Expr(expr) => expr.eval(),
            Stmt::Print(print) => print.eval(),
            Stmt::If(if_stmt) => if_stmt.eval(),
            Stmt::Block(block) => block.eval(),
        }
    }
}

impl Evaluate for Print {
    fn eval(&self) -> EvalRuntimeResult {
        let value = self.expr.eval()?;
        println!("{}", value);
        Ok(EvalResult::Nil)
    }
}

impl Evaluate for If {
    fn eval(&self) -> EvalRuntimeResult {
        let condition = self.condition.eval()?;
        match condition {
            EvalResult::Bool(true) => {
                self.then_branch.eval()?;
            }
            EvalResult::Bool(false) | EvalResult::Nil => {
                if let Some(else_branch) = &self.else_branch {
                    else_branch.eval()?;
                }
            }
            _ => bail!("RUNTIME_ERR: Invalid condition {:?}", condition),
        };

        Ok(EvalResult::Nil)
    }
}

impl Evaluate for Option<Stmt> {
    fn eval(&self) -> EvalRuntimeResult {
        match self {
            Some(stmt) => stmt.eval(),
            None => Ok(EvalResult::Nil),
        }
    }
}

impl Evaluate for Block {
    fn eval(&self) -> EvalRuntimeResult {
        self.stmts.eval()
    }
}

impl Evaluate for Vec<Stmt> {
    fn eval(&self) -> EvalRuntimeResult {
        for stmt in self {
            stmt.eval()?;
        }
        Ok(EvalResult::Nil)
    }
}

impl Evaluate for Program {
    fn eval(&self) -> EvalRuntimeResult {
        self.stmts.eval()
    }
}
