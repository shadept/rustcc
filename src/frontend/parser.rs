use crate::frontend::ast::{BinaryOp, Expr, ExprKind, Function, Program, Stmt, StmtKind, UnaryOp};
use crate::frontend::parser::ParserError::UnexpectedToken;
use crate::frontend::token::{Keyword, Symbol, Token, TokenKind};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::vec::IntoIter;

macro_rules! match_keyword {
    ($self:ident, $keyword:expr $(, $keywords:expr)*) => {
        if let Some(tok) = $self.peek() {
            match tok.kind {
                TokenKind::Keyword(s) if s == $keyword $(|| s == $keywords)* => {
                    $self.advance();
                    Some((s, tok.span))
                },
                _ => None,
            }
        } else {
            None
        }
    };
}

macro_rules! match_symbol {
    ($self:ident, $symbol:expr $(, $symbols:expr)*) => {
        if let Some(tok) = $self.peek() {
            match tok.kind {
                TokenKind::Symbol(s) if s == $symbol $(|| s == $symbols)* => {
                    $self.advance();
                    Some((s, tok.span))
                },
                _ => None,
            }
        } else {
            None
        }
    };
}

#[derive(Debug)]
pub struct Parser {
    tokens: IntoIter<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let iter = tokens.into_iter();
        Parser { tokens: iter }
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let func = self.parse_function()?;
        let ret = Ok(Program::new(func));
        if let Some(token) = self.peek() {
            return Err(ParserError::UnconsumedToken(token));
        }
        ret
    }

    pub fn parse_function(&mut self) -> Result<Function, ParserError> {
        self.expect_keyword(Keyword::Int)?;
        let name = self.expect_ident()?;
        self.expect_symbol(Symbol::OpenParen)?;
        self.expect_keyword(Keyword::Void)?;
        self.expect_symbol(Symbol::CloseParen)?;
        let body = self.parse_block()?;
        Ok(Function::new(name, body))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        self.expect_symbol(Symbol::OpenBrace)?;
        let stmt = self.parse_statement()?;
        self.expect_symbol(Symbol::CloseBrace)?;
        Ok(vec![stmt])
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let ret: Result<Stmt, ParserError>;
        if let Some((_, span)) = match_keyword!(self, Keyword::Return) {
            let expr1 = self.parse_expression(0)?;
            let span1 = expr1.span;
            let expr = Expr::new(ExprKind::Return(Some(Box::from(expr1))), span1);
            ret = Ok(Stmt::new(StmtKind::Expr(Box::from(expr)), span));
        } else {
            let expr = self.parse_expression(0)?;
            let span = expr.span.clone();
            ret = Ok(Stmt::new(StmtKind::Expr(Box::new(expr)), span))
        }
        self.expect_symbol(Symbol::Semicolon)?;
        ret
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr, ParserError> {
        let mut left = self.parse_factor()?;
        loop {
            if let Some(token) = self.peek() {
                if let Some(op) = self.parse_binary_op(&token) {
                    let precedence = self.binop_precedence(&op);
                    if precedence >= min_precedence {
                        self.advance().unwrap();
                        let right = self.parse_expression(precedence + 1)?;
                        let span = left.span + right.span;
                        left = Expr::new(ExprKind::Binary(op, left.into(), right.into()), span);
                        continue;
                    }
                }
            }
            break Ok(left);
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let token = self.peek().ok_or(ParserError::EOT)?;
        if let Some(op) = self.parse_unary_op(&token) {
            let token = self.advance().unwrap();
            let expr = self.parse_factor()?;
            let span = expr.span + token.span;
            return Ok(Expr::new(ExprKind::Unary(op, expr.into()), span));
        }
        match token.kind {
            TokenKind::IntNumber(n, _) => {
                self.advance().unwrap();
                Ok(Expr::new(ExprKind::Constant(n as i32), token.span))
            }
            TokenKind::Symbol(Symbol::OpenParen) => {
                self.advance().unwrap();
                let expr = self.parse_expression(0)?;
                self.expect_symbol(Symbol::CloseParen)?;
                Ok(expr)
            }
            _ => Err(UnexpectedToken(token)),
        }
    }

    fn parse_unary_op(&self, token: &Token) -> Option<UnaryOp> {
        if let TokenKind::Symbol(sym) = &token.kind {
            match sym {
                Symbol::Tilde => Some(UnaryOp::Complement),
                Symbol::Minus => Some(UnaryOp::Negate),
                Symbol::Bang => Some(UnaryOp::Not),
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_binary_op(&self, token: &Token) -> Option<BinaryOp> {
        if let TokenKind::Symbol(sym) = &token.kind {
            match sym {
                Symbol::Plus => Some(BinaryOp::Add),
                Symbol::Minus => Some(BinaryOp::Subtract),
                Symbol::Star => Some(BinaryOp::Multiply),
                Symbol::Slash => Some(BinaryOp::Divide),
                Symbol::Percent => Some(BinaryOp::Remainder),
                Symbol::Pipe => Some(BinaryOp::BitwiseOr),
                Symbol::Ampersand => Some(BinaryOp::BitwiseAnd),
                Symbol::Caret => Some(BinaryOp::BitwiseXor),
                Symbol::AmpersandAmpersand => Some(BinaryOp::And),
                Symbol::PipePipe => Some(BinaryOp::Or),
                Symbol::EqualEqual => Some(BinaryOp::Equal),
                Symbol::BangEqual => Some(BinaryOp::NotEqual),
                Symbol::LessThan => Some(BinaryOp::LessThan),
                Symbol::GreaterThan => Some(BinaryOp::GreaterThan),
                Symbol::LessThanOrEqual => Some(BinaryOp::LessThanOrEqual),
                Symbol::GreaterThanOrEqual => Some(BinaryOp::GreaterThanOrEqual),
                _ => None,
            }
        } else {
            None
        }
    }

    fn binop_precedence(&self, op: &BinaryOp) -> u8 {
        match op {
            BinaryOp::Add => 45,
            BinaryOp::Subtract => 45,
            BinaryOp::Multiply => 50,
            BinaryOp::Divide => 50,
            BinaryOp::Remainder => 50,
            BinaryOp::BitwiseOr => 30,
            BinaryOp::BitwiseAnd => 35,
            BinaryOp::BitwiseXor => 40,
            BinaryOp::And => 10,
            BinaryOp::Or => 5,
            BinaryOp::Equal => 30,
            BinaryOp::NotEqual => 30,
            BinaryOp::LessThan => 35,
            BinaryOp::LessThanOrEqual => 35,
            BinaryOp::GreaterThan => 35,
            BinaryOp::GreaterThanOrEqual => 35,
        }
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.clone().next()
    }

    fn peek2(&self) -> Option<Token> {
        let mut l = self.tokens.clone();
        l.next();
        l.next()
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect_ident(&mut self) -> Result<String, ParserError> {
        if let Some(tok) = self.peek() {
            return match tok.kind {
                TokenKind::Identifier(ident) => {
                    self.advance().unwrap();
                    Ok(ident)
                }
                _ => Err(UnexpectedToken(tok)),
            };
        }
        Err(ParserError::EOT)
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<Token, ParserError> {
        if let Some(tok) = self.peek() {
            if matches!(&tok.kind, TokenKind::Keyword(kw) if *kw == keyword) {
                return Ok(self.advance().unwrap());
            }
            return Err(UnexpectedToken(tok));
        }
        Err(ParserError::EOT)
    }

    fn expect_symbol(&mut self, symbol: Symbol) -> Result<Token, ParserError> {
        if let Some(tok) = self.peek() {
            if matches!(&tok.kind, TokenKind::Symbol(sym) if *sym == symbol) {
                return Ok(self.advance().unwrap());
            }
            return Err(UnexpectedToken(tok));
        }
        Err(ParserError::EOT)
    }
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnconsumedToken(Token),
    EOT, // end-of-tokens
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(token) => write!(f, "Unexpected token `{}`", token.kind),
            ParserError::UnconsumedToken(token) => {
                write!(f, "Unconsumed token in stream `{}`", token.kind)
            }
            ParserError::EOT => write!(f, "End of tokens"),
        }
    }
}

impl Error for ParserError {}
