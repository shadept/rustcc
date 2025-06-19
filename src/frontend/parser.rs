use crate::frontend::ast::{Expr, ExprKind, Function, Program, Stmt, StmtKind, UnaryOp};
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
            let expr = self.parse_return()?;
            ret = Ok(Stmt::new(StmtKind::Expr(Box::from(expr)), span));
        } else {
            let expr = self.parse_expression()?;
            let span = expr.span.clone();
            ret = Ok(Stmt::new(StmtKind::Expr(Box::new(expr)), span))
        }
        self.expect_symbol(Symbol::Semicolon)?;
        ret
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some((sym, span)) = match_symbol!(self, Symbol::Minus, Symbol::Tilde) {
            let op = match sym {
                Symbol::Minus => UnaryOp::Neg,
                Symbol::Tilde => UnaryOp::Comp,
                _ => unreachable!(),
            };
            let expr = self.parse_unary()?;
            Ok(Expr::new(ExprKind::Unary(op, expr.into()), span))
        } else {
            self.parse_primary()
        }
    }

    fn parse_return(&mut self) -> Result<Expr, ParserError> {
        // self.consume_keyword(Keyword::Return)?;
        let expr = self.parse_expression()?;
        let span = expr.span;
        Ok(Expr::new(ExprKind::Return(Some(Box::from(expr))), span))
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.peek().ok_or(ParserError::EOT)?;
        match token.kind {
            TokenKind::Symbol(Symbol::OpenParen) => {
                self.advance().unwrap();
                let expr = self.parse_expression()?;
                self.expect_symbol(Symbol::CloseParen)?;
                Ok(expr)
            }
            TokenKind::IntNumber(n, _) => {
                self.advance().unwrap();
                Ok(Expr::new(ExprKind::Constant(n), token.span))
            }
            _ => Err(UnexpectedToken(token)),
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
