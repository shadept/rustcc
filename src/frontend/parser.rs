use crate::frontend::ast::{
    BinaryOp, BlockItem, Decl, DeclKind, Expr, ExprKind, Function, Program, Stmt, StmtKind, UnaryOp,
};
use crate::frontend::diagnostic::Diagnostic;
use crate::frontend::parser::ParserError::UnexpectedToken;
use crate::frontend::source::SourceFile;
use crate::frontend::span::Span;
use crate::frontend::token::{Keyword, Symbol, Token, TokenKind};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
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
    source_file: Arc<SourceFile>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source_file: Arc<SourceFile>) -> Self {
        let iter = tokens.into_iter();
        Parser {
            tokens: iter,
            source_file,
        }
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
        let name = self.expect_identifier()?;
        self.expect_symbol(Symbol::OpenParen)?;
        self.expect_keyword(Keyword::Void)?;
        self.expect_symbol(Symbol::CloseParen)?;
        let body = self.parse_block()?;
        Ok(Function::new(name, body))
    }

    fn parse_block(&mut self) -> Result<Vec<BlockItem>, ParserError> {
        self.expect_symbol(Symbol::OpenBrace)?;
        let mut items = vec![];
        while match_symbol!(self, Symbol::CloseBrace).is_none() {
            if let Some(tok) = self.peek() {
                if tok.kind == TokenKind::Keyword(Keyword::Int) {
                    let decl = self.parse_declaration()?;
                    items.push(BlockItem::Decl(decl));
                    continue;
                }
            }
            let stmt = self.parse_statement()?;
            items.push(BlockItem::Stmt(stmt));
        }
        Ok(items)
    }

    fn parse_declaration(&mut self) -> Result<Decl, ParserError> {
        if let Some((_, span)) = match_keyword!(self, Keyword::Int) {
            let identifier = self.expect_identifier()?;
            let init = if match_symbol!(self, Symbol::Equal).is_some() {
                Some(self.parse_expression(0)?)
            } else {
                None
            };
            self.expect_symbol(Symbol::Semicolon)?;
            return Ok(Decl::new(DeclKind::Variable(identifier, init), span));
        }
        Err(UnexpectedToken(self.peek().unwrap()))
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some((_, span)) = match_symbol!(self, Symbol::Semicolon) {
            return Ok(Stmt::new(StmtKind::Null, span));
        }

        let ret: Result<Stmt, ParserError>;
        if let Some((_, span)) = match_keyword!(self, Keyword::Return) {
            let expr1 = self.parse_expression(0)?;
            let span1 = expr1.span;
            let expr = Expr::new(ExprKind::Return(Some(Box::from(expr1))), span1);
            ret = Ok(Stmt::new(StmtKind::Return(Box::from(expr)), span));
        } else if let Some((_, span)) = match_keyword!(self, Keyword::If) {
            return self.parse_if_stmt(span);
        } else {
            let expr = self.parse_expression(0)?;
            let span = expr.span.clone();
            ret = Ok(Stmt::new(StmtKind::Expr(Box::new(expr)), span))
        }
        self.expect_symbol(Symbol::Semicolon)?;
        ret
    }

    fn parse_if_stmt(&mut self, if_span: Span) -> Result<Stmt, ParserError> {
        // let if_tok = self.expect_keyword(Keyword::If)?;
        self.expect_symbol(Symbol::OpenParen)?;
        let cond = self.parse_expression(0)?;
        self.expect_symbol(Symbol::CloseParen)?;
        let then_stmt = self.parse_statement()?;
        let else_stmt = if match_keyword!(self, Keyword::Else).is_some() {
            Some(self.parse_statement()?)
        } else {
            None
        };
        let span = if_span + else_stmt.clone().unwrap_or_else(|| then_stmt.clone()).span;
        Ok(Stmt::new(
            StmtKind::If(cond.into(), then_stmt.into(), else_stmt.map(|f| f.into())),
            span,
        ))
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr, ParserError> {
        let left = self.parse_factor()?;
        self.parse_binary_expression(left, min_precedence)
    }

    fn parse_binary_expression(
        &mut self,
        left: Expr,
        min_precedence: u8,
    ) -> Result<Expr, ParserError> {
        let mut left = left;
        while let Some(token) = self.peek() {
            let precedence = self.binop_precedence(&token.kind);
            if let Some(precedence) = precedence {
                if precedence < min_precedence {
                    break;
                }
                self.advance().unwrap();
                left = if token.kind == TokenKind::Symbol(Symbol::Equal) {
                    let right = self.parse_expression(precedence)?;
                    let span = left.span + token.span + right.span;
                    Expr::new(ExprKind::Assignment(left.into(), right.into()), span)
                } else if token.kind == TokenKind::Symbol(Symbol::Question) {
                    let middle = self.parse_conditional_middle()?;
                    let right = self.parse_expression(precedence)?;
                    let span = left.span + right.span;
                    Expr::new(
                        ExprKind::Cond(left.into(), middle.into(), right.into()),
                        span,
                    )
                } else {
                    let op = self.parse_binary_op(&token).unwrap();
                    let right = self.parse_expression(precedence + 1)?;
                    let span = left.span + token.span + right.span;
                    Expr::new(ExprKind::Binary(op, left.into(), right.into()), span)
                }
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_conditional_middle(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_expression(0)?;
        self.expect_symbol(Symbol::Colon)?;
        Ok(expr)
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
            TokenKind::Identifier(ident) => {
                self.advance().unwrap();
                Ok(Expr::new(ExprKind::Var(ident), token.span))
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

    fn parse_identifier(&mut self) -> Result<Expr, ParserError> {
        if let Some(tok) = self.peek() {
            return match tok.kind {
                TokenKind::Identifier(ident) => {
                    self.advance().unwrap();
                    Ok(Expr::new(ExprKind::Var(ident), tok.span))
                }
                _ => Err(UnexpectedToken(tok)),
            };
        }
        Err(ParserError::EOT)
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

    fn binop_precedence(&self, op: &TokenKind) -> Option<u8> {
        if let TokenKind::Symbol(sym) = op {
            match sym {
                Symbol::Plus => Some(45),
                Symbol::Minus => Some(45),
                Symbol::Star => Some(50),
                Symbol::Slash => Some(50),
                Symbol::Percent => Some(50),
                Symbol::Pipe => Some(30),
                Symbol::Ampersand => Some(35),
                Symbol::Caret => Some(40),
                Symbol::AmpersandAmpersand => Some(10),
                Symbol::PipePipe => Some(5),
                Symbol::EqualEqual => Some(30),
                Symbol::BangEqual => Some(30),
                Symbol::LessThan => Some(35),
                Symbol::GreaterThan => Some(35),
                Symbol::LessThanOrEqual => Some(35),
                Symbol::GreaterThanOrEqual => Some(35),
                Symbol::Question => Some(3),
                Symbol::Equal => Some(1),
                _ => None,
            }
        } else {
            None
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

    fn expect_identifier(&mut self) -> Result<String, ParserError> {
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

    /// Reports an error with a diagnostic message.
    pub fn report_error(&self, error: &ParserError) {
        let diagnostic = error.diagnostic(Arc::clone(&self.source_file));
        eprintln!("{}", diagnostic);
    }
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnconsumedToken(Token),
    EOT, // end-of-tokens
}

impl ParserError {
    /// Creates a diagnostic for this error.
    pub fn diagnostic(&self, source_file: Arc<SourceFile>) -> Diagnostic {
        match self {
            ParserError::UnexpectedToken(token) => Diagnostic::error(
                format!("Unexpected token `{}`", token.kind),
                source_file,
                token.span,
            ),
            ParserError::UnconsumedToken(token) => Diagnostic::error(
                format!("Unconsumed token in stream `{}`", token.kind),
                source_file,
                token.span,
            ),
            ParserError::EOT => {
                // For EOT, we don't have a span, so we use a default span at the end of the file
                let span = if let Some(src) = &source_file.src {
                    Span::new(src.len(), src.len())
                } else {
                    Span::default()
                };
                Diagnostic::error("Unexpected end of tokens".to_string(), source_file, span)
            }
        }
    }
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
