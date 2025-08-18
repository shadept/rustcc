use crate::frontend::ast::{
    BinaryOp, BlockItem, Decl, DeclKind, Expr, ExprKind, Function, Program, Stmt, StmtKind, UnaryOp,
};
use crate::frontend::diagnostic::Diagnostic;
use crate::frontend::source::SourceFile;
use crate::frontend::source::Span;
use crate::frontend::token::{Keyword, Symbol, Token, TokenKind};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use std::vec::IntoIter;

#[derive(Debug)]
pub struct Parser {
    source: Arc<SourceFile>,
    tokens: IntoIter<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source_file: Arc<SourceFile>) -> Self {
        let iter = tokens.into_iter();
        Parser {
            source: source_file,
            tokens: iter,
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

    fn parse_block(&mut self) -> Result<Stmt, ParserError> {
        let open_brace = self.expect_symbol(Symbol::OpenBrace)?;
        let mut items = vec![];
        let mut close_brace = None;

        loop {
            if let Some(token) = self.match_symbol(Symbol::CloseBrace) {
                close_brace = Some(token);
                break;
            }

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

        let span = open_brace.span + close_brace.unwrap().span;
        Ok(Stmt::new(StmtKind::Compound(items), span))
    }

    fn parse_declaration(&mut self) -> Result<Decl, ParserError> {
        if let Some(tok) = self.match_keyword(Keyword::Int) {
            let identifier = self.expect_identifier()?;
            let init = if self.match_symbol(Symbol::Equal).is_some() {
                Some(self.parse_expression(0)?)
            } else {
                None
            };
            self.expect_symbol(Symbol::Semicolon)?;
            return Ok(Decl::new(DeclKind::Variable(identifier, init), tok.span));
        }
        Err(ParserError::UnexpectedToken(self.peek().unwrap()))
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if let Some(tok) = self.match_symbol(Symbol::Semicolon) {
            return Ok(Stmt::new(StmtKind::Null, tok.span));
        }

        let tok = self.peek_or_eof()?;
        let mut ret: Result<Stmt, ParserError> = Err(ParserError::UnexpectedToken(tok.clone()));
        if matches!(self.peek(), Some(tok) if tok.kind == TokenKind::Symbol(Symbol::OpenBrace)) {
            return self.parse_block();
        } else if let TokenKind::Keyword(kw) = &tok.kind {
            match kw {
                Keyword::Break => {
                    self.advance().unwrap();
                    ret = Ok(Stmt::new(StmtKind::Break("".to_string()), tok.span));
                }
                Keyword::Continue => {
                    self.advance().unwrap();
                    ret = Ok(Stmt::new(StmtKind::Continue("".to_string()), tok.span));
                }
                Keyword::Do => {
                    self.advance().unwrap();
                    let stmt = self.parse_statement()?;
                    self.expect_keyword(Keyword::While)?;
                    self.expect_symbol(Symbol::OpenParen)?;
                    let expr = self.parse_expression(0)?;
                    self.expect_symbol(Symbol::CloseParen)?;
                    ret = Ok(Stmt::new(
                        StmtKind::DoWhile(stmt.into(), expr.into(), "".into()),
                        tok.span,
                    ));
                }
                Keyword::For => {
                    self.advance().unwrap();
                    todo!();
                }
                Keyword::If => {
                    self.advance().unwrap();
                    return self.parse_if_stmt(tok.span);
                }
                Keyword::Return => {
                    self.advance().unwrap();
                    let expr = self.parse_expression(0)?;
                    let span = expr.span.clone();
                    let expr = Expr::new(ExprKind::Return(Some(Box::from(expr))), span);
                    ret = Ok(Stmt::new(StmtKind::Return(Box::from(expr)), tok.span));
                }
                Keyword::While => {
                    self.advance().unwrap();
                    self.expect_keyword(Keyword::While)?;
                    self.expect_symbol(Symbol::OpenParen)?;
                    let expr = self.parse_expression(0)?;
                    self.expect_symbol(Symbol::CloseParen)?;
                    let stmt = self.parse_statement()?;
                    return Ok(Stmt::new(
                        StmtKind::While(expr.into(), stmt.into(), "".into()),
                        tok.span,
                    ));
                }
                _ => {}
            }
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
        let condition = self.parse_expression(0)?;
        self.expect_symbol(Symbol::CloseParen)?;

        let then_stmt = self.parse_statement()?;

        let else_stmt = if self.match_keyword(Keyword::Else).is_some() {
            Some(self.parse_statement()?)
        } else {
            None
        };

        let end_span = else_stmt
            .as_ref()
            .map(|stmt| stmt.span.clone())
            .unwrap_or_else(|| then_stmt.span.clone());
        let span = if_span + end_span;

        Ok(Stmt::new(
            StmtKind::If(
                condition.into(),
                then_stmt.into(),
                else_stmt.map(|f| f.into()),
            ),
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
                    let span = left.span.clone() + token.span.clone() + right.span.clone();
                    Expr::new(ExprKind::Assignment(left.into(), right.into()), span)
                } else if token.kind == TokenKind::Symbol(Symbol::Question) {
                    let middle = self.parse_conditional_middle()?;
                    let right = self.parse_expression(precedence)?;
                    let span = left.span.clone() + right.span.clone();
                    Expr::new(
                        ExprKind::Cond(left.into(), middle.into(), right.into()),
                        span,
                    )
                } else {
                    let op = self.parse_binary_op(&token).unwrap();
                    let right = self.parse_expression(precedence + 1)?;
                    let span = left.span.clone() + token.span.clone() + right.span.clone();
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
        let token = self.peek_or_eof()?;
        if let Some(op) = self.parse_unary_op(&token) {
            let token = self.advance().unwrap();
            let expr = self.parse_factor()?;
            let span = expr.span.clone() + token.span;
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
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr, ParserError> {
        if let Some(tok) = self.peek() {
            return match tok.kind {
                TokenKind::Identifier(ident) => {
                    self.advance().unwrap();
                    Ok(Expr::new(ExprKind::Var(ident), tok.span))
                }
                _ => Err(ParserError::UnexpectedToken(tok)),
            };
        }
        Err(self.eof())
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

    fn peek_or_eof(&self) -> Result<Token, ParserError> {
        self.tokens.clone().next().ok_or_else(|| self.eof())
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
        let token = self.peek_or_eof()?;
        match token.kind {
            TokenKind::Identifier(ident) => {
                self.advance().unwrap();
                Ok(ident)
            }
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<Token, ParserError> {
        let token = self.peek_or_eof()?;
        if matches!(&token.kind, TokenKind::Keyword(kw) if *kw == keyword) {
            Ok(self.advance().unwrap())
        } else {
            Err(ParserError::UnexpectedToken(token))
        }
    }

    fn expect_symbol(&mut self, symbol: Symbol) -> Result<Token, ParserError> {
        let token = self.peek_or_eof()?;
        if matches!(&token.kind, TokenKind::Symbol(sym) if *sym == symbol) {
            Ok(self.advance().unwrap())
        } else {
            Err(ParserError::UnexpectedToken(token))
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Option<Token> {
        let token = self.peek()?;
        if matches!(&token.kind, TokenKind::Keyword(kw) if *kw == keyword) {
            Some(self.advance().unwrap())
        } else {
            None
        }
    }

    fn match_symbol(&mut self, symbol: Symbol) -> Option<Token> {
        let token = self.peek()?;
        if matches!(&token.kind, TokenKind::Symbol(sym) if *sym == symbol) {
            Some(self.advance().unwrap())
        } else {
            None
        }
    }

    fn eof(&self) -> ParserError {
        let len = self.source.content.len();
        let span = Span::new(self.source.clone(), len, len);
        ParserError::EOF(span)
    }
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnconsumedToken(Token),
    EOF(Span),
}

impl ParserError {
    /// Creates a diagnostic for this error.
    pub fn diagnostic(&self) -> Diagnostic {
        let (msg, span) = match self {
            ParserError::UnexpectedToken(token) => (
                format!("Unexpected token `{}`", token.kind),
                token.span.clone(),
            ),
            ParserError::UnconsumedToken(token) => (
                format!("Unconsumed token in stream `{}`", token.kind),
                token.span.clone(),
            ),
            ParserError::EOF(span) => ("Unexpected end of file".to_string(), span.clone()),
        };

        Diagnostic::error(msg.to_string(), span)
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(token) => write!(f, "Unexpected token `{}`", token.kind),
            ParserError::UnconsumedToken(token) => {
                write!(f, "Unconsumed token in stream `{}`", token.kind)
            }
            ParserError::EOF(span) => write!(f, "End of tokens"),
        }
    }
}

impl Error for ParserError {}
