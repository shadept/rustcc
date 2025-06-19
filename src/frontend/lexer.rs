use crate::frontend::span::Span;
use crate::frontend::token::{Bits, Keyword, Symbol, Token, TokenKind};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    index: usize,
    start: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Lexer<'src> {
        let chars = src.chars();
        Lexer {
            src,
            chars,
            index: 0,
            start: 0,
        }
    }

    pub fn to_tokens(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let res = self.next_token();
            match res {
                Ok(token) => tokens.push(token),
                Err(LexerError::EOF(_)) => break,
                Err(err) => return Err(err),
            }
        }
        Ok(tokens)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_trivia();
        self.start = self.index;
        if self.is_eof() {
            return Err(LexerError::EOF(self.create_span()));
        }

        let ch = self.advance().unwrap();
        let token = match ch {
            '(' => self.create_token_from_symbol(Symbol::OpenParen),
            ')' => self.create_token_from_symbol(Symbol::CloseParen),
            '{' => self.create_token_from_symbol(Symbol::OpenBrace),
            '}' => self.create_token_from_symbol(Symbol::CloseBrace),
            '[' => self.create_token_from_symbol(Symbol::OpenBracket),
            ']' => self.create_token_from_symbol(Symbol::CloseBracket),
            ';' => self.create_token_from_symbol(Symbol::Semicolon),
            '~' => self.create_token_from_symbol(Symbol::Tilde),
            '+' => self.create_token_from_symbol(Symbol::Plus),
            '-' => {
                let sym = if self.match_char('-') {
                    Symbol::MinusMinus
                } else {
                    Symbol::Minus
                };
                self.create_token_from_symbol(sym)
            }
            '*' => self.create_token_from_symbol(Symbol::Star),
            '/' => self.create_token_from_symbol(Symbol::Slash),
            '%' => self.create_token_from_symbol(Symbol::Percent),
            ch if ch.is_digit(10) => self.parse_number(),
            ch if ch.is_alphabetic() => self.parse_identifier_or_keyword(),
            _ => return Err(LexerError::UnexpectedToken(self.create_span())),
        };

        Ok(token)
    }

    fn skip_trivia(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else if c == '/' && self.peek2() == Some('/') {
                self.advance();
                self.advance();
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            } else if c == '/' && self.peek2() == Some('*') {
                self.advance();
                self.advance();
                while let Some(c) = self.advance() {
                    if c == '*' && self.peek() == Some('/') {
                        self.advance();
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek2(&self) -> Option<char> {
        let mut c = self.chars.clone();
        c.next();
        c.next()
    }

    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.chars.next()
    }

    fn match_char(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn is_eof(&self) -> bool {
        self.peek().is_none()
    }

    fn current_lexeme(&self) -> &str {
        &self.src[self.start..self.index]
    }

    fn create_span(&mut self) -> Span {
        Span::new(self.start, self.index)
    }

    fn create_token(&mut self, kind: TokenKind) -> Token {
        let span = self.create_span();
        let token = Token { kind, span };
        token
    }

    fn create_token_from_symbol(&mut self, symbol: Symbol) -> Token {
        self.create_token(TokenKind::Symbol(symbol))
    }

    fn parse_number(&mut self) -> Token {
        // We already know the first character is a digit
        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        let number = self
            .current_lexeme()
            .parse::<i64>()
            .expect("Failed to parse number");
        self.create_token(TokenKind::IntNumber(number, Bits::from(number)))
    }

    fn parse_identifier_or_keyword(&mut self) -> Token {
        // We already know the first character is alphabetic
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if let Ok(keyword) = Keyword::try_from(self.current_lexeme()) {
            return self.create_token(TokenKind::Keyword(keyword));
        }

        self.create_token(TokenKind::Identifier(self.current_lexeme().into()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedToken(Span),
    EOF(Span),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedToken(span) => write!(f, "Unexpected token @ {span:?}"),
            LexerError::EOF(span) => write!(f, "End of file @ {span:?}"),
        }
    }
}

impl Error for LexerError {}
