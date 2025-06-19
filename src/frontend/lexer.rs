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

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_trivia();
        self.start = self.index;
        if self.is_eof() {
            return Err(LexerError::EOF);
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
            '-' => {
                let sym = if self.match_char('-') {
                    Symbol::MinusMinus
                } else {
                    Symbol::Minus
                };
                self.create_token_from_symbol(sym)
            }
            '~' => self.create_token_from_symbol(Symbol::Tilde),
            ch if ch.is_digit(10) => self.parse_number(),
            ch if ch.is_alphabetic() => self.parse_identifier_or_keyword(),
            _ => return Err(LexerError::UnexpectedToken),
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

    fn create_token(&mut self, kind: TokenKind) -> Token {
        let span = Span::new(self.start, self.index);
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

        if self.peek() != Some('.') {
            let number = self
                .current_lexeme()
                .parse::<i64>()
                .expect("Failed to parse number");
            return self.create_token(TokenKind::IntNumber(number, Bits::from(number)));
        }
        // TODO parse rest of float
        let number = self
            .current_lexeme()
            .parse::<f64>()
            .expect("Failed to parse number");
        self.create_token(TokenKind::FloatNumber(number))
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

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().ok()
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedToken,
    EOF,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedToken => write!(f, "Unexpected token"),
            LexerError::EOF => write!(f, "End of file"),
        }
    }
}

impl Error for LexerError {}
