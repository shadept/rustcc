use crate::frontend::span::Span;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "int" => Ok(Keyword::Int),
            "void" => Ok(Keyword::Void),
            "return" => Ok(Keyword::Return),
            _ => Err(()),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Int => write!(f, "int"),
            Keyword::Void => write!(f, "void"),
            Keyword::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Semicolon,
    Colon,
    Dot,
    Minus,
    Tilde,
    MinusMinus,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let glyph = match self {
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenBrace => "{{",
            Symbol::CloseBrace => "}}",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
            Symbol::Comma => ",",
            Symbol::Semicolon => ";",
            Symbol::Colon => ":",
            Symbol::Dot => ".",
            Symbol::Minus => "-",
            Symbol::Tilde => "~",
            Symbol::MinusMinus => "--",
        };
        write!(f, "{}", glyph)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bits {
    B8,
    B16,
    B32,
    B64,
}

impl From<i64> for Bits {
    fn from(value: i64) -> Self {
        match value.abs().ilog2() {
            0..=7 => Bits::B8,
            8..=15 => Bits::B16,
            16..=31 => Bits::B32,
            32..=63 => Bits::B64,
            _ => panic!("Invalid bit size"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Symbol(Symbol),
    Identifier(String),
    Char(char),
    IntNumber(i64, Bits),
    FloatNumber(f64),
    String(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Keyword(keyword) => write!(f, "{}", keyword),
            TokenKind::Symbol(symbol) => write!(f, "{}", symbol),
            TokenKind::Identifier(identifier) => write!(f, "{}", identifier),
            TokenKind::Char(c) => write!(f, "{}", c),
            TokenKind::IntNumber(number, bits) => write!(f, "{}", number),
            TokenKind::FloatNumber(number) => write!(f, "{}", number),
            TokenKind::String(string) => write!(f, "{}", string),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
