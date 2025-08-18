use crate::frontend::source::Span;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Break,
    Continue,
    Do,
    Else,
    For,
    If,
    Int,
    Return,
    Void,
    While,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    /// Attempts to convert a string slice into a `Keyword`.
    ///
    /// Returns `Ok(Keyword::...)` for exact matches of the language's reserved words
    /// ("break", "continue", "do", "else", "if", "int", "return", "void", "while"),
    /// and `Err(())` if the input does not match any keyword.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::frontend::token::Keyword;
    ///
    /// assert_eq!(Keyword::try_from("if"), Ok(Keyword::If));
    /// assert_eq!(Keyword::try_from("while"), Ok(Keyword::While));
    /// assert!(Keyword::try_from("notakeyword").is_err());
    /// ```
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "do" => Ok(Keyword::Do),
            "else" => Ok(Keyword::Else),
            "if" => Ok(Keyword::If),
            "int" => Ok(Keyword::Int),
            "return" => Ok(Keyword::Return),
            "void" => Ok(Keyword::Void),
            "while" => Ok(Keyword::While),
            _ => Err(()),
        }
    }
}

impl Display for Keyword {
    /// Formats the `Keyword` as its canonical source text (e.g., `Keyword::If` -> "if").
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::frontend::token::Keyword;
    /// assert_eq!(format!("{}", Keyword::Break), "break");
    /// assert_eq!(format!("{}", Keyword::While), "while");
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Do => write!(f, "do"),
            Keyword::Else => write!(f, "else"),
            Keyword::For => write!(f, "for"),
            Keyword::If => write!(f, "if"),
            Keyword::Int => write!(f, "int"),
            Keyword::Return => write!(f, "return"),
            Keyword::Void => write!(f, "void"),
            Keyword::While => write!(f, "while"),
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
    Tilde,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    MinusMinus,
    Pipe,      // Bitwise OR (|)
    Ampersand, // Bitwise AND (&)
    Caret,     // Bitwise XOR (^)
    Bang,
    Question,
    Equal,
    AmpersandAmpersand,
    PipePipe,
    EqualEqual,
    BangEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let glyph = match self {
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenBrace => "{",
            Symbol::CloseBrace => "}",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
            Symbol::Comma => ",",
            Symbol::Semicolon => ";",
            Symbol::Colon => ":",
            Symbol::Dot => ".",
            Symbol::Tilde => "~",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Star => "*",
            Symbol::Slash => "/",
            Symbol::Percent => "%",
            Symbol::MinusMinus => "--",
            Symbol::Pipe => "|",
            Symbol::Ampersand => "&",
            Symbol::Caret => "^",
            Symbol::Bang => "!",
            Symbol::Question => "?",
            Symbol::Equal => "=",
            Symbol::AmpersandAmpersand => "&&",
            Symbol::PipePipe => "||",
            Symbol::EqualEqual => "==",
            Symbol::BangEqual => "!=",
            Symbol::LessThan => "<",
            Symbol::GreaterThan => ">",
            Symbol::LessThanOrEqual => "<=",
            Symbol::GreaterThanOrEqual => ">=",
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
        if value == 0 {
            return Bits::B8;
        }
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
