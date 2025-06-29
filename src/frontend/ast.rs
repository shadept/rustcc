use crate::backend::tacky::Identifier;
use crate::frontend::span::Span;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub function_definition: Function,
}

impl Program {
    pub fn new(function_definition: Function) -> Self {
        Self {
            function_definition,
        }
    }

    /// Pretty prints the AST to the given writer.
    pub fn pretty_print<W: Write>(&self, writer: W) -> io::Result<()> {
        use crate::frontend::ast_printer::AstPrinter;
        let mut printer = AstPrinter::new(writer);
        printer.print_program(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Option<Block>,
}

pub type Block = Vec<BlockItem>;

impl Function {
    pub fn new(name: String, body: Block) -> Self {
        Self {
            name,
            body: Some(body),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable(Identifier, Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Null,
    Return(Box<Expr>),
}

impl StmtKind {
    pub fn into_stmt(self, span: Span) -> Stmt {
        Stmt::new(self, span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Assignment(Box<Expr>, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Constant(i32),
    Return(Option<Box<Expr>>),
    Unary(UnaryOp, Box<Expr>),
    Var(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}
