﻿use crate::backend::common::make_unique;
use crate::backend::tacky::Identifier;
use crate::frontend::ast::{
    BlockItem, Decl, DeclKind, Expr, ExprKind, Function, Program, Stmt, StmtKind,
};
use crate::frontend::diagnostic::Diagnostic;
use crate::frontend::parser::ParserError;
use crate::frontend::source::SourceFile;
use crate::frontend::span::Span;
use std::collections::HashMap;
use std::sync::Arc;

type VariableMap = HashMap<Identifier, Identifier>;

pub fn resolve_program(program: Program) -> Result<Program, SemanticError> {
    let mut map = VariableMap::new();
    let function = resolve_function(program.function_definition, &mut map)?;
    Ok(Program::new(function))
}

fn resolve_function(func: Function, map: &mut VariableMap) -> Result<Function, SemanticError> {
    if let Some(items) = func.body {
        let mut body = Vec::new();
        for item in items {
            match item {
                BlockItem::Decl(decl) => body.push(BlockItem::Decl(resolve_decl(decl, map)?)),
                BlockItem::Stmt(stmt) => body.push(BlockItem::Stmt(resolve_stmt(stmt, map)?)),
            }
        }
        Ok(Function::new(func.name, body))
    } else {
        Ok(func)
    }
}

fn resolve_decl(decl: Decl, map: &mut VariableMap) -> Result<Decl, SemanticError> {
    match decl.kind {
        DeclKind::Variable(name, init) => resolve_variable_decl(name, init, decl.span, map),
    }
}

fn resolve_variable_decl(
    name: Identifier,
    init: Option<Expr>,
    span: Span,
    map: &mut VariableMap,
) -> Result<Decl, SemanticError> {
    if map.contains_key(&name) {
        return Err(SemanticError::DuplicatedVariableDeclaration(name, span));
    }
    let unique_name = make_unique(&name);
    map.insert(name, unique_name.clone());
    if let Some(init) = init {
        let init = resolve_expr(&init, map)?;
        return Ok(Decl::new(DeclKind::Variable(unique_name, Some(init)), span));
    }
    Ok(Decl::new(DeclKind::Variable(unique_name, None), span))
}

fn resolve_stmt(stmt: Stmt, map: &mut VariableMap) -> Result<Stmt, SemanticError> {
    match stmt.kind {
        StmtKind::Expr(expr) => {
            Ok(StmtKind::Expr(resolve_expr(&expr, map)?.into()).into_stmt(stmt.span))
        }
        StmtKind::If(cond, if_true, if_false) => Ok(StmtKind::If(
            resolve_expr(&cond, map)?.into(),
            resolve_stmt(*if_true, map)?.into(),
            match if_false {
                Some(if_false) => Some(resolve_stmt(*if_false, map)?.into()),
                None => None,
            },
        )
        .into_stmt(stmt.span)),
        StmtKind::Null => Ok(stmt),
        StmtKind::Return(expr) => {
            Ok(StmtKind::Return(resolve_expr(&expr, map)?.into()).into_stmt(stmt.span))
        }
    }
}

fn resolve_expr(expr: &Expr, map: &mut VariableMap) -> Result<Expr, SemanticError> {
    match &expr.kind {
        ExprKind::Assignment(left, right) => {
            if !matches!(left.kind, ExprKind::Var(_)) {
                Err(SemanticError::InvalidLValue(left.clone()))
            } else {
                let left = resolve_expr(&left, map)?;
                let right = resolve_expr(&right, map)?;
                Ok(Expr::new(
                    ExprKind::Assignment(left.into(), right.into()),
                    expr.span,
                ))
            }
        }
        ExprKind::Binary(op, left, right) => Ok(Expr::new(
            ExprKind::Binary(
                op.clone(),
                resolve_expr(&left, map)?.into(),
                resolve_expr(&right, map)?.into(),
            ),
            expr.span,
        )),
        ExprKind::Cond(cond, if_true, if_false) => Ok(Expr::new(
            ExprKind::Cond(
                resolve_expr(&cond, map)?.into(),
                resolve_expr(&if_true, map)?.into(),
                resolve_expr(&if_false, map)?.into(),
            ),
            expr.span,
        )),
        ExprKind::Constant(literal) => {
            Ok(Expr::new(ExprKind::Constant(literal.clone()), expr.span))
        }
        ExprKind::Return(Some(e)) => Ok(Expr::new(
            ExprKind::Return(Some(resolve_expr(&*e, map)?.into())),
            expr.span,
        )),
        ExprKind::Return(None) => Ok(Expr::new(ExprKind::Return(None), expr.span)),
        ExprKind::Unary(op, expr) => Ok(Expr::new(
            ExprKind::Unary(op.clone(), resolve_expr(&expr, map)?.into()),
            expr.span,
        )),
        ExprKind::Var(name) => {
            if map.contains_key(name) {
                Ok(Expr::new(ExprKind::Var(map[name].clone()), expr.span))
            } else {
                Err(SemanticError::UndeclaredVariable(name.clone(), expr.span))
            }
        }
    }
}

#[derive(Debug)]
pub enum SemanticError {
    DuplicatedVariableDeclaration(Identifier, Span),
    InvalidLValue(Box<Expr>),
    UndeclaredVariable(Identifier, Span),
}

impl SemanticError {
    pub fn diagnostic(&self, source_file: Arc<SourceFile>) -> Diagnostic {
        match self {
            SemanticError::DuplicatedVariableDeclaration(name, span) => {
                Diagnostic::error(self.to_string(), source_file, *span)
            }
            SemanticError::InvalidLValue(token) => {
                Diagnostic::error(self.to_string(), source_file, token.span)
            }
            SemanticError::UndeclaredVariable(name, span) => {
                Diagnostic::error(self.to_string(), source_file, *span)
            }
        }
    }
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::DuplicatedVariableDeclaration(name, span) => {
                write!(f, "duplicated variable declaration: {}", name)
            }
            SemanticError::InvalidLValue(_) => write!(f, "invalid lvalue"),
            SemanticError::UndeclaredVariable(name, span) => {
                write!(f, "undeclared variable: {}", name)
            }
        }
    }
}

impl std::error::Error for SemanticError {}
