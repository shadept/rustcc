use crate::backend::common::make_unique;
use crate::backend::symbols::{MapEntry, VariableMap};
use crate::backend::tacky::Identifier;
use crate::frontend::ast::{
    BlockItem, Decl, DeclKind, Expr, ExprKind, Function, Program, Stmt, StmtKind,
};
use crate::frontend::diagnostic::Diagnostic;
use crate::frontend::source::Span;

/// Resolve semantic information for a whole program.
///
/// Creates a fresh variable map, resolves the program's top-level function (applying unique
/// renaming, scoping rules, and basic semantic checks), and returns a new `Program` with the
/// resolved function. If resolution fails, returns a `SemanticError` describing the problem.
///
/// # Examples
///
/// ```no_run
/// // Construct a minimal `Program` and run semantic resolution.
/// let program = /* build Program */ unimplemented!();
/// let result = resolve_program(program);
/// // `result` is `Ok(resolved_program)` on success or `Err(SemanticError)` on failure.
/// ```
pub fn resolve_program(program: Program) -> Result<Program, SemanticError> {
    let mut map = VariableMap::new();
    let function = resolve_function(program.function_definition, &mut map)?;
    Ok(Program::new(function))
}

fn resolve_function(func: Function, map: &mut VariableMap) -> Result<Function, SemanticError> {
    if let Some(items) = func.body {
        Ok(Function::new(func.name, resolve_stmt(items, map)?))
    } else {
        Ok(func)
    }
}

fn resolve_block_item(item: BlockItem, map: &mut VariableMap) -> Result<BlockItem, SemanticError> {
    match item {
        BlockItem::Decl(decl) => Ok(BlockItem::Decl(resolve_decl(decl, map)?)),
        BlockItem::Stmt(stmt) => Ok(BlockItem::Stmt(resolve_stmt(stmt, map)?)),
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
    if map.contains_key(&name) && map[&name].from_current_block {
        return Err(SemanticError::DuplicatedVariableDeclaration(name, span));
    }
    let unique_name = make_unique(&name);
    map.insert(name, MapEntry::new(unique_name.clone()));
    if let Some(init) = init {
        let init = resolve_expr(&init, map)?;
        return Ok(Decl::new(DeclKind::Variable(unique_name, Some(init)), span));
    }
    Ok(Decl::new(DeclKind::Variable(unique_name, None), span))
}

fn resolve_block(
    block: Vec<BlockItem>,
    map: &mut VariableMap,
) -> Result<Vec<BlockItem>, SemanticError> {
    let mut new_block = Vec::new();
    for item in block {
        new_block.push(resolve_block_item(item, map)?);
    }
    Ok(new_block)
}

/// Resolves names and scopes within a statement, returning a new statement with
/// variables renamed and nested scopes enforced, or a `SemanticError`.
///
/// This performs a semantic resolution pass for a single `Stmt`:
/// - `Compound` creates a new scope by cloning the provided `VariableMap` and
///   resolves the inner block; declarations inside the block do not affect the
///   outer map.
/// - Expression-containing statements (`Expr`, conditionals in `If`, `Return`)
///   are resolved recursively via `resolve_expr`.
/// - `If` resolves its condition and both branches (the `else` branch if present).
/// - `Break`, `Continue`, `DoWhile`, `For`, and `While` are preserved as-is
///   (no further resolution performed by this function).
/// - `Null` is returned unchanged.
///
/// Errors from expression or block resolution (e.g., undeclared variables,
/// invalid l-values, duplicated declarations) are propagated as
/// `SemanticError`.
///
/// # Examples
///
/// ```
/// // Minimal example: resolving a Null statement succeeds.
/// let mut map = VariableMap::new();
/// let stmt = StmtKind::Null.into_stmt(Span::default());
/// assert!(resolve_stmt(stmt, &mut map).is_ok());
/// ```
fn resolve_stmt(stmt: Stmt, map: &mut VariableMap) -> Result<Stmt, SemanticError> {
    match stmt.kind {
        StmtKind::Break(_) => Ok(stmt),
        StmtKind::Continue(_) => Ok(stmt),
        StmtKind::Compound(block) => {
            let mut new_map = map.clone(); // sets from_current_block to false
            Ok(StmtKind::Compound(resolve_block(block, &mut new_map)?).into_stmt(stmt.span))
        }
        StmtKind::DoWhile(_, _, _) => Ok(stmt),
        StmtKind::Expr(expr) => {
            Ok(StmtKind::Expr(resolve_expr(&expr, map)?.into()).into_stmt(stmt.span))
        }
        StmtKind::For(_, _, _, _, _) => Ok(stmt),
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
        StmtKind::While(_, _, _) => Ok(stmt),
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
                    expr.span.clone(),
                ))
            }
        }
        ExprKind::Binary(op, left, right) => Ok(Expr::new(
            ExprKind::Binary(
                op.clone(),
                resolve_expr(&left, map)?.into(),
                resolve_expr(&right, map)?.into(),
            ),
            expr.span.clone(),
        )),
        ExprKind::Cond(cond, if_true, if_false) => Ok(Expr::new(
            ExprKind::Cond(
                resolve_expr(&cond, map)?.into(),
                resolve_expr(&if_true, map)?.into(),
                resolve_expr(&if_false, map)?.into(),
            ),
            expr.span.clone(),
        )),
        ExprKind::Constant(literal) => Ok(Expr::new(
            ExprKind::Constant(literal.clone()),
            expr.span.clone(),
        )),
        ExprKind::Return(Some(e)) => Ok(Expr::new(
            ExprKind::Return(Some(resolve_expr(&*e, map)?.into())),
            expr.span.clone(),
        )),
        ExprKind::Return(None) => Ok(Expr::new(ExprKind::Return(None), expr.span.clone())),
        ExprKind::Unary(op, expr) => Ok(Expr::new(
            ExprKind::Unary(op.clone(), resolve_expr(&expr, map)?.into()),
            expr.span.clone(),
        )),
        ExprKind::Var(name) => {
            if map.contains_key(name) {
                Ok(Expr::new(
                    ExprKind::Var(map[name].name.clone()),
                    expr.span.clone(),
                ))
            } else {
                Err(SemanticError::UndeclaredVariable(
                    name.clone(),
                    expr.span.clone(),
                ))
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
    pub fn diagnostic(&self) -> Diagnostic {
        match self {
            SemanticError::DuplicatedVariableDeclaration(name, span) => {
                Diagnostic::error(self.to_string(), span.clone())
            }
            SemanticError::InvalidLValue(token) => {
                Diagnostic::error(self.to_string(), token.span.clone())
            }
            SemanticError::UndeclaredVariable(name, span) => {
                Diagnostic::error(self.to_string(), span.clone())
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
