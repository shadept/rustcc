//! Module for managing source code files and their metadata.
//!
//! This module provides core abstractions for working with source files,
//! including file names, content management, and source code regions (spans).
//! These components are essential for features like error reporting, code
//! analysis, and source tracking throughout the compilation process.
use std::ops::Add;
use std::path::PathBuf;
use std::sync::Arc;

/// Represents the identifier of a source file in the compilation system.
#[derive(Debug, Clone, PartialEq)]
pub enum FileName {
    Real(PathBuf),
    Anon(usize),
}

impl From<PathBuf> for FileName {
    fn from(path: PathBuf) -> Self {
        FileName::Real(path)
    }
}

/// Container for source code content and its identifier.
///
/// Holds both the content of a source file and its name/identifier. The content
/// can come either from an actual file on disk or from an in-memory buffer.
/// This struct is the primary way to track and access source code throughout
/// the compilation process.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: FileName,
    pub content: String,
}

impl SourceFile {
    pub fn new_from_file(name: PathBuf) -> Self {
        let mut content = std::fs::read_to_string(&name).unwrap();

        // Skip BOM
        if content.starts_with("\u{FEFF}") {
            content.drain(.."\u{FEFF}".len());
        }

        // Skip shebang
        if content.starts_with("#!") {
            content.drain(..=content.find('\n').unwrap_or(content.len()));
        }

        SourceFile {
            name: FileName::Real(name),
            content,
        }
    }

    pub fn new_anon<S: Into<String>>(content: S) -> Self {
        SourceFile {
            name: FileName::Anon(0),
            content: content.into(),
        }
    }
}

/// Represents a region of source code, used for error reporting. Positions are relative to the
/// start of the source file they correspond to. Retrieving source code lines using a span that
/// is generated from a different source file has undefined behavior.
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub source: Arc<SourceFile>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source: Arc<SourceFile>, start: usize, end: usize) -> Span {
        Span { source, start, end }
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source.clone(),
            start: self.start,
            end: rhs.end,
        }
    }
}
