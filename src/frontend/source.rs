use std::path::PathBuf;
use std::sync::Arc;
use std::ops::Add;

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

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub name: FileName,
    pub content: String,
}

impl SourceFile {
    // TODO move file opening and reading to here
    pub fn new(name: PathBuf, content: String) -> Self {
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