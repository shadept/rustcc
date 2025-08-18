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
    /// Converts a `PathBuf` into a `FileName::Real` variant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// let p = PathBuf::from("src/lib.rs");
    /// let f = FileName::from(p.clone());
    /// assert_eq!(f, FileName::Real(p));
    /// ```
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
    /// Create a `SourceFile` by reading the file at `name`.
    ///
    /// The file is read as UTF-8 text (this function will panic if reading or UTF-8 decoding fails).
    /// If the file starts with a UTF-8 BOM (U+FEFF) it is removed. If the file starts with a
    /// shebang (`#!`), the first line (up to and including the first newline, or the whole content
    /// if no newline) is removed. The returned `SourceFile` has `FileName::Real(name)`.
    ///
    /// # Panics
    ///
    /// Panics if the file cannot be read or is not valid UTF-8 (due to the use of `read_to_string().unwrap()`).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use std::fs;
    /// // prepare a temporary file
    /// let mut p = std::env::temp_dir();
    /// p.push("example_source.rs");
    /// fs::write(&p, "#!/usr/bin/env rust\nfn main() {}\n").unwrap();
    ///
    /// let sf = crate::frontend::source::SourceFile::new_from_file(p.clone());
    /// assert!(sf.content.contains("fn main"));
    /// assert_eq!(sf.name, crate::frontend::source::FileName::Real(p));
    /// ```
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

    /// Extends the left span to cover through the end of the right span.
    ///
    /// The two spans must reference the same source; this function will panic if
    /// they come from different `SourceFile` instances.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// // construct an anonymous source and two spans within it
    /// let src = Arc::new(crate::frontend::source::SourceFile::new_anon("abcdef"));
    /// let a = crate::frontend::source::Span::new(src.clone(), 0, 2); // covers "ab"
    /// let b = crate::frontend::source::Span::new(src.clone(), 2, 5); // covers "cde"
    /// let combined = a + b;
    /// assert_eq!(combined.start, 0);
    /// assert_eq!(combined.end, 5);
    /// assert!(Arc::ptr_eq(&combined.source, &src));
    /// ```
    fn add(self, rhs: Self) -> Self::Output {
        assert_eq!(self.source, rhs.source);
        Span {
            source: self.source.clone(),
            start: self.start,
            end: rhs.end,
        }
    }
}
