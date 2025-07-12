use colored::*;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

use crate::frontend::source::SourceFile;
use crate::frontend::span::Span;

/// A diagnostic message with source code context and highlighting.
pub struct Diagnostic {
    pub message: String,
    pub source_file: Arc<SourceFile>,
    pub span: Span,
    pub level: DiagnosticLevel,
}

/// The severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Note,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticLevel::Error => write!(f, "{}", "error".red().bold()),
            DiagnosticLevel::Warning => write!(f, "{}", "warning".yellow().bold()),
            DiagnosticLevel::Note => write!(f, "{}", "note".blue().bold()),
        }
    }
}

impl Diagnostic {
    /// Creates a new diagnostic message.
    pub fn new(
        message: String,
        source_file: Arc<SourceFile>,
        span: Span,
        level: DiagnosticLevel,
    ) -> Self {
        Self {
            message,
            source_file,
            span,
            level,
        }
    }

    /// Creates a new error diagnostic.
    pub fn error(message: String, source_file: Arc<SourceFile>, span: Span) -> Self {
        Self::new(message, source_file, span, DiagnosticLevel::Error)
    }

    /// Creates a new warning diagnostic.
    pub fn warning(message: String, source_file: Arc<SourceFile>, span: Span) -> Self {
        Self::new(message, source_file, span, DiagnosticLevel::Warning)
    }

    /// Creates a new note diagnostic.
    pub fn note(message: String, source_file: Arc<SourceFile>, span: Span) -> Self {
        Self::new(message, source_file, span, DiagnosticLevel::Note)
    }

    /// Formats the diagnostic message with source code context and highlighting.
    pub fn format(&self) -> String {
        let src = &self.source_file.content;
        let mut result = String::new();

        // Add the error message header
        result.push_str(&format!("{}: {}\n", self.level, self.message.bold()));

        // Find the line and column of the span
        let (line_number, column_start, column_end) = self.get_line_and_column(src);

        // Add the file and location information
        let file_name = match &self.source_file.name {
            crate::frontend::source::FileName::Real(path) => path.to_string_lossy().to_string(),
            crate::frontend::source::FileName::Anon(id) => format!("<anonymous-{}>", id),
        };
        result.push_str(&format!(
            " {} {}:{}:{}\n",
            "-->".cyan().bold(),
            file_name.cyan(),
            line_number.to_string().cyan(),
            column_start.to_string().cyan()
        ));

        // Add the previous line if it exists
        if line_number > 1 {
            let prev_line = self.get_line(src, line_number - 1);
            result.push_str(&format!(
                "{:4} {} {}\n",
                (line_number - 1).to_string().cyan(),
                "|".cyan().bold(),
                prev_line
            ));
        }

        // Add the source line
        let line = self.get_line(src, line_number);
        result.push_str(&format!(
            "{:4} {} {}\n",
            line_number.to_string().cyan(),
            "|".cyan().bold(),
            line
        ));

        // Add the highlighting with color based on diagnostic level
        let highlight_color = match self.level {
            DiagnosticLevel::Error => "^".repeat(column_end - column_start).red().bold(),
            DiagnosticLevel::Warning => "^".repeat(column_end - column_start).yellow().bold(),
            DiagnosticLevel::Note => "^".repeat(column_end - column_start).blue().bold(),
        };

        result.push_str(&format!(
            "     {} {}{}\n",
            "|".blue().bold(),
            " ".repeat(column_start),
            highlight_color
        ));

        // Add the next line if it exists
        let next_line = self.get_line(src, line_number + 1);
        if !next_line.is_empty() {
            result.push_str(&format!(
                "{:4} {} {}\n",
                (line_number + 1).to_string().cyan(),
                "|".cyan().bold(),
                next_line
            ));
        }

        result
    }

    /// Gets the line and column of the span.
    fn get_line_and_column(&self, src: &str) -> (usize, usize, usize) {
        let mut line_number = 1;
        let mut line_start = 0;

        for (i, c) in src.char_indices() {
            if i == self.span.start {
                // Found the start of the span
                let column_start = i - line_start;

                // Find the end of the span on the same line
                let mut column_end = column_start;
                for j in i..self.span.end.min(src.len()) {
                    if src[j..].starts_with('\n') {
                        break;
                    }
                    column_end += 1;
                }

                return (line_number, column_start, column_end);
            }

            if c == '\n' {
                line_number += 1;
                line_start = i + 1;
            }
        }

        // If we get here, the span is at the end of the file
        (line_number, src.len() - line_start, src.len() - line_start)
    }

    /// Gets the line at the given line number.
    fn get_line<'a>(&self, src: &'a str, line_number: usize) -> &'a str {
        let mut current_line = 1;
        let mut line_start = 0;

        for (i, c) in src.char_indices() {
            if current_line == line_number && (c == '\n' || i == src.len() - 1) {
                // Found the end of the line
                let end = if c == '\n' { i } else { i + 1 };
                return &src[line_start..end];
            }

            if c == '\n' {
                current_line += 1;
                line_start = i + 1;
            }
        }

        // If we get here, the line number is out of range
        ""
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}
