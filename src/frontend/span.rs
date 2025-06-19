/// Represents a region of source code, used for error reporting. Positions are relative to the
/// start of the source file they correspond to. Retrieving source code lines using a span that
/// is generated from a different source file has undefined behavior.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}
