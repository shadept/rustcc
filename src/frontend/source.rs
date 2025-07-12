use std::path::PathBuf;

#[derive(Debug)]
pub enum FileName {
    Real(PathBuf),
    Anon(usize),
}

impl From<PathBuf> for FileName {
    fn from(path: PathBuf) -> Self {
        FileName::Real(path)
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub name: FileName,
    pub content: String,
}

impl SourceFile {
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
