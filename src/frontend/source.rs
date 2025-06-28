use std::path::PathBuf;
use std::sync::Arc;

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
    pub src: Option<Arc<String>>,
}
