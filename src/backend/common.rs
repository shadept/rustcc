use std::sync::atomic::{AtomicUsize, Ordering};
use crate::backend::tacky::Identifier;

pub fn make_unique(name: &str) -> Identifier {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("{}.{}", name, COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn make_temporary() -> Identifier {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("tmp.{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn make_label() -> Identifier {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}
