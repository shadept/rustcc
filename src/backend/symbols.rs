use std::collections::HashMap;
use crate::backend::tacky::Identifier;

pub type VariableMap = HashMap<Identifier, MapEntry>;

pub struct MapEntry {
    pub name: Identifier,
    pub from_current_block: bool,
}

impl MapEntry {
    pub fn new(name: Identifier) -> Self {
        Self {
            name,
            from_current_block: true,
        }
    }
}

impl Clone for MapEntry {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            from_current_block: false,
        }
    }
}