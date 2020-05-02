use crate::object::Object;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}

impl Environment {
    #[inline]
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    #[inline]
    pub fn get(&self, name: &String) -> Option<&Object> {
        self.store.get(name)
    }

    #[inline]
    pub fn set(&mut self, name: String, val: &Object) {
        self.store.insert(name, val.clone());
    }
}
