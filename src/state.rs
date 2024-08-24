use crate::eval::EvalResult;
use std::collections::HashMap;

pub struct State {
    variables: HashMap<String, EvalResult>,
}

impl State {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&EvalResult> {
        self.variables.get(name)
    }

    pub fn set(&mut self, name: &str, value: EvalResult) {
        self.variables.insert(name.to_string(), value);
    }
}
