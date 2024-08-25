use crate::eval::EvalResult;
use anyhow::bail;
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

    pub fn declare(&mut self, name: &str, value: EvalResult) -> anyhow::Result<()> {
        if self.variables.contains_key(name) {
            bail!("RUNTIME_ERR: Variable {} is already declared", name);
        }

        self.variables.insert(name.to_string(), value);

        Ok(())
    }

    pub fn assign(&mut self, name: &str, value: EvalResult) -> anyhow::Result<()> {
        if !self.variables.contains_key(name) {
            bail!("RUNTIME_ERR: Variable {} is not declared", name);
        }

        self.variables.insert(name.to_string(), value);

        Ok(())
    }
}
