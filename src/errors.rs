use crate::token::Loc;
use colored::*;
use std::fmt::Display;

pub struct ErrorHandler {
    pub lines: Vec<String>,
}

impl ErrorHandler {
    pub fn new(raw_program: &str) -> Self {
        let lines = raw_program.lines().map(|x| x.to_string()).collect();
        Self { lines }
    }

    pub fn print_error<T>(&self, error: T, loc: Loc)
    where
        T: Display,
    {
        let line = self.lines[loc.line as usize].clone();
        let caret = " ".repeat(loc.col as usize) + "^";

        let line_number = (loc.line + 1).to_string();
        let line_width = line_number.len();

        let line_prefix = " ".repeat(line_width + 1);

        eprintln!(
            "{} {}",
            (line_prefix + "|").bold().on_bright_cyan(),
            format!("Error: {}", error).red().bold()
        );

        eprintln!(
            "{} {}",
            (line_number + " |").bold().on_bright_cyan(),
            line.cyan()
        );

        eprintln!(
            "{}{}",
            " ".repeat(loc.line.to_string().len() + line_width + 1),
            caret.yellow().bold()
        );
    }
}
