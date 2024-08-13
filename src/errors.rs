pub struct ErrorHandler {
    pub lines: Vec<String>,
}

impl ErrorHandler {
    // pub fn new(raw_program: &str) -> Self {
    //     let lines = raw_program.lines().map(|x| x.to_string()).collect();
    //
    //     Self { lines }
    // }
    //
    // pub fn print_error(&self, error: &str, loc: Loc) {
    //     let line = self.lines[loc.line].clone();
    //     let caret = " ".repeat(loc.col) + "^";
    //
    //     eprintln!("Error: {}", error);
    //     eprintln!("{}: {}", loc.line, line);
    //     eprintln!("{}{}", " ".repeat(loc.line.to_string().len() + 2), caret);
    // }
}
