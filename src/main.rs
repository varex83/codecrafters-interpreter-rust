mod eval;
mod parser;
mod scanner;

use crate::eval::Evaluate;
use crate::parser::Parser;
use crate::scanner::Token;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

fn tokenize(filename: &str) -> (Vec<Token>, bool) {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });

    let mut scanner = scanner::Scanner::new(&file_contents);

    let tokens = scanner.scan_tokens();

    let is_error = !scanner.errors.is_empty();

    for error in scanner.errors {
        writeln!(io::stderr(), "{}", error).unwrap();
    }

    (tokens, is_error)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let (tokens, is_error) = tokenize(filename);

            for token in tokens {
                println!("{}", token);
            }

            if is_error {
                exit(65)
            } else {
                exit(0)
            }
        }
        "parse" => {
            let (tokens, is_error_lexing) = tokenize(filename);

            let parsed = Parser::new_parse(tokens);

            let is_error_parsing = parsed.is_err();

            if is_error_parsing {
                writeln!(
                    io::stderr(),
                    "Error while parsing {}: {}",
                    filename,
                    parsed.unwrap_err()
                )
                .unwrap();
            } else {
                let ast = parsed.unwrap();

                println!("{}", ast);
            }

            if is_error_lexing || is_error_parsing {
                exit(65)
            } else {
                exit(0)
            }
        }
        "evaluate" => {
            let (tokens, _is_error_lexing) = tokenize(filename);

            let parsed = Parser::new_parse(tokens);

            let is_error_parsing = parsed.is_err();

            if is_error_parsing {
                writeln!(
                    io::stderr(),
                    "Error while parsing {}: {}",
                    filename,
                    parsed.unwrap_err()
                )
                .unwrap();
                exit(65);
            }

            let ast = parsed.unwrap();

            let result = ast.eval();

            match result {
                Ok(eval_result) => println!("{}", eval_result),
                Err(e) => {
                    writeln!(io::stderr(), "Error while evaluating {}: {}", filename, e).unwrap();
                    exit(70);
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
