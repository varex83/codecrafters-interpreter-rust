mod consts;
mod errors;
mod eval;
mod parser;
mod scanner;
mod state;
mod token;

use crate::eval::Evaluate;
use crate::parser::Parser;
use crate::token::Token;
use std::env;
use std::fs;
use std::process::exit;

fn tokenize(filename: &str) -> (Vec<Token>, bool) {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    let error_handler = errors::ErrorHandler::new(&file_contents);

    let mut scanner = scanner::Scanner::new(&file_contents);

    let tokens = scanner.scan_tokens();

    let is_error = !scanner.errors.is_empty();

    for error in scanner.errors {
        error_handler.print_error(&error, error.loc);
    }

    (tokens, is_error)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
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
                eprintln!("Error while parsing {}: {}", filename, parsed.unwrap_err());
            } else {
                let ast = parsed.unwrap();

                println!("{:?}", ast);
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
                eprintln!("Error while parsing {}: {}", filename, parsed.unwrap_err());
                exit(65);
            }

            let ast = parsed.unwrap();

            let mut state = state::State::new();

            let result = ast.eval(&mut state);

            match result {
                Ok(eval_result) => {
                    println!("{:?}", eval_result);
                }
                Err(e) => {
                    eprintln!("Error while evaluating {}: {}", filename, e);
                    exit(70);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
