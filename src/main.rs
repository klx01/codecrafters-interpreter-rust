mod tokenizer;
mod parser;

use std::fs;
use std::process::exit;
use std::env;
use crate::parser::parse_expression_from_string;
use crate::tokenizer::tokenize_string;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => tokenize_command(filename),
        "parse" => parse_command(filename),
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
}

fn tokenize_command(filename: &str) {
    let file_contents = fs::read_to_string(filename)
        .expect(&format!("failed to read file {filename}"));
    let (tokens, has_errors) = tokenize_string(&file_contents);
    for token in tokens {
        println!("{token}");
    }
    if has_errors {
        exit(65)
    }
}

fn parse_command(filename: &str) {
    let file_contents = fs::read_to_string(filename)
        .expect(&format!("failed to read file {filename}"));
    if let Some(expr) = parse_expression_from_string(&file_contents) {
        println!("{expr}");
    }
}

