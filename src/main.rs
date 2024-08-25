use std::env;
use std::fs;

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
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
}

fn tokenize_command(filename: &str) {
    let file_contents = fs::read_to_string(filename)
        .expect(&format!("failed to read file {filename}"));

    // Uncomment this block to pass the first stage
    // if !file_contents.is_empty() {
    //     panic!("Scanner not implemented");
    // } else {
    //     println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
    // }
}
