use std::env;
use std::fs;
use std::fmt::{Display, Formatter};

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
    let tokens = tokenize_string(&file_contents);
    for token in tokens {
        println!("{token}");
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    EOF,
}
#[derive(Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    code: String,
    literal: Option<String>,
    row: usize,
    col: usize,
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?} {} {}",
            self.kind,
            self.code,
            match &self.literal {
                Some(x) => x.as_str(),
                None => "null",
            },
        ))
    }
}

fn tokenize_string(str: &str) -> Vec<Token> {
    let chars = str.chars().collect::<Vec<_>>();
    let mut row = 1usize;
    let mut col = 1usize;
    let mut tokens = vec![];
    for index in 0..chars.len() {
        let char = chars[index];
        if char == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }

        match char {
            _ => panic!("Unexpected character {char} code {:X} at char position {row}:{col}", char as u32),
        }
    }
    let eof = Token {
        kind: TokenKind::EOF,
        code: "".to_string(),
        literal: None,
        row,
        col: if col == 1 { 1 } else { col + 1 },
    };
    tokens.push(eof);
    tokens
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_empty() {
        let str = "";
        let expected_tokens = "EOF  null";
        let tokens = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
    }

    fn tokens_as_string(tokens: &[Token]) -> String {
        tokens.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}
