use std::env;
use std::fs;
use std::fmt::{Display, Formatter};
use std::process::exit;

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
    let (tokens, has_errors) = tokenize_string(&file_contents);
    for token in tokens {
        println!("{token}");
    }
    if has_errors {
        exit(65)
    }
}

#[derive(Debug, PartialEq)]
#[allow(non_camel_case_types)]
enum TokenKind {
    EOF,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    STAR,
    DOT,
    COMMA,
    PLUS,
    SEMICOLON,
    MINUS,
    SLASH,
    EQUAL_EQUAL,
    EQUAL,
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

fn tokenize_string(str: &str) -> (Vec<Token>, bool) {
    let chars = str.chars().collect::<Vec<_>>();
    let mut row = 1usize;
    let mut col = 1usize;
    let mut tokens = vec![];
    let mut has_errors = false;
    let mut index = 0usize;
    while index < chars.len() {
        let char = chars[index];
        index += 1;

        if char == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }

        if let Some(next) = chars.get(index) {
            let matched_kind = match (char, next) {
                ('=', '=') => Some(TokenKind::EQUAL_EQUAL),
                _ => None,
            };
            if let Some(kind) = matched_kind {
                index += 1;
                col += 1;
                let token = Token{ kind, code: format!("{char}{next}"), literal: None, row, col };
                tokens.push(token);
                continue;
            }
        }

        let char_token_kind = match char {
            '(' => Some(TokenKind::LEFT_PAREN),
            ')' => Some(TokenKind::RIGHT_PAREN),
            '{' => Some(TokenKind::LEFT_BRACE),
            '}' => Some(TokenKind::RIGHT_BRACE),
            '*' => Some(TokenKind::STAR),
            '.' => Some(TokenKind::DOT),
            ',' => Some(TokenKind::COMMA),
            '+' => Some(TokenKind::PLUS),
            '-' => Some(TokenKind::MINUS),
            ';' => Some(TokenKind::SEMICOLON),
            '/' => Some(TokenKind::SLASH),
            '=' => Some(TokenKind::EQUAL),
            _ => None,
        };
        if let Some(kind) = char_token_kind {
            let token = Token{ kind, code: char.to_string(), literal: None, row, col };
            tokens.push(token);
            continue;
        }

        match char {
            _ => {
                eprintln!("[line {row}] Error: Unexpected character: {char}");
                has_errors = true;
            },
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
    (tokens, has_errors)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_empty() {
        let str = "";
        let expected_tokens = "EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_parens() {
        let str = "(()";
        let expected_tokens = "LEFT_PAREN ( null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_braces() {
        let str = "{{}}";
        let expected_tokens = "LEFT_BRACE { null
LEFT_BRACE { null
RIGHT_BRACE } null
RIGHT_BRACE } null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_chars() {
        let str = "({*.,+*})-;/";
        let expected_tokens = "LEFT_PAREN ( null
LEFT_BRACE { null
STAR * null
DOT . null
COMMA , null
PLUS + null
STAR * null
RIGHT_BRACE } null
RIGHT_PAREN ) null
MINUS - null
SEMICOLON ; null
SLASH / null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_equals() {
        let str = "={===}";
        let expected_tokens = "EQUAL = null
LEFT_BRACE { null
EQUAL_EQUAL == null
EQUAL = null
RIGHT_BRACE } null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    fn tokens_as_string(tokens: &[Token]) -> String {
        tokens.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}
