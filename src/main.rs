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
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    STRING,
    NUMBER,
    IDENTIFIER,
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
    let len = chars.len();
    while index < len {
        let char = chars[index];
        index += 1;

        if char == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }

        if char.is_whitespace() {
            continue;
        }

        if let Some(&next) = chars.get(index) {
            let matched_kind = match (char, next) {
                ('=', '=') => Some(TokenKind::EQUAL_EQUAL),
                ('!', '=') => Some(TokenKind::BANG_EQUAL),
                ('<', '=') => Some(TokenKind::LESS_EQUAL),
                ('>', '=') => Some(TokenKind::GREATER_EQUAL),
                _ => None,
            };
            if let Some(kind) = matched_kind {
                index += 1;
                col += 1;
                let token = Token{ kind, code: format!("{char}{next}"), literal: None, row, col };
                tokens.push(token);
                continue;
            }
            if (char, next) == ('/', '/') {
                index = index + 1;
                col += 1;
                while (index < len) && (chars[index] != '\n') {
                    index += 1;
                    col += 1; // need to keep track of col in case if we hit EOF while in comment, EOF token should have correct position
                }
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
            '!' => Some(TokenKind::BANG),
            '<' => Some(TokenKind::LESS),
            '>' => Some(TokenKind::GREATER),
            _ => None,
        };
        if let Some(kind) = char_token_kind {
            let token = Token{ kind, code: char.to_string(), literal: None, row, col };
            tokens.push(token);
            continue;
        }

        if char.is_ascii_digit() {
            let mut integer = String::from(char);
            while (index < len) && chars[index].is_ascii_digit() {
                integer.push(chars[index]);
                index += 1;
                col += 1;
            }
            let mut fraction = String::new();
            if (index < len) && (chars[index] == '.') {
                index += 1;
                col += 1;
                while (index < len) && chars[index].is_ascii_digit() {
                    fraction.push(chars[index]);
                    index += 1;
                    col += 1;
                }
                if fraction.is_empty() {
                    index -= 1;
                    col -= 1;
                }
            }

            let fraction_trimmed = fraction.trim_end_matches('0');
            let token = Token{
                kind: TokenKind::NUMBER,
                code: if fraction.is_empty() { integer.clone() } else { format!("{integer}.{fraction}") },
                literal: Some(format!("{integer}.{}", if fraction_trimmed.is_empty() { "0" } else { fraction_trimmed })),
                row,
                col,
            };
            tokens.push(token);
            continue;
        }

        if char == '"' {
            // todo: this does not handle escaped quotes \"
            let mut literal = String::new();
            while (index < len) && (chars[index] != '"') {
                literal.push(chars[index]);
                index += 1;
                col += 1;
            }
            if index == len {
                eprintln!("[line {row}] Error: Unterminated string.");
                has_errors = true;
            } else {
                let close_char = chars[index];
                index += 1;
                col += 1;
                let token = Token{
                    kind: TokenKind::STRING,
                    code: format!("{char}{literal}{close_char}"),
                    literal: Some(literal),
                    row,
                    col,
                };
                tokens.push(token);
            }
            continue;
        }

        if is_identifier_char(char) {
            let mut code = String::from(char);
            while (index < len) && is_identifier_char(chars[index]) {
                code.push(chars[index]);
                index += 1;
                col += 1;
            }
            let token = Token{
                kind: TokenKind::IDENTIFIER,
                code,
                literal: None,
                row,
                col,
            };
            tokens.push(token);
            continue;
        }

        eprintln!("[line {row}] Error: Unexpected character: {char}");
        has_errors = true;
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

fn is_identifier_char(char: char) -> bool {
    char.is_ascii_alphanumeric() || (char == '_')
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

    #[test]
    fn test_tokenize_neg() {
        let str = "!!===";
        let expected_tokens = "BANG ! null
BANG_EQUAL != null
EQUAL_EQUAL == null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_relation() {
        let str = "<<=>>=";
        let expected_tokens = "LESS < null
LESS_EQUAL <= null
GREATER > null
GREATER_EQUAL >= null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_comment() {
        let str = "()// Comment";
        let expected_tokens = "LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_slash() {
        let str = "/()";
        let expected_tokens = "SLASH / null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_spaces() {
        let str = "(
 )";
        let expected_tokens = "LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_string_valid() {
        let str = "\"foo baz\"";
        let expected_tokens = "STRING \"foo baz\" foo baz
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_string_invalid() {
        let str = "\"bar";
        let expected_tokens = "EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(has_errors);
    }

    #[test]
    fn test_tokenize_number_no_dot() {
        let str = "42";
        let expected_tokens = "NUMBER 42 42.0
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_number_with_dot() {
        let str = "1234.1234.";
        let expected_tokens = "NUMBER 1234.1234 1234.1234
DOT . null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_number_edge_cases() {
        let str = "200.00 .456 123. ";
        let expected_tokens = "NUMBER 200.00 200.0
DOT . null
NUMBER 456 456.0
NUMBER 123 123.0
DOT . null
EOF  null";
        let (tokens, has_errors) = tokenize_string(str);
        assert_eq!(expected_tokens, tokens_as_string(&tokens));
        assert!(!has_errors);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let str = "foo bar _hello";
        let expected_tokens = "IDENTIFIER foo null
IDENTIFIER bar null
IDENTIFIER _hello null
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
