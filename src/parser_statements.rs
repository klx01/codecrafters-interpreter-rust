use std::fmt::{Display, Formatter};
use crate::parser_expressions::{parse_expression, Expression, ExpressionBody, Literal};
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum StatementBody {
    Print(Expression),
    Expression(Expression),
    VariableDeclaration{name: String, value: Expression},
    Scope(Box<Scope>)
}
impl Display for StatementBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementBody::Print(expr) => f.write_fmt(format_args!("print {expr};")),
            StatementBody::Expression(expr) => f.write_fmt(format_args!("{expr};")),
            StatementBody::VariableDeclaration{name, value} => f.write_fmt(format_args!("var {name} = {value};")),
            StatementBody::Scope(scope) => f.write_fmt(format_args!("{scope}")),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Statement {
    pub body: StatementBody,
    pub loc: Location,
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.body))
    }
}

enum ParseResult {
    Statement(Statement),
    Nop,
    ExitScope,
}

#[derive(Debug)]
pub(crate) struct Scope {
    pub statements: Vec<Statement>,
    pub start: Location,
    pub end: Location,
}
impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        for stmt in &self.statements {
            f.write_fmt(format_args!("{stmt} "))?;
        }
        f.write_str("}")
    }
}

pub(crate) fn parse_statement_list_from_string(str: &str) -> Option<Scope> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (scope, tail) = parse_scope(&tokens, true)?;
    assert!(tail.is_empty(), "tail for top scope should always be empty");
    if !tail.is_empty() {
        eprintln!("Unexpected end of scope at {}", scope.end);
        return None;
    }
    Some(scope)
}

#[cfg(test)]
pub(crate) fn parse_statement_from_string(str: &str) -> Option<Statement> {
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (parse_result, tail) = parse_statement(&tokens)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of statement");
    }
    match parse_result {
        ParseResult::Statement(x) => Some(x),
        ParseResult::Nop => None,
        ParseResult::ExitScope => None,
    }
}

pub(crate) fn parse_scope(mut tail: &[Token], is_top_scope: bool) -> Option<(Scope, &[Token])> {
    let start_loc = if let Some(first) = tail.first() {
        first.loc
    } else {
        Location { row: 0, col: 0 }
    };
    let mut end_loc = start_loc;
    let mut statements = vec![];
    let mut is_closed = false;

    while !tail.is_empty() {
        end_loc = tail.first().unwrap().loc;
        let (parse_result, tail2) = parse_statement(tail)?;
        tail = tail2;
        match parse_result {
            ParseResult::Statement(stmt) => statements.push(stmt),
            ParseResult::Nop => {}
            ParseResult::ExitScope => {
                is_closed = true;
                break;
            } 
        }
    }
    if is_top_scope && is_closed {
        eprintln!("Unexpected end of scope at {end_loc}");
        return None;
    } else if !is_top_scope && !is_closed {
        eprintln!("Scope that starts at {start_loc} is not closed");
        return None;
    }
    let scope = Scope{ statements, start: start_loc, end: end_loc };
    Some((scope, tail))
}

fn parse_statement(tail: &[Token]) -> Option<(ParseResult, &[Token])> {
    let orig_tail = tail;
    let Some((head, tail)) = tail.split_first() else {
        eprintln!("Unexpected end of token stream, expected start of a statement");
        return None;
    };
    let loc = head.loc;
    match head.kind {
        TokenKind::SEMICOLON => Some((ParseResult::Nop, tail)),
        TokenKind::PRINT => {
            let (expr, tail) = parse_expression(tail, None)?;
            let tail = check_statement_terminated(tail, loc)?;
            let stmt = Statement { body: StatementBody::Print(expr), loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::NUMBER | TokenKind::STRING | TokenKind::IDENTIFIER
            | TokenKind::NIL | TokenKind::TRUE | TokenKind::FALSE
            | TokenKind::LEFT_PAREN | TokenKind::MINUS | TokenKind::BANG => {
            let (expr, tail) = parse_expression(orig_tail, None)?;
            let tail = check_statement_terminated(tail, loc)?;
            let stmt = Statement { body: StatementBody::Expression(expr), loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::VAR => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, loc)?;
            let Some((next, tail)) = tail.split_first() else {
                eprintln!("Unexpected end of token stream, expected = or ; in variable declaration after {}", name.loc);
                return None;
            };
            let (expr, tail) = match next.kind {
                TokenKind::EQUAL => {
                    let (expr, tail) = parse_expression(tail, None)?;
                    let tail = check_statement_terminated(tail, loc)?;
                    (expr, tail)
                }
                TokenKind::SEMICOLON => {
                    let expr = Expression{
                        body: ExpressionBody::Literal(Literal::Nil),
                        loc: next.loc,
                    };
                    (expr, tail)
                }
                _ => {
                    eprintln!("Expected = or ;, found {next} at {}", next.loc);
                    return None;
                },
            };

            let body = StatementBody::VariableDeclaration {
                name: name.code.clone(), // todo: check if we can remove copying here
                value: expr,
            };
            let stmt = Statement { body, loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::LEFT_BRACE => {
            let (scope, tail) = parse_scope(tail, false)?;
            if scope.statements.is_empty() {
                return Some((ParseResult::Nop, tail));
            }
            let body = StatementBody::Scope(Box::new(scope));
            let stmt = Statement { body, loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::RIGHT_BRACE => {
            Some((ParseResult::ExitScope, tail))
        },
        _ => {
            eprintln!("Unexpected token {head} at {loc}, expected a start of a statement");
            None
        },
    }
}

fn check_statement_terminated(tail: &[Token], start_loc: Location) -> Option<&[Token]> {
    let (_, tail) = expect_token_kind(tail, TokenKind::SEMICOLON, start_loc)?;
    Some(tail)
}

fn expect_token_kind(tail: &[Token], expected: TokenKind, start_loc: Location) -> Option<(&Token, &[Token])> {
    let Some((head, tail)) = tail.split_first() else {
        eprintln!("Unexpected end of token stream, expected {expected:?} in a statement that starts at {start_loc}");
        return None;
    };
    if head.kind == expected {
        Some((head, tail))
    } else {
        eprintln!("Expected {expected:?}, found {head} at {}", head.loc);
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_nops() {
        assert!(parse_statement_list_from_string(";;;;;").unwrap().statements.is_empty());
        assert!(parse_statement_list_from_string("").unwrap().statements.is_empty());
    }

    #[test]
    fn test_print() {
        assert_eq!("print (+ 1.0 1.0);", parse_statement_from_string("print 1+1;").unwrap().to_string());
        assert!(parse_statement_from_string("print 1+1").is_none());
        assert!(parse_statement_from_string("print 1+;").is_none());
        assert!(parse_statement_from_string("print (;").is_none());
        assert!(parse_statement_from_string("print ;").is_none());
    }

    #[test]
    fn test_var() {
        assert_eq!("var x = (+ var(y) 1.0);", parse_statement_from_string("var x = y+1;").unwrap().to_string());
        assert!(parse_statement_from_string("var x = y+1").is_none());
        assert!(parse_statement_from_string("var x = y+;").is_none());
        assert!(parse_statement_from_string("var x = (;").is_none());
        assert!(parse_statement_from_string("var x = ;").is_none());
        assert!(parse_statement_from_string("var x y+1;").is_none());
        assert_eq!("(= var(x) (+ var(y) 1.0));", parse_statement_from_string("x = y+1;").unwrap().to_string());
        assert!(parse_statement_from_string("var = y+1;").is_none());
        assert_eq!("var x = nil;", parse_statement_from_string("var x;").unwrap().to_string());
    }

    #[test]
    fn test_scopes() {
        let statements = "{var a = 1;}";
        let expected = "{ { var a = 1.0; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
        assert!(parse_statement_list_from_string("{var a = 1;}}").is_none());
        assert!(parse_statement_list_from_string("{var a = 1;").is_none());
        
        let statements = "var a = 1; {var a = a + 2; print a;} print a;";
        let expected = "{ var a = 1.0; { var a = (+ var(a) 2.0); print var(a); } print var(a); }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
    }
}
