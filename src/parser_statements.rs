use std::fmt::{Display, Formatter, Write};
use crate::parser_expressions::{parse_expression, Expression};
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum StatementBody {
    Nop,
    Print(Expression),
    Expression(Expression),
    VariableDeclaration{name: String, value: Expression},
}
impl Display for StatementBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementBody::Nop => f.write_char(';'),
            StatementBody::Print(expr) => f.write_fmt(format_args!("print {expr};")),
            StatementBody::Expression(expr) => f.write_fmt(format_args!("{expr};")),
            StatementBody::VariableDeclaration{name, value} => f.write_fmt(format_args!("var {name} = {value};")),
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

pub(crate) fn parse_statement_list_from_string(str: &str) -> Option<Vec<Statement>> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let mut res = vec![];
    let mut tail = tokens.as_slice();
    while !tail.is_empty() {
        let (statement, tail2) = parse_statement(tail)?;
        tail = tail2;
        if !matches!(statement.body, StatementBody::Nop) {
            res.push(statement)
        }
    }
    Some(res)
}

#[cfg(test)]
pub(crate) fn parse_statement_from_string(str: &str) -> Option<Statement> {
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (statement, tail) = parse_statement(&tokens)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of statement");
    }
    Some(statement)
}

fn parse_statement(tail: &[Token]) -> Option<(Statement, &[Token])> {
    let orig_tail = tail;
    let Some((head, tail)) = tail.split_first() else {
        eprintln!("Unexpected end of token stream, expected start of a statement");
        return None;
    };
    let loc = head.loc;
    match head.kind {
        TokenKind::SEMICOLON => Some((Statement{ body: StatementBody::Nop, loc }, tail)),
        TokenKind::PRINT => {
            let (expr, tail) = parse_expression(tail, None)?;
            let tail = check_statement_terminated(tail, loc)?;
            Some((Statement { body: StatementBody::Print(expr), loc }, tail))
        },
        TokenKind::NUMBER | TokenKind::STRING
            | TokenKind::NIL | TokenKind::TRUE | TokenKind::FALSE
            | TokenKind::LEFT_PAREN | TokenKind::MINUS | TokenKind::BANG => {
            let (expr, tail) = parse_expression(orig_tail, None)?;
            let tail = check_statement_terminated(tail, loc)?;
            Some((Statement { body: StatementBody::Expression(expr), loc }, tail))
        },
        TokenKind::VAR => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, loc)?;
            let (_, tail) = expect_token_kind(tail, TokenKind::EQUAL, loc)?;
            let (expr, tail) = parse_expression(tail, None)?;
            let tail = check_statement_terminated(tail, loc)?;
            let body = StatementBody::VariableDeclaration {
                name: name.code.clone(), // todo: check if we can remove copying here
                value: expr,
            };
            Some((Statement { body, loc }, tail))
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
        eprintln!("Unexpected end of token stream, expected {expected:?} after statement that starts at {start_loc}");
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
        assert!(parse_statement_list_from_string(";;;;;").unwrap().is_empty());
        assert!(parse_statement_list_from_string("").unwrap().is_empty());
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
        assert!(parse_statement_from_string("x = y+1;").is_none());
        assert!(parse_statement_from_string("var = y+1;").is_none());
    }
}
