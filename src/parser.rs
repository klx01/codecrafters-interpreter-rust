use std::fmt::{Display, Formatter};
use crate::tokenizer::{tokenize_string_no_eof, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Grouping(Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(lit) => match lit {
                Literal::Number(n) => f.write_str(n),
                Literal::String(s) => f.write_str(s),
                Literal::Bool(b) => f.write_fmt(format_args!("{b}")),
                Literal::Nil => f.write_str("nil"),
            }
            _ => panic!("display is not implemented yet for this type of expression {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Number(String),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum UnaryOperator {
    Minus,
    Not,
}

#[derive(Debug)]
pub(crate) struct UnaryExpression {
    op: UnaryOperator,
    ex: Expression,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum BinaryOperator {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub(crate) struct BinaryExpression {
    op: BinaryOperator,
    left: Expression,
    right: Expression,
}

pub(crate) fn parse_expression_from_string(str: &str) -> Option<Expression> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (expr, tail) = parse_expression_from_tokens(&tokens)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of expression");
    }
    Some(expr)
}

fn parse_expression_from_tokens(tail: &[Token]) -> Option<(Expression, &[Token])> {
    let Some((token, tail)) = tail.split_first() else {
        eprintln!("Unexpectedly reached the end of token stream");
        return None;
    };

    let literal = match token.kind {
        TokenKind::NIL => Some(Literal::Nil),
        TokenKind::FALSE => Some(Literal::Bool(false)),
        TokenKind::TRUE => Some(Literal::Bool(true)),
        TokenKind::NUMBER => Some(Literal::Number(token.literal.clone().expect("got a literal token without literal value"))), // todo: check if we can remove copying her)e
        TokenKind::STRING => Some(Literal::String(token.literal.clone().expect("got a literal token without literal value"))),
        _ => None,
    };
    if let Some(literal) = literal {
        return Some((Expression::Literal(literal), tail));
    }
    eprintln!("Unexpected token kind {:?} at {}:{}", token.kind, token.row, token.col);
    None
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_literals() {
        for str in ["true", "false", "nil", "42.3"] {
            assert_eq!(str, parse_expression_from_string(str).unwrap().to_string(), "value {str}");
        }
        assert_eq!("asdf", parse_expression_from_string("\"asdf\"").unwrap().to_string());
    }
}
