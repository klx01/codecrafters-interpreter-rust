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
            Expression::Grouping(inner) => f.write_fmt(format_args!("(group {inner})")),
            Expression::Unary(ex) => f.write_fmt(format_args!("({} {})", ex.op, ex.ex)),
            Expression::Binary(ex) => f.write_fmt(format_args!("({} {} {})", ex.op, ex.left, ex.right)),
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
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Minus => f.write_str("-"),
            UnaryOperator::Not => f.write_str("!"),
        }
    }
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
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Equal => f.write_str("=="),
            BinaryOperator::NotEqual => f.write_str("!="),
            BinaryOperator::Less => f.write_str("<"),
            BinaryOperator::LessOrEqual => f.write_str("<="),
            BinaryOperator::Greater => f.write_str(">"),
            BinaryOperator::GreaterOrEqual => f.write_str(">="),
            BinaryOperator::Plus => f.write_str("+"),
            BinaryOperator::Minus => f.write_str("-"),
            BinaryOperator::Multiply => f.write_str("*"),
            BinaryOperator::Divide => f.write_str("/"),
        }
    }
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
    let (expr, tail) = parse_expression(&tokens, None)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of expression");
    }
    Some(expr)
}

fn parse_expression<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    parse_equality(tail, parent)
}

fn parse_equality<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let (mut left, mut tail) = parse_comparison(tail, parent)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let op = match next.kind {
            TokenKind::EQUAL_EQUAL => BinaryOperator::Equal,
            TokenKind::BANG_EQUAL => BinaryOperator::NotEqual,
            _ => break,
        };
        tail = tail2;
        let (right, tail2) = parse_comparison(tail, Some(next))?;
        tail = tail2;
        let expr = BinaryExpression{ op, left, right };
        left = Expression::Binary(Box::new(expr));
    }
    Some((left, tail))
}

fn parse_comparison<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let (mut left, mut tail) = parse_term(tail, parent)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let op = match next.kind {
            TokenKind::LESS => BinaryOperator::Less,
            TokenKind::LESS_EQUAL => BinaryOperator::LessOrEqual,
            TokenKind::GREATER => BinaryOperator::Greater,
            TokenKind::GREATER_EQUAL => BinaryOperator::GreaterOrEqual,
            _ => break,
        };
        tail = tail2;
        let (right, tail2) = parse_term(tail, Some(next))?;
        tail = tail2;
        let expr = BinaryExpression{ op, left, right };
        left = Expression::Binary(Box::new(expr));
    }
    Some((left, tail))
}

fn parse_term<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let (mut left, mut tail) = parse_factor(tail, parent)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let op = match next.kind {
            TokenKind::PLUS => BinaryOperator::Plus,
            TokenKind::MINUS => BinaryOperator::Minus,
            _ => break,
        };
        tail = tail2;
        let (right, tail2) = parse_factor(tail, Some(next))?;
        tail = tail2;
        let expr = BinaryExpression{ op, left, right };
        left = Expression::Binary(Box::new(expr));
    }
    Some((left, tail))
}

fn parse_factor<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let (mut left, mut tail) = parse_operand(tail, parent)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let op = match next.kind {
            TokenKind::STAR => BinaryOperator::Multiply,
            TokenKind::SLASH => BinaryOperator::Divide,
            _ => break,
        };
        tail = tail2;
        let (right, tail2) = parse_operand(tail, Some(next))?;
        tail = tail2;
        let expr = BinaryExpression{ op, left, right };
        left = Expression::Binary(Box::new(expr));
    }
    Some((left, tail))
}

fn parse_operand<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let Some((token, tail)) = tail.split_first() else {
        match parent {
            None => eprintln!("empty input"),
            Some(parent) => eprintln!(
                "Unexpectedly reached the end of the token stream when parsing {:?} at {}:{}",
                parent.kind,
                parent.row,
                parent.col,
            ),
        }
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

    if token.kind == TokenKind::LEFT_PAREN {
        let (inner, tail) = parse_expression(tail, Some(token))?;
        let Some((next, tail)) = tail.split_first() else {
            eprintln!("Parenthesis that was opened at {}:{} is never closed", token.row, token.col);
            return None;
        };
        if next.kind != TokenKind::RIGHT_PAREN {
            eprintln!(
                "Expected closing parenthesis at {}:{}, got {:?} instead. Parenthesis was opened at {}:{}",
                next.row,
                next.col,
                next.kind,
                token.row,
                token.col,
            );
            return None;
        }
        return Some((Expression::Grouping(Box::new(inner)), tail));
    }

    let unary_op = match token.kind {
        TokenKind::MINUS => Some(UnaryOperator::Minus),
        TokenKind::BANG => Some(UnaryOperator::Not),
        _ => None,
    };
    if let Some(op) = unary_op {
        let (inner, tail) = parse_operand(tail, Some(token))?;
        let expr = UnaryExpression{ op, ex: inner };
        return Some((Expression::Unary(Box::new(expr)), tail));
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

    #[test]
    fn test_parse_group() {
        assert_eq!("(group foo)", parse_expression_from_string("(\"foo\")").unwrap().to_string());
    }

    #[test]
    fn test_parse_unary() {
        assert_eq!("(! true)", parse_expression_from_string("!true").unwrap().to_string());
        assert_eq!("(- 1.2)", parse_expression_from_string("-1.2").unwrap().to_string());
    }

    #[test]
    fn test_parse_binary() {
        assert_eq!("(/ (* 16.0 38.0) 58.0)", parse_expression_from_string("16 * 38 / 58").unwrap().to_string());
        assert_eq!("(- (+ 52.0 80.0) 94.0)", parse_expression_from_string("52 + 80 - 94").unwrap().to_string());
        assert_eq!("(< (< 83.0 99.0) 115.0)", parse_expression_from_string("83 < 99 < 115").unwrap().to_string());
        assert_eq!("(== baz baz)", parse_expression_from_string("\"baz\" == \"baz\"").unwrap().to_string());
    }
}
