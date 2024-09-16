use std::fmt::{Display, Formatter};
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};

#[derive(Debug)]
pub(crate) enum ExpressionBody {
    Literal(Literal),
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Grouping(Box<Expression>),
}
#[derive(Debug)]
pub(crate) struct Expression {
    pub body: ExpressionBody,
    pub loc: Location,
}

impl Display for ExpressionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionBody::Literal(Literal::Number(n)) if n.fract() == 0.0 => f.write_fmt(format_args!("{n}.0")),
            ExpressionBody::Literal(lit) => f.write_fmt(format_args!("{lit}")),
            ExpressionBody::Grouping(inner) => f.write_fmt(format_args!("(group {inner})")),
            ExpressionBody::Unary(ex) => f.write_fmt(format_args!("({} {})", ex.op, ex.ex)),
            ExpressionBody::Binary(ex) => f.write_fmt(format_args!("({} {} {})", ex.op, ex.left, ex.right)),
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.body))
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => f.write_fmt(format_args!("{n}")),
            Literal::String(s) => f.write_str(s),
            Literal::Bool(b) => f.write_fmt(format_args!("{b}")),
            Literal::Nil => f.write_str("nil"),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum UnaryOperator {
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
    pub op: UnaryOperator,
    pub ex: Expression,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum BinaryOperator {
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
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
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

pub(crate) fn parse_expression<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    parse_binary_expression(tail, parent, 4)
}

fn parse_binary_expression<'a>(tail: &'a [Token], parent: Option<&'a Token>, parent_priority: u8) -> Option<(Expression, &'a [Token])> {
    if parent_priority == 0 {
        return parse_operand(tail, parent);
    }
    let current_priority = parent_priority - 1;
    let (mut left, mut tail) = parse_binary_expression(tail, parent, current_priority)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let (op, priority) = match next.kind {
            TokenKind::EQUAL_EQUAL => (BinaryOperator::Equal, 3),
            TokenKind::BANG_EQUAL => (BinaryOperator::NotEqual, 3),
            TokenKind::LESS => (BinaryOperator::Less, 2),
            TokenKind::LESS_EQUAL => (BinaryOperator::LessOrEqual, 2),
            TokenKind::GREATER => (BinaryOperator::Greater, 2),
            TokenKind::GREATER_EQUAL => (BinaryOperator::GreaterOrEqual, 2),
            TokenKind::PLUS => (BinaryOperator::Plus, 1),
            TokenKind::MINUS => (BinaryOperator::Minus, 1),
            TokenKind::STAR => (BinaryOperator::Multiply, 0),
            TokenKind::SLASH => (BinaryOperator::Divide, 0),
            _ => break,
        };
        if priority != current_priority {
            break;
        }
        tail = tail2;
        let (right, tail2) = parse_binary_expression(tail, Some(next), current_priority)?;
        tail = tail2;
        let loc = left.loc;
        let expr = BinaryExpression{ op, left, right };
        let expr = ExpressionBody::Binary(Box::new(expr));
        left = Expression{body: expr, loc };
    }
    Some((left, tail))
}

fn parse_operand<'a>(tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    let Some((token, tail)) = tail.split_first() else {
        match parent {
            None => eprintln!("empty input"),
            Some(parent) => eprintln!(
                "Unexpectedly reached the end of the token stream when parsing {:?} at {}",
                parent.kind,
                parent.loc,
            ),
        }
        return None;
    };

    let literal = match token.kind {
        TokenKind::NIL => Some(Literal::Nil),
        TokenKind::FALSE => Some(Literal::Bool(false)),
        TokenKind::TRUE => Some(Literal::Bool(true)),
        TokenKind::NUMBER => Some(Literal::Number(
            token.literal.as_ref()
                .expect("got a literal token without literal value")
                .parse()
                .expect("got an invalid numeric literal")
        )),
        TokenKind::STRING => Some(Literal::String(token.literal.clone().expect("got a literal token without literal value"))), // todo: check if we can remove copying her)e
        _ => None,
    };
    if let Some(literal) = literal {
        let body = ExpressionBody::Literal(literal);
        return Some((Expression{ body, loc: token.loc }, tail));
    }

    if token.kind == TokenKind::LEFT_PAREN {
        let (inner, tail) = parse_expression(tail, Some(token))?;
        let Some((next, tail)) = tail.split_first() else {
            eprintln!("Parenthesis that was opened at {} is never closed", token.loc);
            return None;
        };
        if next.kind != TokenKind::RIGHT_PAREN {
            eprintln!(
                "Expected closing parenthesis at {}, got {:?} instead. Parenthesis was opened at {}",
                next.loc,
                next.kind,
                token.loc,
            );
            return None;
        }
        let body = ExpressionBody::Grouping(Box::new(inner));
        return Some((Expression{body, loc: token.loc}, tail));
    }

    let unary_op = match token.kind {
        TokenKind::MINUS => Some(UnaryOperator::Minus),
        TokenKind::BANG => Some(UnaryOperator::Not),
        _ => None,
    };
    if let Some(op) = unary_op {
        let (inner, tail) = parse_operand(tail, Some(token))?;
        let expr = UnaryExpression{ op, ex: inner };
        let body = ExpressionBody::Unary(Box::new(expr));
        return Some((Expression{ body, loc: token.loc }, tail));
    }

    eprintln!("Unexpected token kind {:?} at {}", token.kind, token.loc);
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

    #[test]
    fn test_parse_priority() {
        assert_eq!("(- (+ 1.0 (/ (* 2.0 3.0) 4.0)) 5.0)", parse_expression_from_string("1 + 2 * 3 / 4 - 5").unwrap().to_string());
        assert_eq!("(== (> (+ 3.0 2.0) 5.0) (>= 3.0 (+ 2.0 2.0)))", parse_expression_from_string("3 + 2 > 5 == 3 >= 2 + 2").unwrap().to_string());
    }
}
