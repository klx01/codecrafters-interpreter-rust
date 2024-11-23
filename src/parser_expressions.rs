use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};
use crate::value::Value;

#[derive(Debug)]
pub(crate) enum ExpressionBody<'a> {
    Literal(Value<'a>),
    Unary(Box<UnaryExpression<'a>>),
    Binary(Box<BinaryExpression<'a>>),
    Grouping(Box<Expression<'a>>),
    Variable(String),
    Assignment(Box<AssignmentExpression<'a>>),
    Call{expr: Box<Expression<'a>>, args: Vec<Expression<'a>>},
}
#[derive(Debug)]
pub(crate) struct Expression<'a> {
    pub body: ExpressionBody<'a>,
    pub start: Location,
    pub end: Location,
}

impl<'a> Display for ExpressionBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionBody::Literal(Value::Number(n)) if n.fract() == 0.0 => f.write_fmt(format_args!("{n}.0")),
            ExpressionBody::Literal(lit) => f.write_fmt(format_args!("{lit}")),
            ExpressionBody::Grouping(inner) => f.write_fmt(format_args!("(group {inner})")),
            ExpressionBody::Unary(ex) => f.write_fmt(format_args!("({} {})", ex.op, ex.ex)),
            ExpressionBody::Binary(ex) => f.write_fmt(format_args!("({} {} {})", ex.op, ex.left, ex.right)),
            ExpressionBody::Variable(name) => f.write_fmt(format_args!("var({name})")),
            ExpressionBody::Assignment(ass) => f.write_fmt(format_args!("(= var({}) {})", ass.var, ass.expr)),
            ExpressionBody::Call{ expr, args } => {
                f.write_fmt(format_args!("(call {expr}"))?;
                for arg in args {
                    f.write_fmt(format_args!(" {arg}"))?;
                }
                f.write_char(')')
            },
        }
    }
}
impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.body))
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
pub(crate) struct UnaryExpression<'a> {
    pub op: UnaryOperator,
    pub ex: Expression<'a>,
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
    And,
    Or,
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
            BinaryOperator::And => f.write_str("and"),
            BinaryOperator::Or => f.write_str("or"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct BinaryExpression<'a> {
    pub op: BinaryOperator,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug)]
pub(crate) struct AssignmentExpression<'a> {
    pub var: String,
    pub expr: Expression<'a>,
}

pub(crate) fn parse_expression_from_string(str: &str) -> Option<String> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (expr, tail) = parse_expression(&tokens, None)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of expression");
    }
    Some(expr.to_string())
}

pub(crate) fn parse_expression(tail: &[Token], prev_end: Option<Location>) -> Option<(Expression, &[Token])> {
    parse_assignment(tail, prev_end)
}

fn parse_assignment(mut tail: &[Token], prev_end: Option<Location>) -> Option<(Expression, &[Token])> {
    if let Some((left, tail2)) = parse_operand(tail, prev_end) {
        match left.body {
            ExpressionBody::Variable(var) => {
                if let Some((next, tail2)) = tail2.split_first() {
                    if next.kind == TokenKind::EQUAL {
                        tail = tail2;
                        let (right, tail2) = parse_assignment(tail, Some(next.end))?;
                        tail = tail2;
                        let start = left.start;
                        let end = right.end;
                        let expr = AssignmentExpression{ var, expr: right };
                        let expr = ExpressionBody::Assignment(Box::new(expr));
                        let expr = Expression{ body: expr, start, end };
                        return Some((expr, tail));
                    }
                };
            }
            _ => {},
        }
    };
    parse_binary_expression(tail, prev_end, 6)
}

fn parse_binary_expression(tail: &[Token], prev_end: Option<Location>, parent_priority: u8) -> Option<(Expression, &[Token])> {
    if parent_priority == 0 {
        return parse_operand(tail, prev_end);
    }
    let current_priority = parent_priority - 1;
    let (mut left, mut tail) = parse_binary_expression(tail, prev_end, current_priority)?;
    loop {
        let Some((next, tail2)) = tail.split_first() else {
            break;
        };
        let (op, priority) = match next.kind {
            TokenKind::OR => (BinaryOperator::Or, 5),
            TokenKind::AND => (BinaryOperator::And, 4),
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
        let (right, tail2) = parse_binary_expression(tail, Some(next.end), current_priority)?;
        tail = tail2;
        let start = left.start;
        let end = right.end;
        let expr = BinaryExpression{ op, left, right };
        let expr = ExpressionBody::Binary(Box::new(expr));
        left = Expression{body: expr, start, end };
    }
    Some((left, tail))
}

fn parse_operand(tail: &[Token], prev_end: Option<Location>) -> Option<(Expression, &[Token])> {
    let (mut expr, mut tail) = parse_operand_inner(tail, prev_end)?;
    loop {
        let (paren, tail2) = check_token_kind(tail, TokenKind::LEFT_PAREN);
        tail = tail2;
        if let Some(paren) = paren {
            let (args, tail2, last_token) = parse_call_args(tail, paren.end)?;
            tail = tail2;
            let start = expr.start;
            expr = Expression{ body: ExpressionBody::Call{ expr: Box::new(expr), args }, start, end: last_token.end };
        } else {
            break;
        }
    }
    Some((expr, tail))
}

fn parse_operand_inner(tail: &[Token], prev_end: Option<Location>) -> Option<(Expression, &[Token])> {
    let Some((token, tail)) = tail.split_first() else {
        match prev_end {
            None => eprintln!("empty input"),
            Some(loc) => eprintln!("Unexpectedly reached the end of the token stream at {loc}, expected expression operand"),
        }
        return None;
    };

    match token.kind {
        TokenKind::IDENTIFIER => {
            let name = token.code.clone(); // todo: check if we can remove copying here
            let expr = Expression{ body: ExpressionBody::Variable(name), start: token.start, end: token.end };
            return Some((expr, tail));
        }
        _ => {},
    };

    let literal = match token.kind {
        TokenKind::NIL => Some(Value::Nil),
        TokenKind::FALSE => Some(Value::Bool(false)),
        TokenKind::TRUE => Some(Value::Bool(true)),
        TokenKind::NUMBER => Some(Value::Number(
            token.literal.as_ref()
                .expect("got a literal token without literal value")
                .parse()
                .expect("got an invalid numeric literal")
        )),
        TokenKind::STRING => {
            // todo: check if we can remove copying here
            let string = token.literal.clone().expect("got a literal token without literal value");
            Some(Value::String(Rc::new(string)))
        },
        _ => None,
    };
    if let Some(literal) = literal {
        let body = ExpressionBody::Literal(literal);
        return Some((Expression{ body, start: token.start, end: token.end }, tail));
    }

    if token.kind == TokenKind::LEFT_PAREN {
        let (inner, tail) = parse_expression(tail, Some(token.end))?;
        let (close, tail) = expect_token_kind(tail, TokenKind::RIGHT_PAREN, inner.end)?;
        let body = ExpressionBody::Grouping(Box::new(inner));
        return Some((Expression{body, start: token.start, end: close.end}, tail));
    }

    let unary_op = match token.kind {
        TokenKind::MINUS => Some(UnaryOperator::Minus),
        TokenKind::BANG => Some(UnaryOperator::Not),
        _ => None,
    };
    if let Some(op) = unary_op {
        let (inner, tail) = parse_operand(tail, Some(token.end))?;
        let end = inner.end;
        let expr = UnaryExpression{ op, ex: inner };
        let body = ExpressionBody::Unary(Box::new(expr));
        return Some((Expression{ body, start: token.start, end }, tail));
    }

    eprintln!("Unexpected token kind {:?} at {}", token.kind, token.start);
    None
}

fn parse_call_args(mut tail: &[Token], mut prev_end: Location) -> Option<(Vec<Expression>, &[Token], &Token)> {
    let mut args = vec![];
    loop {
        let (paren, tail2) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
        tail = tail2;
        if let Some(paren) = paren {
            return Some((args, tail, paren));
        }
        let (expression, tail2) = parse_expression(tail, Some(prev_end))?;
        tail = tail2;
        let Some((next, tail2)) = tail.split_first() else {
            eprintln!("Unexpected end of token stream, expected ) or , at {}", expression.end);
            return None;
        };
        args.push(expression);
        tail = tail2;
        match next.kind {
            TokenKind::COMMA => prev_end = next.end,
            TokenKind::RIGHT_PAREN => return Some((args, tail, next)),
            _ => {
                eprintln!("Expected ) or , but found {next} at {}", next.start);
                return None;
            },
        }
    }
}

pub(crate) fn expect_token_kind(tail: &[Token], expected: TokenKind, prev_end: Location) -> Option<(&Token, &[Token])> {
    let Some((head, tail)) = tail.split_first() else {
        eprintln!("Unexpected end of token stream, expected {expected:?} at {prev_end}");
        return None;
    };
    if head.kind == expected {
        Some((head, tail))
    } else {
        eprintln!("Expected {expected:?}, found {head} at {}", head.start);
        None
    }
}

pub(crate) fn check_token_kind(tail_orig: &[Token], expected: TokenKind) -> (Option<&Token>, &[Token]) {
    let Some((head, tail)) = tail_orig.split_first() else {
        return (None, tail_orig);
    };
    if head.kind == expected {
        (Some(head), tail)
    } else {
        (None, tail_orig)
    }
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
        assert_eq!("var(asdf)", parse_expression_from_string("asdf").unwrap().to_string());
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
        assert_eq!("(or true false)", parse_expression_from_string("true or false").unwrap().to_string());
        assert_eq!("(and true false)", parse_expression_from_string("true and false").unwrap().to_string());
    }

    #[test]
    fn test_parse_priority() {
        assert_eq!("(- (+ 1.0 (/ (* 2.0 3.0) 4.0)) 5.0)", parse_expression_from_string("1 + 2 * 3 / 4 - 5").unwrap().to_string());
        assert_eq!("(== (> (+ 3.0 2.0) 5.0) (>= 3.0 (+ 2.0 2.0)))", parse_expression_from_string("3 + 2 > 5 == 3 >= 2 + 2").unwrap().to_string());

        assert_eq!("(+ (+ (+ (+ var(a) var(b)) var(c)) 1.0) 2.0)", parse_expression_from_string("a + b + c + 1 + 2").unwrap().to_string());
        assert_eq!("(= var(a) (= var(b) (= var(c) (+ 1.0 2.0))))", parse_expression_from_string("a = b = c = 1 + 2").unwrap().to_string());
        assert_eq!("1.0", parse_expression_from_string("1 = a").unwrap().to_string());
        assert_eq!("(+ 1.0 var(a))", parse_expression_from_string("1 + a = 1").unwrap().to_string());

        assert_eq!("(or (== var(a) var(b)) (and (== var(a) var(c)) (== var(z) var(x))))", parse_expression_from_string("a == b or a == c and z == x").unwrap().to_string());
    }

    #[test]
    fn test_parse_call() {
        assert_eq!("(call var(foo))", parse_expression_from_string("foo()").unwrap().to_string());
        assert_eq!("(call 100.0)", parse_expression_from_string("100()").unwrap().to_string());
        assert_eq!("(call (call (call var(foo))))", parse_expression_from_string("foo()()()").unwrap().to_string());
        assert_eq!("(call var(foo) 1.0 2.0 3.0)", parse_expression_from_string("foo(1, 2, 3)").unwrap().to_string());
        assert_eq!("(call var(foo) 1.0 2.0 3.0)", parse_expression_from_string("foo(1, 2, 3,)").unwrap().to_string());
        assert_eq!("(call var(foo) (call var(bar)) (= var(a) (+ 1.0 2.0)))", parse_expression_from_string("foo(bar(), a = 1 + 2)").unwrap().to_string());
        assert!(parse_expression_from_string("foo(").is_none());
        assert!(parse_expression_from_string("foo(1").is_none());
        assert!(parse_expression_from_string("foo(1,").is_none());
        assert!(parse_expression_from_string("foo(1,]").is_none());
    }
}
