use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};
use crate::value::Value;

#[derive(Debug)]
pub(crate) enum ExpressionBody {
    Literal(Value),
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
    Grouping(Box<Expression>),
    Variable(String),
    Assignment(Box<AssignmentExpression>),
    Call{expr: Box<Expression>, args: Vec<Expression>},
}
#[derive(Debug)]
pub(crate) struct Expression {
    pub body: ExpressionBody,
    pub loc: Location,
}

impl Display for ExpressionBody {
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
impl Display for Expression {
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
pub(crate) struct BinaryExpression {
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug)]
pub(crate) struct AssignmentExpression {
    pub var: String,
    pub expr: Expression,
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
    parse_assignment(tail, parent)
}

fn parse_assignment<'a>(mut tail: &'a [Token], parent: Option<&'a Token>) -> Option<(Expression, &'a [Token])> {
    if let Some((left, tail2)) = parse_operand(tail, parent) {
        match left.body {
            ExpressionBody::Variable(var) => {
                if let Some((next, tail2)) = tail2.split_first() {
                    if next.kind == TokenKind::EQUAL {
                        tail = tail2;
                        let (right, tail2) = parse_assignment(tail, Some(next))?;
                        tail = tail2;
                        let loc = left.loc;
                        let expr = AssignmentExpression{ var, expr: right };
                        let expr = ExpressionBody::Assignment(Box::new(expr));
                        let expr = Expression{ body: expr, loc };
                        return Some((expr, tail));
                    }
                };
            }
            _ => {},
        }
    };
    parse_binary_expression(tail, parent, 6)
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

    match token.kind {
        TokenKind::IDENTIFIER => {
            let name = token.code.clone(); // todo: check if we can remove copying here
            let mut expr = Expression{ body: ExpressionBody::Variable(name), loc: token.loc };
            let mut tail = tail;
            loop {
                let (paren, tail2) = check_token_kind(tail, TokenKind::LEFT_PAREN);
                tail = tail2;
                if paren.is_some() {
                    let (args, tail2) = parse_call_args(tail, token)?;
                    tail = tail2;
                    expr = Expression{ body: ExpressionBody::Call{ expr: Box::new(expr), args }, loc: token.loc }; // todo: fix location
                } else {
                    break;
                }
            }
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
        return Some((Expression{ body, loc: token.loc }, tail));
    }

    if token.kind == TokenKind::LEFT_PAREN {
        let (inner, tail) = parse_expression(tail, Some(token))?;
        let (_, tail) = expect_token_kind(tail, TokenKind::RIGHT_PAREN, token.loc)?;
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

fn parse_call_args<'a>(mut tail: &'a [Token], parent: &'a Token) -> Option<(Vec<Expression>, &'a [Token])> {
    let mut args = vec![];
    loop {
        let (paren, tail2) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
        tail = tail2;
        if paren.is_some() {
            return Some((args, tail));
        }
        let (expression, tail2) = parse_expression(tail, Some(parent))?; // todo: fix parent for parse expression
        tail = tail2;
        args.push(expression);
        let Some((next, tail2)) = tail.split_first() else {
            eprintln!("Unexpected end of token stream, expected ) or , in arguments list for a function call at {}", parent.loc); // todo: fix location
            return None;
        };
        tail = tail2;
        match next.kind {
            TokenKind::COMMA => {},
            TokenKind::RIGHT_PAREN => return Some((args, tail)),
            _ => {
                eprintln!("Expected ) or , but found {next} at {}", next.loc);
                return None;
            },
        }
    }
}

pub(crate) fn expect_token_kind(tail: &[Token], expected: TokenKind, start_loc: Location) -> Option<(&Token, &[Token])> {
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
