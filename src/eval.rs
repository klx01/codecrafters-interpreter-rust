use std::mem;
use crate::parser::{parse_expression_from_string, BinaryOperator, Expression, ExpressionBody, Literal, UnaryOperator};
use crate::tokenizer::Location;

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<Literal> {
    let expr = parse_expression_from_string(str)?;
    let result = eval(expr)?;
    Some(result)
}

fn eval(expr: Expression) -> Option<Literal> {
    let loc = expr.loc;
    match expr.body {
        ExpressionBody::Literal(x) => Some(x),
        ExpressionBody::Unary(expr) => {
            let value = eval(expr.ex)?;
            match expr.op {
                UnaryOperator::Minus => match value {
                    Literal::Number(n) => Some(Literal::Number(-n)),
                    _ => {
                        eprintln!("operator {} can not be applied to value {value:?} at {loc}", expr.op);
                        None
                    }
                }
                UnaryOperator::Not => match value {
                    Literal::Bool(b) => Some(Literal::Bool(!b)),
                    _ => {
                        eprintln!("operator {} can not be applied to value {value:?} at {loc}", expr.op);
                        None
                    }
                }
            }
        }
        ExpressionBody::Binary(expr) => {
            let left = eval(expr.left)?;
            let right = eval(expr.right)?;
            if mem::discriminant(&left) != mem::discriminant(&right) {
                eprintln!("mismatched types of values of operand {} at {loc}: {left:?} and {right:?}", expr.op);
                return None;
            }
            match expr.op {
                BinaryOperator::Equal => process_bool_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left == right),
                    loc, expr.op
                ),
                BinaryOperator::NotEqual => process_bool_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left != right),
                    loc, expr.op
                ),
                BinaryOperator::Less => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left < right),
                    loc, expr.op
                ),
                BinaryOperator::LessOrEqual => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left <= right),
                    loc, expr.op
                ),
                BinaryOperator::Greater => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left > right),
                    loc, expr.op
                ),
                BinaryOperator::GreaterOrEqual => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Bool(left >= right),
                    loc, expr.op
                ),
                BinaryOperator::Plus => match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => {
                        Some(Literal::Number(left + right))
                    }
                    (Literal::String(mut left), Literal::String(right)) => {
                        left.push_str(&right);
                        Some(Literal::String(left))
                    }
                    (left, right) => {
                        eprintln!("expected both operands to be numbers or strings for {} at {loc}, got {left:?} and {right:?}", expr.op);
                        None
                    }
                }
                BinaryOperator::Minus => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Number(left - right),
                    loc, expr.op
                ),
                BinaryOperator::Multiply => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Number(left * right),
                    loc, expr.op
                ),
                BinaryOperator::Divide => process_number_literals(
                    &left, &right,
                    |left, right| Literal::Number(left / right),
                    loc, expr.op
                ),
            }
        },
        ExpressionBody::Grouping(expr) => eval(*expr),
    }
}

fn process_bool_literals(left: &Literal, right: &Literal, res: impl Fn(bool, bool) -> Literal, loc: Location, op: BinaryOperator) -> Option<Literal> {
    match (left, right) {
        (Literal::Bool(left), Literal::Bool(right)) => {
            Some(res(*left, *right))
        }
        (_, _) => {
            eprintln!("expected both operands to be bool for {op} at {loc}, got {left:?} and {right:?}");
            None
        }
    }
}

fn process_number_literals(left: &Literal, right: &Literal, res: impl Fn(f64, f64) -> Literal, loc: Location, op: BinaryOperator) -> Option<Literal> {
    match (left, right) {
        (Literal::Number(left), Literal::Number(right)) => {
            Some(res(*left, *right))
        }
        (_, _) => {
            eprintln!("expected both operands to be numbers for {op} at {loc}, got {left:?} and {right:?}");
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let res = evaluate_expr_from_string("nil");
        assert_eq!(Some(Literal::Nil), res);
        assert_eq!("nil", res.unwrap().to_string());
        let res = evaluate_expr_from_string("true");
        assert_eq!(Some(Literal::Bool(true)), res);
        assert_eq!("true", res.unwrap().to_string());
        let res = evaluate_expr_from_string("false");
        assert_eq!(Some(Literal::Bool(false)), res);
        assert_eq!("false", res.unwrap().to_string());
        
        let res = evaluate_expr_from_string("\"Hello, World!\"");
        assert_eq!(Some(Literal::String("Hello, World!".to_string())), res);
        assert_eq!("Hello, World!", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10.40");
        assert_eq!(Some(Literal::Number(10.4)), res);
        assert_eq!("10.4", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10");
        assert_eq!(Some(Literal::Number(10.0)), res);
        assert_eq!("10.0", res.unwrap().to_string());
        
        let res = evaluate_expr_from_string("((false))");
        assert_eq!(Some(Literal::Bool(false)), res);
        assert_eq!("false", res.unwrap().to_string());
    }
}