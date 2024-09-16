use std::collections::HashMap;
use crate::parser_expressions::{parse_expression_from_string, BinaryOperator, Expression, ExpressionBody, Literal, UnaryOperator};
use crate::parser_statements::{parse_statement_list_from_string, Statement, StatementBody};
use crate::tokenizer::Location;

#[derive(PartialEq, Debug)]
pub(crate) enum EvalResult {
    Ok,
    ParseError,
    RuntimeError,
}
impl EvalResult {
    pub(crate) fn get_exit_code(&self) -> Option<i32> {
        match self {
            EvalResult::Ok => None,
            EvalResult::ParseError => Some(65),
            EvalResult::RuntimeError => Some(70),
        }
    }
}

#[derive(Default)]
pub(crate) struct Memory {
    variables: HashMap<String, Literal>,
}

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<Literal> {
    let expr = parse_expression_from_string(str)?;
    let mut memory = Memory::default();
    let result = eval_expr(expr, &mut memory)?;
    Some(result)
}

pub(crate) fn evaluate_statements_list_from_string(str: &str, memory: &mut Memory) -> EvalResult {
    let Some(statements) = parse_statement_list_from_string(str) else {
        return EvalResult::ParseError;
    };
    if !eval_statements_list(statements, memory) {
        return EvalResult::RuntimeError;
    }
    EvalResult::Ok
}

fn eval_statements_list(statements: Vec<Statement>, memory: &mut Memory) -> bool {
    for statement in statements {
        if eval_statement(statement, memory).is_none() {
            return false;
        }
    }
    true
}

fn eval_statement(statement: Statement, memory: &mut Memory) -> Option<()> {
    match statement.body {
        StatementBody::Nop => {},
        StatementBody::Print(expr) => println!("{}", eval_expr(expr, memory)?),
        StatementBody::Expression(expr) => { eval_expr(expr, memory)?; () },
        StatementBody::VariableDeclaration { name, value } => {
            let value = eval_expr(value, memory)?;
            memory.variables.insert(name, value);
        },
    }
    Some(())
}

fn eval_expr(expr: Expression, memory: &mut Memory) -> Option<Literal> {
    let loc = expr.loc;
    match expr.body {
        ExpressionBody::Literal(x) => Some(x),
        ExpressionBody::Unary(expr) => {
            let value = eval_expr(expr.ex, memory)?;
            match expr.op {
                UnaryOperator::Minus => match value {
                    Literal::Number(n) => Some(Literal::Number(-n)),
                    _ => {
                        eprintln!("operator {} can not be applied to value {value:?} at {loc}", expr.op);
                        None
                    }
                }
                UnaryOperator::Not => Some(Literal::Bool(!cast_to_bool(value))),
            }
        }
        ExpressionBody::Binary(expr) => {
            let left = eval_expr(expr.left, memory)?;
            let right = eval_expr(expr.right, memory)?;
            match expr.op {
                BinaryOperator::Equal => Some(Literal::Bool(is_equal(left, right))),
                BinaryOperator::NotEqual => Some(Literal::Bool(!is_equal(left, right))),
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
        ExpressionBody::Grouping(expr) => eval_expr(*expr, memory),
        ExpressionBody::Variable(name) => {
            if let Some(val) = memory.variables.get(&name) {
                Some(val.clone())
            } else {
                eprintln!("Undefined variable {name} at {loc}");
                None
            }
        }
        ExpressionBody::Assignment(expr) => {
            let name = expr.var;
            let value = eval_expr(expr.expr, memory)?;
            memory.variables.insert(name, value.clone());
            Some(value)
        },
    }
}

fn is_equal(left: Literal, right: Literal) -> bool {
    match (left, right) {
        (Literal::Nil, Literal::Nil) => true,
        (Literal::Bool(left), Literal::Bool(right)) => left == right,
        (Literal::Number(left), Literal::Number(right)) => left == right,
        (Literal::String(left), Literal::String(right)) => left == right,
        (_, _) => false,
    }
}

fn cast_to_bool(value: Literal) -> bool {
    match value {
        Literal::Nil => false,
        Literal::Bool(val) => val,
        _ => true,
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
    fn test_literals() {
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
        assert_eq!("10", res.unwrap().to_string());

        let res = evaluate_expr_from_string("((false))");
        assert_eq!(Some(Literal::Bool(false)), res);
        assert_eq!("false", res.unwrap().to_string());
    }

    #[test]
    fn test_unary() {
        assert_eq!("true", evaluate_expr_from_string("!false").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("!true").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("!!true").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("!!nil").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("!!1").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("!!\"str\"").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("!!\"\"").unwrap().to_string());

        assert_eq!("-73", evaluate_expr_from_string("-(73)").unwrap().to_string());
        assert_eq!(None, evaluate_expr_from_string("-false"));
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!("3", evaluate_expr_from_string("(18 * 3 / (3 * 6))").unwrap().to_string());
        assert_eq!("8.4", evaluate_expr_from_string("42 / 5").unwrap().to_string());
        assert_eq!("10.4", evaluate_expr_from_string("(10.40 * 2) / 2").unwrap().to_string());

        assert_eq!("75", evaluate_expr_from_string("20 + 74 - (-(14 - 33))").unwrap().to_string());
        assert_eq!("5", evaluate_expr_from_string("70 - 65").unwrap().to_string());
        assert_eq!("-24", evaluate_expr_from_string("69 - 93").unwrap().to_string());
        assert_eq!("8.4", evaluate_expr_from_string("10.40 - 2").unwrap().to_string());
        assert_eq!("13", evaluate_expr_from_string("23 + 28 - (-(61 - 99))").unwrap().to_string());

        assert_eq!("2", evaluate_expr_from_string("1 + 1").unwrap().to_string());
        assert_eq!(None, evaluate_expr_from_string("1 + true"));
        assert_eq!(None, evaluate_expr_from_string("true + 1"));
        assert_eq!(None, evaluate_expr_from_string("true + true"));
    }

    #[test]
    fn test_concat() {
        assert_eq!("hello world!", evaluate_expr_from_string("\"hello\" + \" world!\"").unwrap().to_string());
        assert_eq!("4224", evaluate_expr_from_string("\"42\" + \"24\"").unwrap().to_string());
        assert_eq!(None, evaluate_expr_from_string("\"42\" + 24"));
        assert_eq!(None, evaluate_expr_from_string("42 + \"24\""));
    }

    #[test]
    fn test_comparison() {
        assert_eq!("true", evaluate_expr_from_string("1 > 0").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 > 1").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 >= 1").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 >= 2").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 >= 0").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 < 2").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 < 1").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 <= 1").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 <= 0").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 <= 2").unwrap().to_string());

        assert_eq!(None, evaluate_expr_from_string("1 > true"));
        assert_eq!(None, evaluate_expr_from_string("true > 1"));
        assert_eq!(None, evaluate_expr_from_string("true > true"));
    }

    #[test]
    fn test_equality() {
        assert_eq!("true", evaluate_expr_from_string("nil == nil").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("false == false").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("true == true").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("1 == 1").unwrap().to_string());
        assert_eq!("true", evaluate_expr_from_string("\"foo\" == \"foo\"").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("true == false").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("nil == false").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 == 2").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("\"foo\" == \"bar\"").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 == true").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("1 == \"1\"").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("\"\" == nil").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("\"\" == true").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("\"\" == false").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("\"\" == 0").unwrap().to_string());
    }

    #[test]
    fn test_variables() {
        let mut memory = Memory::default();
        let statements = "var a = 1; var a = a + a +3;";
        let res = evaluate_statements_list_from_string(statements, &mut memory);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("5", memory.variables.get("a").unwrap().to_string());
        
        let statements = "var b = a = a * 3;";
        let res = evaluate_statements_list_from_string(statements, &mut memory);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("15", memory.variables.get("b").unwrap().to_string());
        assert_eq!("15", memory.variables.get("a").unwrap().to_string());
    }
}