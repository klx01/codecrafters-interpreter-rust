use std::collections::HashMap;
use std::io::Write;
use crate::parser_expressions::{parse_expression_from_string, BinaryOperator, Expression, ExpressionBody, Literal, UnaryOperator};
use crate::parser_statements::{parse_statement_list_from_string, Scope, Statement, StatementBody};
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

type MemoryScope = HashMap<String, Literal>;
#[derive(Default)]
struct Memory {
    scopes: Vec<MemoryScope>,
}
impl Memory {
    fn new() -> Self {
        Self::default()
    }
    fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
    }
    fn leave_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, name: &str, value: Literal) {
        let scope = self.scopes.last_mut().expect("No scopes left in memory!");
        if let Some(val_ref) = scope.get_mut(name) {
            *val_ref = value;
        } else {
            scope.insert(name.to_string(), value);
        }
    }
    fn assign(&mut self, name: &str, value: Literal) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                *val = value;
                return true;
            }
        }
        false
    }
    fn get(&self, name: &str) -> Option<&Literal> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<Literal> {
    let expr = parse_expression_from_string(str)?;
    let mut memory = Memory::new();
    let result = eval_expr(&expr, &mut memory)?;
    Some(result)
}

pub(crate) fn evaluate_statements_list_from_string(str: &str, output: &mut impl Write) -> EvalResult {
    let Some(scope) = parse_statement_list_from_string(str) else {
        return EvalResult::ParseError;
    };
    let mut memory = Memory::new();
    if eval_scope(&scope, &mut memory, output).is_none() {
        return EvalResult::RuntimeError;
    }
    EvalResult::Ok
}

fn eval_scope(scope: &Scope, memory: &mut Memory, output: &mut impl Write) -> Option<()> {
    memory.enter_scope();
    for statement in &scope.statements {
        if eval_statement(statement, memory, output).is_none() {
            memory.leave_scope();
            return None;
        }
    }
    memory.leave_scope();
    Some(())
}

fn eval_statement(statement: &Statement, memory: &mut Memory, output: &mut impl Write) -> Option<()> {
    match &statement.body {
        StatementBody::Print(expr) => {
            let res = eval_expr(expr, memory)?;
            output.write_fmt(format_args!("{res}\n")).unwrap();
        },
        StatementBody::Expression(expr) => { eval_expr(expr, memory)?; () },
        StatementBody::VariableDeclaration { name, value } => {
            let value = eval_expr(value, memory)?;
            memory.declare(name, value);
        },
        StatementBody::Scope(scope) => eval_scope(scope, memory, output)?,
        StatementBody::If { condition, body, else_body } => {
            let condition_result = eval_expr(&condition, memory)?;
            let condition_result = cast_to_bool(&condition_result);
            let eval_body = if condition_result {
                body
            } else {
                else_body
            };
            if let Some(eval_body) = eval_body {
                eval_statement(eval_body, memory, output);
            }
        }
        StatementBody::While { condition, body } => {
            let mut iter_count = 0;
            while cast_to_bool(&eval_expr(condition, memory)?) {
                if cfg!(test) {
                    iter_count += 1;
                    if iter_count > 100000 {
                        panic!("while loop reached iteration {iter_count}");
                    }
                }
                if let Some(body) = body {
                    eval_statement(body, memory, output);
                }
            }
        }
        StatementBody::For { init, condition, increment, body } => {
            if let Some(init) = init {
                eval_statement(init, memory, output)?;
            }
            let mut iter_count = 0;
            while cast_to_bool(&eval_expr(condition, memory)?) {
                if cfg!(test) {
                    iter_count += 1;
                    if iter_count > 100000 {
                        panic!("for loop reached iteration {iter_count}");
                    }
                }
                if let Some(body) = body {
                    eval_statement(body, memory, output);
                }
                if let Some(increment) = increment {
                    eval_expr(increment, memory)?;
                }
            }
        }
    }
    Some(())
}

fn eval_expr(expr: &Expression, memory: &mut Memory) -> Option<Literal> {
    let loc = expr.loc;
    match &expr.body {
        ExpressionBody::Literal(x) => Some(x.clone()), // todo: is it possible to not clone this when we don't actually need to?
        ExpressionBody::Unary(expr) => {
            let value = eval_expr(&expr.ex, memory)?;
            match expr.op {
                UnaryOperator::Minus => match value {
                    Literal::Number(n) => Some(Literal::Number(-n)),
                    _ => {
                        eprintln!("operator {} can not be applied to value {value:?} at {loc}", expr.op);
                        None
                    }
                }
                UnaryOperator::Not => Some(Literal::Bool(!cast_to_bool(&value))),
            }
        }
        ExpressionBody::Binary(expr) => {
            let left = eval_expr(&expr.left, memory)?;
            match expr.op {
                BinaryOperator::Or => {
                    if cast_to_bool(&left) {
                        return Some(left);
                    }
                    let right = eval_expr(&expr.right, memory)?;
                    return Some(right);
                },
                BinaryOperator::And => {
                    if !cast_to_bool(&left) {
                        return Some(left);
                    }
                    let right = eval_expr(&expr.right, memory)?;
                    return Some(right);
                },
                _ => {},
            };
            let right = eval_expr(&expr.right, memory)?;
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
                BinaryOperator::Or => unreachable!(),
                BinaryOperator::And => unreachable!(),
            }
        },
        ExpressionBody::Grouping(expr) => eval_expr(expr, memory),
        ExpressionBody::Variable(name) => {
            if let Some(val) = memory.get(&name) {
                Some(val.clone())
            } else {
                eprintln!("Can not read an undefined variable {name} at {loc}");
                None
            }
        }
        ExpressionBody::Assignment(expr) => {
            let name = &expr.var;
            let value = eval_expr(&expr.expr, memory)?;
            let is_found = memory.assign(name, value.clone());
            if !is_found {
                eprintln!("Can not assign to an undefined variable {name} at {loc}");
                return None;
            }
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

fn cast_to_bool(value: &Literal) -> bool {
    match value {
        Literal::Nil => false,
        Literal::Bool(val) => *val,
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
        let mut output = Vec::<u8>::new();

        let statements = "var a = 1; var a = a + a +3; print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("5\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 5; var b = a = a * 3; print a; print b;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("15\n15\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 15; var b = 15; b = a = a * 3; print a; print b;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("45\n45\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 45; b = a = a * 3; print a; print b;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::RuntimeError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }

    #[test]
    fn test_scopes() {
        let mut output = Vec::<u8>::new();

        let statements = "var a = 1; {var a = a + 2; print a;} print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("3\n1\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 1; var b = 2; {var a = 3; a = 4; b = 5;} print a; print b;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n5\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "{var a = 1;}}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::ParseError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }

    #[test]
    fn test_if() {
        let mut output = Vec::<u8>::new();

        let statements = "if (true) print 1; else print 2; print 3;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n3\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (true) print 1; else {print 2; print 3;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (false) print 1; else print 2;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (false) print 1; else {print 2; print 3;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n3\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (false) print 1; print 2;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (false) {print 1; print 2;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = false; if (a = true) {print 1;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (true);";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if true print 1;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::ParseError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (true) if (false) print 1; else print 2;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "if (true) {if (false) print 1;} else print 2;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }

    #[test]
    fn test_bool_ops() {
        assert_eq!("true", evaluate_expr_from_string("true or false and false").unwrap().to_string());
        assert_eq!("false", evaluate_expr_from_string("(true or false) and false").unwrap().to_string());

        assert_eq!("1", evaluate_expr_from_string("nil or 1").unwrap().to_string());
        assert_eq!("1", evaluate_expr_from_string("1 or nil").unwrap().to_string());
        assert_eq!("nil", evaluate_expr_from_string("false or nil").unwrap().to_string());

        assert_eq!("nil", evaluate_expr_from_string("nil and 1").unwrap().to_string());
        assert_eq!("nil", evaluate_expr_from_string("1 and nil").unwrap().to_string());
        assert_eq!("asd", evaluate_expr_from_string("1 and \"asd\"").unwrap().to_string());
    }

    #[test]
    fn test_bool_ops_short_circuit() {
        let mut output = Vec::<u8>::new();

        let statements = "var a = 1; (a = 2) or (a = 3); print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 1; (a = nil) or (a = 3); print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("3\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 1; (a = nil) and (a = 3); print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("nil\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 1; (a = 2) and (a = 3); print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("3\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }

    #[test]
    fn test_while() {
        let mut output = Vec::<u8>::new();

        let statements = "var a = 3; while (a > 0) print a = a - 1; print 100;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n1\n0\n100\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 3; while (a > 0) {print a = a - 1; print 100;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("2\n100\n1\n100\n0\n100\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 3; while (a < 0) print a = a - 1; print 100;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("100\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 3; while (a < 0) {print a = a - 1; print 100;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 3; while ((a = a - 1) > 0); print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("0\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "var a = 3; while a > 0 print a = a - 1; print 100;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::ParseError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }

    #[test]
    fn test_for() {
        let mut output = Vec::<u8>::new();

        /*let statements = "for (;;);";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);*/

        let statements = "for (var a = 0; a > 0; a = a + 1) {}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "for (var a = 1; a < 4; a = a + 1) {print a; print 9;}";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n9\n2\n9\n3\n9\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "for (var a = 1; a < 4; a = a + 1) print a; print 9;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("1\n2\n3\n9\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "for (var a = 1; a < 4; a = a + 1) print a; print a;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::RuntimeError, res);
        assert_eq!("1\n2\n3\n", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "for (;;;);";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::ParseError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }
}