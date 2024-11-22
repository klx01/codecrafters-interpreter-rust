use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::time::SystemTime;
use crate::parser_expressions::{parse_expression_from_string, BinaryOperator, Expression, ExpressionBody, UnaryOperator};
use crate::parser_statements::{parse_statement_list_from_string, Scope, Statement, StatementBody};
use crate::tokenizer::Location;
use crate::value::Value;

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

type MemoryScope = HashMap<String, Value>;
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
    fn declare(&mut self, name: &str, value: Value) {
        let scope = self.scopes.last_mut().expect("No scopes left in memory!");
        if let Some(val_ref) = scope.get_mut(name) {
            *val_ref = value;
        } else {
            scope.insert(name.to_string(), value);
        }
    }
    fn assign(&mut self, name: &str, value: Value) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                *val = value;
                return true;
            }
        }
        false
    }
    fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<Value> {
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
        StatementBody::FunctionDeclaration { name, args, body } => {
            todo!()
        }
    }
    Some(())
}

fn eval_expr(expr: &Expression, memory: &mut Memory) -> Option<Value> {
    let loc = expr.loc;
    match &expr.body {
        ExpressionBody::Literal(x) => Some(x.clone()), // todo: is it possible to not clone this when we don't actually need to?
        ExpressionBody::Unary(expr) => {
            let value = eval_expr(&expr.ex, memory)?;
            match expr.op {
                UnaryOperator::Minus => match value {
                    Value::Number(n) => Some(Value::Number(-n)),
                    _ => {
                        eprintln!("operator {} can not be applied to value {value:?} at {loc}", expr.op);
                        None
                    }
                }
                UnaryOperator::Not => Some(Value::Bool(!cast_to_bool(&value))),
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
                BinaryOperator::Equal => Some(Value::Bool(is_equal(left, right))),
                BinaryOperator::NotEqual => Some(Value::Bool(!is_equal(left, right))),
                BinaryOperator::Less => process_number_values(
                    &left, &right,
                    |left, right| Value::Bool(left < right),
                    loc, expr.op
                ),
                BinaryOperator::LessOrEqual => process_number_values(
                    &left, &right,
                    |left, right| Value::Bool(left <= right),
                    loc, expr.op
                ),
                BinaryOperator::Greater => process_number_values(
                    &left, &right,
                    |left, right| Value::Bool(left > right),
                    loc, expr.op
                ),
                BinaryOperator::GreaterOrEqual => process_number_values(
                    &left, &right,
                    |left, right| Value::Bool(left >= right),
                    loc, expr.op
                ),
                BinaryOperator::Plus => match (left, right) {
                    (Value::Number(left), Value::Number(right)) => {
                        Some(Value::Number(left + right))
                    }
                    (Value::String(left), Value::String(right)) => {
                        // todo: this is really not optimal if this is the only remaining usage of left, or if any of them is empty
                        let res = format!("{left}{right}");
                        Some(Value::String(Rc::new(res)))
                    }
                    (left, right) => {
                        eprintln!("expected both operands to be numbers or strings for {} at {loc}, got {left:?} and {right:?}", expr.op);
                        None
                    }
                }
                BinaryOperator::Minus => process_number_values(
                    &left, &right,
                    |left, right| Value::Number(left - right),
                    loc, expr.op
                ),
                BinaryOperator::Multiply => process_number_values(
                    &left, &right,
                    |left, right| Value::Number(left * right),
                    loc, expr.op
                ),
                BinaryOperator::Divide => process_number_values(
                    &left, &right,
                    |left, right| Value::Number(left / right),
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
        ExpressionBody::Call { name, args } => {
            match name.as_str() {
                "clock" => {
                    check_args_count(args, 0, name, loc)?;
                    Some(Value::Number(time_now()?))
                },
                _ => {
                    eprintln!("Unknown function name {name} at {loc}");
                    None
                },
            }
        }
    }
}

fn is_equal(left: Value, right: Value) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (_, _) => false,
    }
}

fn cast_to_bool(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Bool(val) => *val,
        _ => true,
    }
}

fn process_number_values(left: &Value, right: &Value, res: impl Fn(f64, f64) -> Value, loc: Location, op: BinaryOperator) -> Option<Value> {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => {
            Some(res(*left, *right))
        }
        (_, _) => {
            eprintln!("expected both operands to be numbers for {op} at {loc}, got {left:?} and {right:?}");
            None
        }
    }
}

fn check_args_count(args: &[Expression], expected: usize, name: &str, loc: Location) -> Option<()> {
    let count = args.len();
    if count < expected {
        eprintln!("Too few arguments when calling {name} at {loc}, expected {expected}, got {count}");
        return None;
    }
    if count > expected {
        eprintln!("Too many arguments when calling {name} at {loc}, expected {expected}, got {count}");
        return None;
    }
    Some(())
}

fn time_now() -> Option<f64> {
    let now = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(x) => x,
        Err(err) => {
            eprintln!("{err}");
            return None;
        }
    };
    Some(now.as_secs_f64())
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use super::*;

    #[test]
    fn test_literals() {
        let res = evaluate_expr_from_string("nil");
        assert_eq!(Some(Value::Nil), res);
        assert_eq!("nil", res.unwrap().to_string());
        let res = evaluate_expr_from_string("true");
        assert_eq!(Some(Value::Bool(true)), res);
        assert_eq!("true", res.unwrap().to_string());
        let res = evaluate_expr_from_string("false");
        assert_eq!(Some(Value::Bool(false)), res);
        assert_eq!("false", res.unwrap().to_string());

        let res = evaluate_expr_from_string("\"Hello, World!\"");
        assert_eq!(Some(Value::String(Rc::new("Hello, World!".to_string()))), res);
        assert_eq!("Hello, World!", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10.40");
        assert_eq!(Some(Value::Number(10.4)), res);
        assert_eq!("10.4", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10");
        assert_eq!(Some(Value::Number(10.0)), res);
        assert_eq!("10", res.unwrap().to_string());

        let res = evaluate_expr_from_string("((false))");
        assert_eq!(Some(Value::Bool(false)), res);
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

        let statements = "var a = 1; var a = a + a + 3; print a;";
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
    fn test_strings() {
        let mut output = Vec::<u8>::new();
        let statements = "var a = \"test\"; var b = a; b = b + b; print a; print b;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert_eq!("test\ntesttest\n", std::str::from_utf8(&output).unwrap());
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

    #[test]
    fn test_functions() {
        let mut output = Vec::<u8>::new();
        let expected_min = time_now().unwrap() - 100.0;

        let statements = "print clock();";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::Ok, res);
        assert!(std::str::from_utf8(&output).unwrap().trim().parse::<f64>().unwrap() > expected_min);
        output.truncate(0);

        let statements = "print clock(;";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::ParseError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);

        let statements = "print clock(1);";
        let res = evaluate_statements_list_from_string(statements, &mut output);
        assert_eq!(EvalResult::RuntimeError, res);
        assert_eq!("", std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }
}