use std::io::{stdout, Write};
use std::rc::Rc;
use std::time::SystemTime;
use crate::parser_expressions::{parse_expression_from_string, BinaryOperator, Expression, ExpressionBody, UnaryOperator};
use crate::parser_statements::{parse_statement_list_from_string, Scope, Statement, StatementBody};
use crate::tokenizer::Location;
use crate::value::Value;
use crate::memory::Memory;

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

#[derive(Debug)]
enum StatementResult {
    Next,
    Return(Value),
    // could also be break or continue, but this language does not have those
}

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<Value> {
    let expr = parse_expression_from_string(str)?;
    let mut memory = Memory::new();
    let result = eval_expr(&expr, &mut memory, &mut stdout())?;
    Some(result)
}

pub(crate) fn evaluate_statements_list_from_string(str: &str, output: &mut impl Write) -> EvalResult {
    let Some(scope) = parse_statement_list_from_string(str) else {
        return EvalResult::ParseError;
    };
    let mut memory = Memory::new();
    let result = eval_scope(&scope, &mut memory, output, init_native_functions);
    match result {
        None => EvalResult::RuntimeError,
        Some(x) => match x {
            StatementResult::Next => EvalResult::Ok,
            other => {
                eprintln!("Unexpected {other:?}"); // todo: needs location
                EvalResult::RuntimeError
            }
        },
    }
}

fn eval_scope(scope: &Scope, memory: &mut Memory, output: &mut impl Write, init_scope: impl FnOnce(&mut Memory) -> Option<()>) -> Option<StatementResult> {
    memory.enter_scope();
    init_scope(memory)?;
    for statement in &scope.statements {
        let res = eval_statement(statement, memory, output);
        let res = match res {
            Some(x) => x,
            None => {
                memory.leave_scope();
                return None;
            }
        };
        match res {
            StatementResult::Next => {}
            other => {
                memory.leave_scope();
                return Some(other);
            }
        }
    }
    memory.leave_scope();
    Some(StatementResult::Next)
}

fn eval_statement(statement: &Statement, memory: &mut Memory, output: &mut impl Write) -> Option<StatementResult> {
    match &statement.body {
        StatementBody::Print(expr) => {
            let res = eval_expr(expr, memory, output)?;
            output.write_fmt(format_args!("{res}\n")).unwrap();
        },
        StatementBody::Expression(expr) => { eval_expr(expr, memory, output)?; },
        StatementBody::VariableDeclaration { name, value } => {
            let value = eval_expr(value, memory, output)?;
            memory.declare_variable(name, value, statement.loc)?;
        },
        StatementBody::Scope(scope) => {
            return eval_scope(scope, memory, output, |mem| Some(()));
        },
        StatementBody::If { condition, body, else_body } => {
            let condition_result = eval_expr(&condition, memory, output)?;
            let condition_result = cast_to_bool(&condition_result);
            let eval_body = if condition_result {
                body
            } else {
                else_body
            };
            if let Some(eval_body) = eval_body {
                return eval_statement(eval_body, memory, output);
            }
        }
        StatementBody::While { condition, body } => {
            let mut iter_count = 0;
            while cast_to_bool(&eval_expr(condition, memory, output)?) {
                if cfg!(test) {
                    iter_count += 1;
                    if iter_count > 100000 {
                        panic!("while loop reached iteration {iter_count}");
                    }
                }
                if let Some(body) = body {
                    let result = eval_statement(body, memory, output)?;
                    match result {
                        StatementResult::Next => {}
                        StatementResult::Return(x) => return Some(StatementResult::Return(x)),
                    }
                }
            }
        }
        StatementBody::For { init, condition, increment, body } => {
            if let Some(init) = init {
                eval_statement(init, memory, output)?;
            }
            let mut iter_count = 0;
            while cast_to_bool(&eval_expr(condition, memory, output)?) {
                if cfg!(test) {
                    iter_count += 1;
                    if iter_count > 100000 {
                        panic!("for loop reached iteration {iter_count}");
                    }
                }
                if let Some(body) = body {
                    let result = eval_statement(body, memory, output)?;
                    match result {
                        StatementResult::Next => {}
                        StatementResult::Return(x) => return Some(StatementResult::Return(x)),
                    }
                }
                if let Some(increment) = increment {
                    eval_expr(increment, memory, output)?;
                }
            }
        }
        StatementBody::FunctionDeclaration(func) => {
            memory.declare_user_function(func.clone(), statement.loc)?;
        }
        StatementBody::Return(expr) => {
            let res = eval_expr(expr, memory, output)?;
            return Some(StatementResult::Return(res));
        }
    }
    Some(StatementResult::Next)
}

fn eval_expr(expr: &Expression, memory: &mut Memory, output: &mut impl Write) -> Option<Value> {
    let loc = expr.loc;
    match &expr.body {
        ExpressionBody::Literal(x) => Some(x.clone()), // todo: is it possible to not clone this when we don't actually need to?
        ExpressionBody::Unary(expr) => {
            let value = eval_expr(&expr.ex, memory, output)?;
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
            let left = eval_expr(&expr.left, memory, output)?;
            match expr.op {
                BinaryOperator::Or => {
                    if cast_to_bool(&left) {
                        return Some(left);
                    }
                    let right = eval_expr(&expr.right, memory, output)?;
                    return Some(right);
                },
                BinaryOperator::And => {
                    if !cast_to_bool(&left) {
                        return Some(left);
                    }
                    let right = eval_expr(&expr.right, memory, output)?;
                    return Some(right);
                },
                _ => {},
            };
            let right = eval_expr(&expr.right, memory, output)?;
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
        ExpressionBody::Grouping(expr) => eval_expr(expr, memory, output),
        ExpressionBody::Variable(name) => {
            memory.get(&name, loc)
        }
        ExpressionBody::Assignment(expr) => {
            let name = &expr.var;
            let value = eval_expr(&expr.expr, memory, output)?;
            memory.assign(name, value.clone(), loc)?;
            Some(value)
        },
        ExpressionBody::Call { name, args } => {
            let function = memory.get(name, loc)?;
            match function {
                Value::NativeFunction(name) => match name {
                    "clock" => {
                        check_args_count(args, 0, name, loc)?;
                        Some(Value::Number(time_now()?))
                    },
                    _ => {
                        eprintln!("Native function {name} is not implemented, called at {loc}");
                        None
                    },
                }
                Value::UserFunction(function) => {
                    let function = function.inner;
                    check_args_count(args, function.args.len(), name, loc)?;
                    let arg_values = eval_args(args, memory, output)?;
                    let result = eval_scope(&function.body, memory, output, |mem| {
                        for (index, arg_value) in arg_values.into_iter().enumerate() {
                            mem.declare_variable(&function.args[index], arg_value, loc)?;
                        }
                        Some(())
                    })?;
                    let return_value = match result {
                        StatementResult::Next => Value::Nil,
                        StatementResult::Return(x) => x,
                    };
                    Some(return_value)
                }
                _ => {
                    eprintln!("{name} is not callable at {loc}");
                    None
                },
            }
        }
    }
}

fn eval_args(args: &[Expression], memory: &mut Memory, output: &mut impl Write) -> Option<Vec<Value>> {
    let mut arg_values = Vec::with_capacity(args.len());
    for arg in args {
        arg_values.push(eval_expr(arg, memory, output)?);
    }
    Some(arg_values)
}

fn init_native_functions(memory: &mut Memory) -> Option<()> {
    for func in ["clock"] {
        memory.declare_native_function(func)?;
    }
    Some(())
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
        let output = &mut output;
        assert_eval_with_output("var a = 1; var a = a + a + 3; print a;", EvalResult::Ok, "5\n", output);
        assert_eval_with_output("var a = 5; var b = a = a * 3; print a; print b;", EvalResult::Ok, "15\n15\n", output);
        assert_eval_with_output("var a = 15; var b = 15; b = a = a * 3; print a; print b;", EvalResult::Ok, "45\n45\n", output);
        assert_eval_with_output("var a = 45; b = a = a * 3; print a; print b;", EvalResult::RuntimeError, "", output);
    }

    #[test]
    fn test_strings() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("var a = \"test\"; var b = a; b = b + b; print a; print b;", EvalResult::Ok, "test\ntesttest\n", output);
    }

    #[test]
    fn test_scopes() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("var a = 1; {var a = a + 2; print a;} print a;", EvalResult::Ok, "3\n1\n", output);
        assert_eval_with_output("var a = 1; var b = 2; {var a = 3; a = 4; b = 5;} print a; print b;", EvalResult::Ok, "1\n5\n", output);
        assert_eval_with_output("{var a = 1;}}", EvalResult::ParseError, "", output);
    }

    #[test]
    fn test_if() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("if (true) print 1; else print 2; print 3;", EvalResult::Ok, "1\n3\n", output);
        assert_eval_with_output("if (true) print 1; else {print 2; print 3;}", EvalResult::Ok, "1\n", output);
        assert_eval_with_output("if (false) print 1; else print 2;", EvalResult::Ok, "2\n", output);
        assert_eval_with_output("if (false) print 1; else {print 2; print 3;}", EvalResult::Ok, "2\n3\n", output);
        assert_eval_with_output("if (false) print 1; print 2;", EvalResult::Ok, "2\n", output);
        assert_eval_with_output("if (false) {print 1; print 2;}", EvalResult::Ok, "", output);
        assert_eval_with_output("var a = false; if (a = true) {print 1;}", EvalResult::Ok, "1\n", output);
        assert_eval_with_output("if (true);", EvalResult::Ok, "", output);
        assert_eval_with_output("if true print 1;", EvalResult::ParseError, "", output);
        assert_eval_with_output("if (true) if (false) print 1; else print 2;", EvalResult::Ok, "2\n", output);
        assert_eval_with_output("if (true) {if (false) print 1;} else print 2;", EvalResult::Ok, "", output);
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
        let output = &mut output;
        assert_eval_with_output("var a = 1; (a = 2) or (a = 3); print a;", EvalResult::Ok, "2\n", output);
        assert_eval_with_output("var a = 1; (a = nil) or (a = 3); print a;", EvalResult::Ok, "3\n", output);
        assert_eval_with_output("var a = 1; (a = nil) and (a = 3); print a;", EvalResult::Ok, "nil\n", output);
        assert_eval_with_output("var a = 1; (a = 2) and (a = 3); print a;", EvalResult::Ok, "3\n", output);
    }

    #[test]
    fn test_while() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("var a = 3; while (a > 0) print a = a - 1; print 100;", EvalResult::Ok, "2\n1\n0\n100\n", output);
        assert_eval_with_output("var a = 3; while (a > 0) {print a = a - 1; print 100;}", EvalResult::Ok, "2\n100\n1\n100\n0\n100\n", output);
        assert_eval_with_output("var a = 3; while (a < 0) print a = a - 1; print 100;", EvalResult::Ok, "100\n", output);
        assert_eval_with_output("var a = 3; while (a < 0) {print a = a - 1; print 100;}", EvalResult::Ok, "", output);
        assert_eval_with_output("var a = 3; while ((a = a - 1) > 0); print a;", EvalResult::Ok, "0\n", output);
        assert_eval_with_output("var a = 3; while a > 0 print a = a - 1; print 100;", EvalResult::ParseError, "", output);
    }

    #[test]
    fn test_for() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("for (var a = 0; a > 0; a = a + 1) {}", EvalResult::Ok, "", output);
        assert_eval_with_output("for (var a = 1; a < 4; a = a + 1) {print a; print 9;}", EvalResult::Ok, "1\n9\n2\n9\n3\n9\n", output);
        assert_eval_with_output("for (var a = 1; a < 4; a = a + 1) print a; print 9;", EvalResult::Ok, "1\n2\n3\n9\n", output);
        assert_eval_with_output("for (var a = 1; a < 4; a = a + 1) print a; print a;", EvalResult::RuntimeError, "1\n2\n3\n", output);
        assert_eval_with_output("for (;;;);", EvalResult::ParseError, "", output);
    }

    #[test]
    fn test_native_functions() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        
        let expected_min = time_now().unwrap() - 100.0;
        let statements = "print clock();";
        let res = evaluate_statements_list_from_string(statements, output);
        assert_eq!(EvalResult::Ok, res);
        assert!(std::str::from_utf8(&output).unwrap().trim().parse::<f64>().unwrap() > expected_min);
        output.truncate(0);

        assert_eval_with_output("print clock;", EvalResult::Ok, "<native fn clock>\n", output);
        assert_eval_with_output("print clock(;", EvalResult::ParseError, "", output);
        assert_eval_with_output("print clock(1);", EvalResult::RuntimeError, "", output);

        assert_eval_with_output("var clock = 1;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("var clock = clock;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("var test = clock; print test;", EvalResult::Ok, "<native fn clock>\n", output);
        assert_eval_with_output("{var clock = 1; print clock;} print clock;", EvalResult::Ok, "1\n<native fn clock>\n", output);
        assert_eval_with_output("{var clock = clock; print clock;} print clock;", EvalResult::Ok, "<native fn clock>\n<native fn clock>\n", output);
    }

    #[test]
    fn test_user_functions() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("fun foo() {} print foo(); print foo;", EvalResult::Ok, "nil\n<fn foo>\n", output);
        assert_eval_with_output("fun foo(a, b) {print a + b;} foo(1, 2 + 3); ", EvalResult::Ok, "6\n", output);
        assert_eval_with_output("foo(); ", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo(); ", EvalResult::ParseError, "", output);

        assert_eval_with_output("fun foo() {}; var foo = 1;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo() {}; var foo = foo;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("var foo = 1; fun foo() {};", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo() {}; var test = foo; print test;", EvalResult::Ok, "<fn foo>\n", output);
        assert_eval_with_output("fun foo() {}; {var foo = 1; print foo;} print foo;", EvalResult::Ok, "1\n<fn foo>\n", output);
        assert_eval_with_output("var foo = 1; {fun foo() {}; print foo;} print foo;", EvalResult::Ok, "<fn foo>\n1\n", output);
    }

    #[test]
    fn test_return() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("fun foo() {{{{return 1;}}}} print foo();", EvalResult::Ok, "1\n", output);
        assert_eval_with_output("fun foo() {return;} print foo();", EvalResult::Ok, "nil\n", output);
        assert_eval_with_output(
            "fun foo(cond) {for (;;) while (true) if (cond) return 1; else return 2;} print foo(true); print foo(false);", 
            EvalResult::Ok, "1\n2\n", output
        );
    }
    
    fn assert_eval_with_output(statements: &str, expect_result: EvalResult, expect_output: &str, output: &mut Vec<u8>) {
        let res = evaluate_statements_list_from_string(statements, output);
        assert_eq!(expect_result, res);
        assert_eq!(expect_output, std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }
}