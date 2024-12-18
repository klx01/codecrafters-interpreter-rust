use std::borrow::Cow;
use std::io::{stdout, Write};
use std::time::SystemTime;
use crate::parser_expressions::{parse_expression, BinaryOperator, Expression, ExpressionBody, UnaryOperator};
use crate::parser_statements::{parse_scope, Scope, Statement, StatementBody};
use crate::tokenizer::{tokenize_string_no_eof, Location};
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
enum StatementResult<'a> {
    Next,
    Return{val: Value<'a>, loc: Location},
    // could also be break or continue, but this language does not have those
}

pub(crate) fn evaluate_expr_from_string(str: &str) -> Option<String> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (expr, tail) = parse_expression(&tokens, None)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of expression");
    }
    let mut memory = Memory::new();
    let result = eval_expr(&expr, &mut memory, &mut stdout())?;
    Some(result.to_string())
}

pub(crate) fn evaluate_statements_list_from_string(str: &str, output: &mut impl Write) -> EvalResult {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let Some((scope, tail, _, end)) = parse_scope(&tokens, None) else {
        return EvalResult::ParseError;
    };
    if !tail.is_empty() {
        eprintln!("Unexpected end of scope at {}", end);
        return EvalResult::ParseError;
    }

    let mut memory = Memory::new();
    for func in ["clock", "_debug_native_function"] {
        let success = memory.declare_native_function(func);
        if !success {
            return EvalResult::RuntimeError;
        }
    }
    let result = eval_scope(&scope, &mut memory, output);
    match result {
        None => EvalResult::RuntimeError,
        Some(x) => match x {
            StatementResult::Next => EvalResult::Ok,
            StatementResult::Return {loc, ..} => {
                eprintln!("Unexpected return at {loc}");
                EvalResult::RuntimeError
            }
        },
    }
}

fn eval_scope<'a>(scope: &'a Scope<'a>, memory: &mut Memory<'a>, output: &mut impl Write) -> Option<StatementResult<'a>> {
    for statement in &scope.statements {
        let res = eval_statement(statement, memory, output)?;
        match res {
            StatementResult::Next => {}
            other => {
                return Some(other);
            }
        }
    }
    Some(StatementResult::Next)
}

fn eval_statement<'a>(statement: &'a Statement<'a>, memory: &mut Memory<'a>, output: &mut impl Write) -> Option<StatementResult<'a>> {
    match &statement.body {
        StatementBody::Print(expr) => {
            let res = eval_expr(expr, memory, output)?;
            output.write_fmt(format_args!("{res}\n")).unwrap();
        },
        StatementBody::Expression(expr) => { eval_expr(expr, memory, output)?; },
        StatementBody::VariableDeclaration { name, value } => {
            let value = eval_expr(value, memory, output)?;
            memory.declare_variable(name, value, statement.start)?;
        },
        StatementBody::Scope(scope) => {
            memory.enter_scope();
            let result = eval_scope(scope, memory, output);
            memory.leave_scope();
            return result;
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
                        StatementResult::Return{..} => return Some(result),
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
                        StatementResult::Return{..} => return Some(result),
                    }
                }
                if let Some(increment) = increment {
                    eval_expr(increment, memory, output)?;
                }
            }
        }
        StatementBody::FunctionDeclaration(func) => {
            memory.declare_user_function(func, statement.start)?;
        }
        StatementBody::Return(expr) => {
            let res = eval_expr(expr, memory, output)?;
            return Some(StatementResult::Return{val: res, loc: expr.start});
        }
    }
    Some(StatementResult::Next)
}

fn eval_expr<'a>(expr: &Expression<'a>, memory: &mut Memory<'a>, output: &mut impl Write) -> Option<Value<'a>> {
    let loc = expr.start;
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
                BinaryOperator::Equal => Some(Value::Bool(is_equal(&left, &right))),
                BinaryOperator::NotEqual => Some(Value::Bool(!is_equal(&left, &right))),
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
                        Some(Value::String(Cow::Owned(res)))
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
        ExpressionBody::Call { expr, args } => {
            let value = eval_expr(expr, memory, output)?;
            match &value {
                Value::NativeFunction(name) => match name {
                    &"clock" => {
                        check_args_count(args, 0, name, loc)?;
                        Some(Value::Number(time_now()?))
                    },
                    &"_debug_native_function" => {
                        check_args_count(args, 0, name, loc)?;
                        Some(Value::Nil)
                    },
                    _ => {
                        eprintln!("Native function {name} is not implemented, called at {loc}");
                        None
                    },
                }
                Value::UserFunction(function) => {
                    let inner = &function.inner;
                    check_args_count(args, inner.args.len(), &inner.name, loc)?;
                    let arg_values = eval_args(args, memory, output)?;
                    memory.enter_call();
                    if function.captures.is_some() {
                        memory.init_closure(&function, loc)?;
                    }
                    for (index, arg_value) in arg_values.into_iter().enumerate() {
                        memory.declare_variable(&inner.args[index], arg_value, loc)?;
                    }
                    let result = eval_scope(&inner.body, memory, output);
                    memory.leave_call();
                    let return_value = match result? {
                        StatementResult::Next => Value::Nil,
                        StatementResult::Return{val, ..} => val,
                    };
                    Some(return_value)
                }
                _ => {
                    eprintln!("Value {value} is not callable at {} - {}", expr.start, expr.end);
                    None
                },
            }
        }
    }
}

fn eval_args<'a>(args: &[Expression<'a>], memory: &mut Memory<'a>, output: &mut impl Write) -> Option<Vec<Value<'a>>> {
    let mut arg_values = Vec::with_capacity(args.len());
    for arg in args {
        arg_values.push(eval_expr(arg, memory, output)?);
    }
    Some(arg_values)
}

fn is_equal<'a>(left: &Value<'a>, right: &Value<'a>) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(left), Value::Bool(right)) => left == right,
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::NativeFunction(left), Value::NativeFunction(right)) => left == right,
        (Value::UserFunction(left), Value::UserFunction(right)) => left == right,
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

fn process_number_values<'a>(left: &Value, right: &Value, res: impl Fn(f64, f64) -> Value<'a>, loc: Location, op: BinaryOperator) -> Option<Value<'a>> {
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
    use super::*;

    #[test]
    fn test_literals() {
        let res = evaluate_expr_from_string("nil");
        assert_eq!("nil", res.unwrap().to_string());
        let res = evaluate_expr_from_string("true");
        assert_eq!("true", res.unwrap().to_string());
        let res = evaluate_expr_from_string("false");
        assert_eq!("false", res.unwrap().to_string());

        let res = evaluate_expr_from_string("\"Hello, World!\"");
        assert_eq!("Hello, World!", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10.40");
        assert_eq!("10.4", res.unwrap().to_string());
        let res = evaluate_expr_from_string("10");
        assert_eq!("10", res.unwrap().to_string());

        let res = evaluate_expr_from_string("((false))");
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
        assert_eval_with_output(
            "var a = \"test\"; var b = a; var c = \"test\"; print a == b; print a == c; b = b + b; print a; print b;", 
            EvalResult::Ok, "true\ntrue\ntest\ntesttest\n", output
        );
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
        assert_eval_with_output(
            "var test = clock; print test == clock; print clock == clock; print _debug_native_function == clock;",
            EvalResult::Ok, "true\ntrue\nfalse\n", output
        );
        assert_eval_with_output("{var clock = 1; print clock;} print clock;", EvalResult::Ok, "1\n<native fn clock>\n", output);
        assert_eval_with_output("{var clock = clock; print clock;} print clock;", EvalResult::Ok, "<native fn clock>\n<native fn clock>\n", output);
    }

    #[test]
    fn test_user_functions() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output("fun foo() {} print foo(); print foo;", EvalResult::Ok, "nil\n<fn foo>\n", output);
        assert_eval_with_output("fun foo(a, b) {print a + b;} foo(1, 2 + 3);", EvalResult::Ok, "6\n", output);
        assert_eval_with_output("var a = 1; fun foo() {print a;} {var a = 2; foo();}", EvalResult::Ok, "1\n", output);
        assert_eval_with_output("foo(); ", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo(); ", EvalResult::ParseError, "", output);

        assert_eval_with_output("fun foo() {{{{return 1;}}}} print foo();", EvalResult::Ok, "1\n", output);
        assert_eval_with_output("fun foo() {return;} print foo();", EvalResult::Ok, "nil\n", output);
        assert_eval_with_output(
            "fun foo(cond) {for (;;) while (true) if (cond) return 1; else return 2;} print foo(true); print foo(false);",
            EvalResult::Ok, "1\n2\n", output
        );

        assert_eval_with_output(
            "var a = 0; fun foo() {var a = 1; bar(); print a;} fun bar() {print 4;} {var a = 2; foo(); print a;} print a;",
            EvalResult::Ok, "4\n1\n2\n0\n", output
        );
        assert_eval_with_output(
            "fun foo() {} fun bar() {fun foo() {} return foo;} fun baz() {} var test = foo; print foo == test; print foo == foo; print foo == bar; print foo == bar(); print foo() == baz;",
            EvalResult::Ok, "true\ntrue\nfalse\nfalse\nfalse\n", output
        );

        assert_eval_with_output("fun foo() {}; var foo = 1;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo() {}; var foo = foo;", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("var foo = 1; fun foo() {};", EvalResult::RuntimeError, "", output);
        assert_eval_with_output("fun foo() {}; var test = foo; print test;", EvalResult::Ok, "<fn foo>\n", output);
        assert_eval_with_output("fun foo() {}; {var foo = 1; print foo;} print foo;", EvalResult::Ok, "1\n<fn foo>\n", output);
        assert_eval_with_output("var foo = 1; {fun foo() {}; print foo;} print foo;", EvalResult::Ok, "<fn foo>\n1\n", output);
        assert_eval_with_output("100();", EvalResult::RuntimeError, "", output);
    }

    #[test]
    fn test_closures() {
        let mut output = Vec::<u8>::new();
        let output = &mut output;
        assert_eval_with_output(
            "fun foo(a) { fun foo(b) {return a + b;} return foo; }
            var a = foo(1); print a(2); print a;
            var b = a; print a == b; print b(5);
            var c = foo(1); print c(2); print a == c;
            print foo(3)(4);
            ",
            EvalResult::Ok, "3\n<fn foo>\ntrue\n6\n3\nfalse\n7\n", output
        );
        assert_eval_with_output(
            "fun makeCounter() {
                var i = 0;
                fun count() {
                  i = i + 1;
                  var j = i;
                  var i = 0;
                  return j;
                }
                return count;
            }
            var a = makeCounter();
            var b = a;
            print a();
            print b();
            print a();
            print a == b;
            var c = makeCounter();
            print c();
            print a == c;
            ",
            EvalResult::Ok, "1\n2\n3\ntrue\n1\nfalse\n", output
        );
        assert_eval_with_output(
            "{
              fun fib(n) {
                if (n < 2) return n;
                return fib(n - 1) + fib(n - 2);
              }

              print fib(8);
            }
            ",
            EvalResult::Ok, "21\n", output
        );
    }
    
    fn assert_eval_with_output(statements: &str, expect_result: EvalResult, expect_output: &str, output: &mut Vec<u8>) {
        let res = evaluate_statements_list_from_string(statements, output);
        assert_eq!(expect_result, res);
        assert_eq!(expect_output, std::str::from_utf8(&output).unwrap());
        output.truncate(0);
    }
}