use std::fmt::{Display, Formatter, Write};
use std::rc::Rc;
use crate::parser_expressions::{parse_expression, Expression, ExpressionBody, expect_token_kind, check_token_kind};
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};
use crate::value::{FunctionRef, FunctionValue, Value};

#[derive(Debug)]
pub(crate) enum StatementBody {
    Print(Expression),
    Expression(Expression),
    VariableDeclaration{name: String, value: Expression},
    Scope(Scope),
    If{condition: Expression, body: Option<Box<Statement>>, else_body: Option<Box<Statement>>},
    While{condition: Expression, body: Option<Box<Statement>>},
    For{init: Option<Box<Statement>>, condition: Expression, increment: Option<Expression>, body: Option<Box<Statement>>},
    FunctionDeclaration(FunctionRef),
    Return(Expression),
}
impl Display for StatementBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementBody::Print(expr) => f.write_fmt(format_args!("print {expr};")),
            StatementBody::Expression(expr) => f.write_fmt(format_args!("{expr};")),
            StatementBody::VariableDeclaration{name, value} => f.write_fmt(format_args!("var {name} = {value};")),
            StatementBody::Scope(scope) => f.write_fmt(format_args!("{scope}")),
            StatementBody::If{ condition, body, else_body } => {
                f.write_fmt(format_args!("if {condition}"))?;
                if let Some(body) = body {
                    f.write_fmt(format_args!(" {{{body}}}"))?;
                } else {
                    f.write_char(';')?;
                }
                if let Some(else_body) = else_body {
                    f.write_fmt(format_args!(" else {{{else_body}}}"))
                } else {
                    Ok(())
                }
            },
            StatementBody::While { condition, body } => {
                f.write_fmt(format_args!("while {condition}"))?;
                if let Some(body) = body {
                    f.write_fmt(format_args!(" {{{body}}}"))
                } else {
                    f.write_char(';')
                }
            }
            StatementBody::For { init, condition, increment, body } => {
                f.write_str("for (")?;
                if let Some(init) = init {
                    f.write_fmt(format_args!("{init}"))?;
                } else {
                    f.write_char(';')?;
                }
                f.write_fmt(format_args!(" {condition};"))?;
                if let Some(increment) = increment {
                    f.write_fmt(format_args!(" {increment}"))?;
                }
                f.write_char(')')?;
                if let Some(body) = body {
                    f.write_fmt(format_args!(" {{{body}}}"))
                } else {
                    f.write_char(';')
                }
            }
            StatementBody::FunctionDeclaration(func) => {
                let func = &func.inner;
                f.write_fmt(format_args!("fun {}(", func.name))?;
                for arg in &func.args {
                    f.write_fmt(format_args!("{arg}, "))?;
                }
                f.write_fmt(format_args!(") {}", func.body))
            },
            StatementBody::Return(expr) => f.write_fmt(format_args!("return {expr};")),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Statement {
    pub body: StatementBody,
    pub start: Location,
    pub end: Location,
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.body))
    }
}

enum ParseResult {
    Statement(Statement),
    Nop{start: Location, end: Location},
    ExitScope(Location),
}

#[derive(Debug)]
pub(crate) struct Scope {
    pub statements: Vec<Statement>,
}
impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        for stmt in &self.statements {
            f.write_fmt(format_args!("{stmt} "))?;
        }
        f.write_str("}")
    }
}

pub(crate) fn parse_statement_list_from_string(str: &str) -> Option<Scope> {
    // todo: how should we handle token errors?
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (scope, tail, _, end) = parse_scope(&tokens, None)?;
    if !tail.is_empty() {
        eprintln!("Unexpected end of scope at {}", end);
        return None;
    }
    Some(scope)
}

#[cfg(test)]
pub(crate) fn parse_statement_from_string(str: &str) -> Option<Statement> {
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (parse_result, tail) = parse_statement(&tokens, None)?;
    if let Some(first) = tail.first() {
        eprintln!("extra tokens found after the end of statement at {}", first.start);
    }
    match parse_result {
        ParseResult::Statement(x) => Some(x),
        ParseResult::Nop{..} => None,
        ParseResult::ExitScope{..} => None,
    }
}

pub(crate) fn parse_scope(mut tail: &[Token], open_location: Option<Location>) -> Option<(Scope, &[Token], Location, Location)> {
    let is_top_scope = open_location.is_none();
    let start_loc = open_location.unwrap_or(Location { row: 0, col: 0 });
    let mut end_loc = start_loc;
    let mut statements = vec![];
    let mut is_closed = false;

    while !tail.is_empty() {
        let (parse_result, tail2) = parse_statement(tail, None)?;
        tail = tail2;
        match parse_result {
            ParseResult::Statement(stmt) => { 
                end_loc = stmt.end;
                statements.push(stmt);
            },
            ParseResult::Nop{end, ..} => {
                end_loc = end;
            }
            ParseResult::ExitScope(end) => {
                end_loc = end;
                is_closed = true;
                break;
            } 
        }
    }
    if is_top_scope && is_closed {
        eprintln!("Unexpected end of scope at {end_loc}");
        return None;
    } else if !is_top_scope && !is_closed {
        eprintln!("Scope that starts at {start_loc} is not closed");
        return None;
    }
    let scope = Scope{ statements };
    Some((scope, tail, start_loc, end_loc))
}

fn parse_statement(tail: &[Token], prev_end: Option<Location>) -> Option<(ParseResult, &[Token])> {
    let orig_tail = tail;
    let Some((head, tail)) = tail.split_first() else {
        match prev_end {
            None => eprintln!("empty input"),
            Some(loc) => eprintln!("Unexpectedly reached the end of the token stream at {loc}, expected statement"),
        }
        return None;
    };


    let start = head.start;
    match head.kind {
        TokenKind::SEMICOLON => Some((ParseResult::Nop{start, end:start}, tail)),
        TokenKind::PRINT => {
            let (expr, tail) = parse_expression(tail, Some(head.end))?;
            let (tail, end) = check_statement_terminated(tail, expr.end)?;
            let stmt = Statement { body: StatementBody::Print(expr), start, end };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::NUMBER | TokenKind::STRING | TokenKind::IDENTIFIER
            | TokenKind::NIL | TokenKind::TRUE | TokenKind::FALSE
            | TokenKind::LEFT_PAREN | TokenKind::MINUS | TokenKind::BANG => {
            let (expr, tail) = parse_expression(orig_tail, Some(head.end))?;
            let (tail, end) = check_statement_terminated(tail, expr.end)?;
            let stmt = Statement { body: StatementBody::Expression(expr), start, end };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::VAR => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, head.end)?;
            let Some((next, tail)) = tail.split_first() else {
                eprintln!("Unexpected end of token stream, expected = or ; at {}", name.end);
                return None;
            };
            let (expr, tail, end) = match next.kind {
                TokenKind::EQUAL => {
                    let (expr, tail) = parse_expression(tail, Some(next.end))?;
                    let (tail, end) = check_statement_terminated(tail, expr.end)?;
                    (expr, tail, end)
                }
                TokenKind::SEMICOLON => {
                    let expr = Expression{
                        body: ExpressionBody::Literal(Value::Nil),
                        start: next.start,
                        end: next.end,
                    };
                    (expr, tail, next.end)
                }
                _ => {
                    eprintln!("Expected = or ;, found {next} at {}", next.start);
                    return None;
                },
            };

            let body = StatementBody::VariableDeclaration {
                name: name.code.clone(), // todo: check if we can remove copying here
                value: expr,
            };
            let stmt = Statement { body, start, end };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::LEFT_BRACE => {
            let (scope, tail, start, end) = parse_scope(tail, Some(head.start))?;
            if scope.statements.is_empty() {
                return Some((ParseResult::Nop{start, end}, tail));
            }
            let body = StatementBody::Scope(scope);
            let stmt = Statement { body, start, end };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::RIGHT_BRACE => {
            Some((ParseResult::ExitScope(head.end), tail))
        },
        TokenKind::IF => {
            let _ = expect_token_kind(tail, TokenKind::LEFT_PAREN, head.end)?;
            let (condition, tail) = parse_expression(tail, Some(head.end))?;
            let (body, tail, if_end) = parse_control_flow_body(tail, condition.end)?;
            let (els, tail) = check_token_kind(tail, TokenKind::ELSE);
            let (else_body, tail, end) = if let Some(els) = els {
                parse_control_flow_body(tail, els.end)?
            } else {
                (None, tail, if_end)
            };
            let if_body = StatementBody::If {
                condition,
                body: body.map(|x| Box::new(x)),
                else_body: else_body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: if_body, start, end};
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::WHILE => {
            let _ = expect_token_kind(tail, TokenKind::LEFT_PAREN, head.end)?;
            let (condition, tail) = parse_expression(tail, Some(head.end))?;
            let (body, tail, end) = parse_control_flow_body(tail, condition.end)?;
            let while_body = StatementBody::While {
                condition,
                body: body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: while_body, start, end};
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::FOR => {
            let (paren, tail) = expect_token_kind(tail, TokenKind::LEFT_PAREN, head.end)?;
            let (init, tail, init_end) = parse_actual_statement(tail, paren.end)?;
            if let Some(init_stmt) = init.as_ref() {
                let is_allowed = match init_stmt.body {
                    StatementBody::Print(_)
                        | StatementBody::Expression(_)
                        | StatementBody::VariableDeclaration { .. } => true,
                    _ => false,
                };
                if !is_allowed {
                    eprintln!("This type of statement is not allowed at the init section of for loop at {}", init_stmt.start);
                    return None;
                }
            }
            let (semi, tail) = check_token_kind(tail, TokenKind::SEMICOLON);
            let (condition, tail, cond_end) = if let Some(semi) = semi {
                (Expression{ body: ExpressionBody::Literal(Value::Bool(true)), start: semi.start, end: semi.end }, tail, semi.end)
            } else {
                let (condition, tail) = parse_expression(tail, Some(init_end))?;
                let (semi, tail) = expect_token_kind(tail, TokenKind::SEMICOLON, condition.end)?;
                (condition, tail, semi.end)
            };

            let (close, tail) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
            let (increment, tail, incr_end) = if let Some(close) = close {
                (None, tail, close.end)
            } else {
                let (increment, tail) = parse_expression(tail, Some(cond_end))?;
                let (close, tail) = expect_token_kind(tail, TokenKind::RIGHT_PAREN, increment.end)?;
                (Some(increment), tail, close.end)
            };
            let (body, tail, end) = parse_control_flow_body(tail, incr_end)?;

            let for_body = StatementBody::For {
                init: init.map(|x| Box::new(x)),
                condition,
                increment,
                body: body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: for_body, start, end};

            // added a scope to make sure that variables that are declared in the init part stay in this scope
            // could be resolved at the evaluation stage, but this way was more simple
            let wrapper_scope = Scope {
                statements: vec![stmt],
            };
            let wrapper_scope = Statement{body: StatementBody::Scope(wrapper_scope), start, end};

            Some((ParseResult::Statement(wrapper_scope), tail))
        },
        TokenKind::FUN => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, head.end)?;
            let (paren, tail) = expect_token_kind(tail, TokenKind::LEFT_PAREN, name.end)?;
            let name = name.code.clone(); // todo: check if we can remove copying here
            let (args, tail, last_token) = parse_call_args(tail, &name, paren.end)?;
            let (brace, tail) = expect_token_kind(tail, TokenKind::LEFT_BRACE, last_token.end)?;
            let (scope, tail, _, end) = parse_scope(tail, Some(brace.start))?;
            let func = FunctionValue{
                name,
                args,
                body: scope,
                loc: start,
            };
            let body = StatementBody::FunctionDeclaration(Rc::new(func).into());
            let stmt = Statement{body, start, end};
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::RETURN => {
            let (semi, tail) = check_token_kind(tail, TokenKind::SEMICOLON);
            let (expr, tail, end) = if let Some(semi) = semi {
                let expr = Expression { body: ExpressionBody::Literal(Value::Nil), start: semi.start, end: semi.end };
                (expr, tail, semi.end)
            } else {
                let (expr, tail) = parse_expression(tail, Some(head.end))?;
                let (tail, end) = check_statement_terminated(tail, expr.end)?;
                (expr, tail, end)
            };
            let stmt = Statement { body: StatementBody::Return(expr), start, end };
            Some((ParseResult::Statement(stmt), tail))
        },
        _ => {
            eprintln!("Unexpected token {head} at {}, expected a start of a statement", head.start);
            None
        },
    }
}

fn parse_control_flow_body(tail: &[Token], prev_end: Location) -> Option<(Option<Statement>, &[Token], Location)> {
    let (stmt, tail, end) = parse_actual_statement(tail, prev_end)?;
    let stmt = match stmt {
        Some(x) => {
            if matches!(x.body, StatementBody::VariableDeclaration {..}) {
                eprintln!("Variable declaration is not allowed as a body of control flow structures, found at {}", x.start);
                return None;
            }
            Some(x)
        }
        None => None,
    };
    Some((stmt, tail, end))
}

fn parse_actual_statement(tail: &[Token], prev_end: Location) -> Option<(Option<Statement>, &[Token], Location)> {
    let (parse_result, tail) = parse_statement(tail, Some(prev_end))?;
    match parse_result {
        ParseResult::Statement(stmt) => {
            let end = stmt.end;
            Some((Some(stmt), tail, end))
        },
        ParseResult::Nop{end, ..} => Some((None, tail, end)),
        ParseResult::ExitScope(loc) => {
            eprintln!("Got closing brace when expecting statement at {loc}");
            None
        }
    }
}

fn check_statement_terminated(tail: &[Token], start_loc: Location) -> Option<(&[Token], Location)> {
    let (token, tail) = expect_token_kind(tail, TokenKind::SEMICOLON, start_loc)?;
    Some((tail, token.end))
}

fn parse_call_args<'a, 'b>(mut tail: &'a [Token], func_name: &'b str, mut prev_end: Location) -> Option<(Vec<String>, &'a [Token], &'a Token)> {
    let mut args = vec![];
    loop {
        let (paren, tail2) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
        tail = tail2;
        if let Some(paren) = paren {
            return Some((args, tail, paren));
        }
        let (arg, tail2) = expect_token_kind(tail, TokenKind::IDENTIFIER, prev_end)?;
        tail = tail2;
        let arg_name = arg.code.clone(); // todo: check if it's possible not to clone this
        if arg_name == func_name {
            eprintln!("argument name can not be the same as function name at {}", arg.start);
            return None;
        }
        args.push(arg_name);
        let Some((next, tail2)) = tail.split_first() else {
            eprintln!("Unexpected end of token stream, expected ) or , at {}", arg.end);
            return None;
        };
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_nops() {
        assert!(parse_statement_list_from_string(";;;;;").unwrap().statements.is_empty());
        assert!(parse_statement_list_from_string("").unwrap().statements.is_empty());
    }

    #[test]
    fn test_print() {
        assert_eq!("print (+ 1.0 1.0);", parse_statement_from_string("print 1+1;").unwrap().to_string());
        assert!(parse_statement_from_string("print 1+1").is_none());
        assert!(parse_statement_from_string("print 1+;").is_none());
        assert!(parse_statement_from_string("print (;").is_none());
        assert!(parse_statement_from_string("print ;").is_none());
    }

    #[test]
    fn test_var() {
        assert_eq!("var x = (+ var(y) 1.0);", parse_statement_from_string("var x = y+1;").unwrap().to_string());
        assert!(parse_statement_from_string("var x = y+1").is_none());
        assert!(parse_statement_from_string("var x = y+;").is_none());
        assert!(parse_statement_from_string("var x = (;").is_none());
        assert!(parse_statement_from_string("var x = ;").is_none());
        assert!(parse_statement_from_string("var x y+1;").is_none());
        assert_eq!("(= var(x) (+ var(y) 1.0));", parse_statement_from_string("x = y+1;").unwrap().to_string());
        assert!(parse_statement_from_string("var = y+1;").is_none());
        assert_eq!("var x = nil;", parse_statement_from_string("var x;").unwrap().to_string());
    }

    #[test]
    fn test_scopes() {
        let statements = "{var a = 1;}";
        let expected = "{ { var a = 1.0; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
        assert!(parse_statement_list_from_string("{var a = 1;}}").is_none());
        assert!(parse_statement_list_from_string("{var a = 1;").is_none());
        
        let statements = "var a = 1; {var a = a + 2; print a;} print a;";
        let expected = "{ var a = 1.0; { var a = (+ var(a) 2.0); print var(a); } print var(a); }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
    }

    #[test]
    fn test_if() {
        let statements = "if (true) print 1;";
        let expected = "{ if (group true) {print 1.0;} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) {print 1; print 2;}";
        let expected = "{ if (group true) {{ print 1.0; print 2.0; }} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) print 1; print 2;";
        let expected = "{ if (group true) {print 1.0;} print 2.0; }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true);";
        let expected = "{ if (group true); }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        assert!(parse_statement_list_from_string("if true print 1;").is_none());

        let statements = "if (true) print 1; else print 2;";
        let expected = "{ if (group true) {print 1.0;} else {print 2.0;} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) print 1; else {print 2; print 3;}";
        let expected = "{ if (group true) {print 1.0;} else {{ print 2.0; print 3.0; }} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) print 1; else print 2; print 3;";
        let expected = "{ if (group true) {print 1.0;} else {print 2.0;} print 3.0; }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) print 1; else;";
        let expected = "{ if (group true) {print 1.0;} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) if (false) print 1; else print 2;";
        let expected = "{ if (group true) {if (group false) {print 1.0;} else {print 2.0;}} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "if (true) {if (false) print 1;} else print 2;";
        let expected = "{ if (group true) {{ if (group false) {print 1.0;} }} else {print 2.0;} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
    }

    #[test]
    fn test_while() {
        let statements = "while (true) print 1;";
        let expected = "{ while (group true) {print 1.0;} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "while (true) {print 1; print 2;}";
        let expected = "{ while (group true) {{ print 1.0; print 2.0; }} }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "while (true) print 1; print 2;";
        let expected = "{ while (group true) {print 1.0;} print 2.0; }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "while (true);";
        let expected = "{ while (group true); }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        assert!(parse_statement_list_from_string("while true print 1;").is_none());
    }

    #[test]
    fn test_for() {
        let statements = "for (;;);";
        let expected = "{ { for (; true;); } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "for (var a = 1; a < 10; a = a + 1) {print a; print 1;}";
        let expected = "{ { for (var a = 1.0; (< var(a) 10.0); (= var(a) (+ var(a) 1.0))) {{ print var(a); print 1.0; }} } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        let statements = "for (var a = 1; a < 10; a = a + 1) print a; print 1;";
        let expected = "{ { for (var a = 1.0; (< var(a) 10.0); (= var(a) (+ var(a) 1.0))) {print var(a);} } print 1.0; }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        assert!(parse_statement_list_from_string("for ({var a = 1;};;);").is_none());
        assert!(parse_statement_list_from_string("for (if (true) print 1.0;;);").is_none());
        assert!(parse_statement_list_from_string("for (;{false};);").is_none());
        assert!(parse_statement_list_from_string("for (var a = 1;;{a = a + 1});").is_none());
        assert!(parse_statement_list_from_string("for (;;) var a;").is_none());
        assert!(parse_statement_list_from_string("for (;);").is_none());
        assert!(parse_statement_list_from_string("for (;;;);").is_none());
    }

    #[test]
    fn test_function_declaration() {
        let statements = "fun foo() {}";
        let expected = "{ fun foo() { } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
        
        let statements = "fun foo(a, b, c) {print 1.0;}";
        let expected = "{ fun foo(a, b, c, ) { print 1.0; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
        
        let statements = "fun foo(a, b, c,) {print 1.0;}";
        let expected = "{ fun foo(a, b, c, ) { print 1.0; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());

        assert!(parse_statement_list_from_string("fun () {}").is_none());
        assert!(parse_statement_list_from_string("fun foo(foo) {}").is_none());
        assert!(parse_statement_list_from_string("fun 123() {}").is_none());
        assert!(parse_statement_list_from_string("fun foo(123) {}").is_none());
        assert!(parse_statement_list_from_string("fun foo()").is_none());
        assert!(parse_statement_list_from_string("fun foo();").is_none());
        assert!(parse_statement_list_from_string("fun foo() print 1;").is_none());
        assert!(parse_statement_list_from_string("fun foo() 1").is_none());
        assert!(parse_statement_list_from_string("fun foo() {").is_none());
        assert!(parse_statement_list_from_string("fun foo( {}").is_none());
        
        let statements = "fun foo() {return 1;}";
        let expected = "{ fun foo() { return 1.0; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
        
        let statements = "fun foo() {return;}";
        let expected = "{ fun foo() { return nil; } }";
        assert_eq!(expected, parse_statement_list_from_string(statements).unwrap().to_string());
    }
}
