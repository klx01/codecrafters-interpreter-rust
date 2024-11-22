use std::fmt::{Display, Formatter, Write};
use crate::parser_expressions::{parse_expression, Expression, ExpressionBody, expect_token_kind, check_token_kind};
use crate::tokenizer::{tokenize_string_no_eof, Location, Token, TokenKind};
use crate::value::Value;

#[derive(Debug)]
pub(crate) enum StatementBody {
    Print(Expression),
    Expression(Expression),
    VariableDeclaration{name: String, value: Expression},
    Scope(Scope),
    If{condition: Expression, body: Option<Box<Statement>>, else_body: Option<Box<Statement>>},
    While{condition: Expression, body: Option<Box<Statement>>},
    For{init: Option<Box<Statement>>, condition: Expression, increment: Option<Expression>, body: Option<Box<Statement>>},
    FunctionDeclaration{name: String, args: Vec<String>, body: Scope},
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
            StatementBody::FunctionDeclaration { name, args, body } => {
                f.write_fmt(format_args!("fun {name}("))?;
                for arg in args {
                    f.write_fmt(format_args!("{arg}, "))?;
                }
                f.write_fmt(format_args!(") {body}"))
            },
        }
    }
}

#[derive(Debug)]
pub(crate) struct Statement {
    pub body: StatementBody,
    pub loc: Location,
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.body))
    }
}

enum ParseResult {
    Statement(Statement),
    Nop,
    ExitScope,
}

#[derive(Debug)]
pub(crate) struct Scope {
    pub statements: Vec<Statement>,
    pub start: Location,
    pub end: Location,
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
    let (scope, tail) = parse_scope(&tokens, true)?;
    assert!(tail.is_empty(), "tail for top scope should always be empty");
    if !tail.is_empty() {
        eprintln!("Unexpected end of scope at {}", scope.end);
        return None;
    }
    Some(scope)
}

#[cfg(test)]
pub(crate) fn parse_statement_from_string(str: &str) -> Option<Statement> {
    let (tokens, _has_errors) = tokenize_string_no_eof(str);
    let (parse_result, tail) = parse_statement(&tokens)?;
    if !tail.is_empty() {
        eprintln!("extra tokens found after the end of statement");
    }
    match parse_result {
        ParseResult::Statement(x) => Some(x),
        ParseResult::Nop => None,
        ParseResult::ExitScope => None,
    }
}

pub(crate) fn parse_scope(mut tail: &[Token], is_top_scope: bool) -> Option<(Scope, &[Token])> {
    let start_loc = if let Some(first) = tail.first() {
        first.loc
    } else {
        Location { row: 0, col: 0 }
    };
    let mut end_loc = start_loc;
    let mut statements = vec![];
    let mut is_closed = false;

    while !tail.is_empty() {
        end_loc = tail.first().unwrap().loc;
        let (parse_result, tail2) = parse_statement(tail)?;
        tail = tail2;
        match parse_result {
            ParseResult::Statement(stmt) => statements.push(stmt),
            ParseResult::Nop => {}
            ParseResult::ExitScope => {
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
    let scope = Scope{ statements, start: start_loc, end: end_loc };
    Some((scope, tail))
}

fn parse_statement(tail: &[Token]) -> Option<(ParseResult, &[Token])> {
    let orig_tail = tail;
    let Some((head, tail)) = tail.split_first() else {
        eprintln!("Unexpected end of token stream, expected start of a statement");
        return None;
    };
    let loc = head.loc; // todo: fix locations, use actual locations instead of head location
    match head.kind {
        TokenKind::SEMICOLON => Some((ParseResult::Nop, tail)),
        TokenKind::PRINT => {
            let (expr, tail) = parse_expression(tail, Some(head))?;
            let tail = check_statement_terminated(tail, loc)?;
            let stmt = Statement { body: StatementBody::Print(expr), loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::NUMBER | TokenKind::STRING | TokenKind::IDENTIFIER
            | TokenKind::NIL | TokenKind::TRUE | TokenKind::FALSE
            | TokenKind::LEFT_PAREN | TokenKind::MINUS | TokenKind::BANG => {
            let (expr, tail) = parse_expression(orig_tail, Some(head))?;
            let tail = check_statement_terminated(tail, loc)?;
            let stmt = Statement { body: StatementBody::Expression(expr), loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::VAR => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, loc)?;
            let Some((next, tail)) = tail.split_first() else {
                eprintln!("Unexpected end of token stream, expected = or ; in variable declaration after {}", name.loc);
                return None;
            };
            let (expr, tail) = match next.kind {
                TokenKind::EQUAL => {
                    let (expr, tail) = parse_expression(tail, Some(next))?;
                    let tail = check_statement_terminated(tail, loc)?;
                    (expr, tail)
                }
                TokenKind::SEMICOLON => {
                    let expr = Expression{
                        body: ExpressionBody::Literal(Value::Nil),
                        loc: next.loc,
                    };
                    (expr, tail)
                }
                _ => {
                    eprintln!("Expected = or ;, found {next} at {}", next.loc);
                    return None;
                },
            };

            let body = StatementBody::VariableDeclaration {
                name: name.code.clone(), // todo: check if we can remove copying here
                value: expr,
            };
            let stmt = Statement { body, loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::LEFT_BRACE => {
            let (scope, tail) = parse_scope(tail, false)?;
            if scope.statements.is_empty() {
                return Some((ParseResult::Nop, tail));
            }
            let body = StatementBody::Scope(scope);
            let stmt = Statement { body, loc };
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::RIGHT_BRACE => {
            Some((ParseResult::ExitScope, tail))
        },
        TokenKind::IF => {
            let _ = expect_token_kind(tail, TokenKind::LEFT_PAREN, loc)?;
            let (condition, tail) = parse_expression(tail, Some(head))?;
            let (body, tail) = parse_control_flow_body(tail)?;
            let (els, tail) = check_token_kind(tail, TokenKind::ELSE);
            let (else_body, tail) = if els.is_some() {
                parse_control_flow_body(tail)?
            } else {
                (None, tail)
            };
            let if_body = StatementBody::If {
                condition,
                body: body.map(|x| Box::new(x)),
                else_body: else_body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: if_body, loc};
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::WHILE => {
            let _ = expect_token_kind(tail, TokenKind::LEFT_PAREN, loc)?;
            let (condition, tail) = parse_expression(tail, Some(head))?;
            let (body, tail) = parse_control_flow_body(tail)?;
            let while_body = StatementBody::While {
                condition,
                body: body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: while_body, loc};
            Some((ParseResult::Statement(stmt), tail))
        },
        TokenKind::FOR => {
            let (_, tail) = expect_token_kind(tail, TokenKind::LEFT_PAREN, loc)?;
            let (init, tail) = parse_actual_statement(tail)?;
            if let Some(init_stmt) = init.as_ref() {
                let is_allowed = match init_stmt.body {
                    StatementBody::Print(_)
                        | StatementBody::Expression(_)
                        | StatementBody::VariableDeclaration { .. } => true,
                    _ => false,
                };
                if !is_allowed {
                    eprintln!("This type of statement is not allowed at the init section of for loop at {}", init_stmt.loc);
                    return None;
                }
            }
            let (semi, tail) = check_token_kind(tail, TokenKind::SEMICOLON);
            let (condition, tail) = if semi.is_none() {
                let (condition, tail) = parse_expression(tail, Some(head))?;
                let (_, tail) = expect_token_kind(tail, TokenKind::SEMICOLON, loc)?;
                (condition, tail)
            } else {
                (Expression{ body: ExpressionBody::Literal(Value::Bool(true)), loc }, tail)
            };

            let (close, tail) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
            let (increment, tail) = if close.is_none() {
                let (increment, tail) = parse_expression(tail, Some(head))?;
                let (_, tail) = expect_token_kind(tail, TokenKind::RIGHT_PAREN, loc)?;
                (Some(increment), tail)
            } else {
                (None, tail)
            };
            let (body, tail) = parse_control_flow_body(tail)?;

            let for_body = StatementBody::For {
                init: init.map(|x| Box::new(x)),
                condition,
                increment,
                body: body.map(|x| Box::new(x)),
            };
            let stmt = Statement{body: for_body, loc};

            // added a scope to make sure that variables that are declared in the init part stay in this scope
            // could be resolved at the evaluation stage, but this way was more simple
            let wrapper_scope = Scope {
                statements: vec![stmt],
                start: loc,
                end: Location { row: 0, col: 0 }, // todo: set the actual end location
            };
            let wrapper_scope = Statement{body: StatementBody::Scope(wrapper_scope), loc};

            Some((ParseResult::Statement(wrapper_scope), tail))
        },
        TokenKind::FUN => {
            let (name, tail) = expect_token_kind(tail, TokenKind::IDENTIFIER, loc)?;
            let (_, tail) = expect_token_kind(tail, TokenKind::LEFT_PAREN, loc)?;
            let (args, tail) = parse_call_args(tail, loc)?;
            let (_, tail) = expect_token_kind(tail, TokenKind::LEFT_BRACE, loc)?;
            let (scope, tail) = parse_scope(tail, false)?;
            let body = StatementBody::FunctionDeclaration {
                name: name.code.clone(), // todo: check if we can remove copying here
                args,
                body: scope,
            };
            let stmt = Statement{body, loc};
            Some((ParseResult::Statement(stmt), tail))
        },
        _ => {
            eprintln!("Unexpected token {head} at {loc}, expected a start of a statement");
            None
        },
    }
}

fn parse_control_flow_body(tail: &[Token]) -> Option<(Option<Statement>, &[Token])> {
    let (stmt, tail) = parse_actual_statement(tail)?;
    let stmt = match stmt {
        Some(x) => {
            if matches!(x.body, StatementBody::VariableDeclaration {..}) {
                eprintln!("Variable declaration is not allowed as a body of control flow structures for some reason at {}", x.loc);
                return None;
            }
            Some(x)
        }
        None => None,
    };
    Some((stmt, tail))
}

fn parse_actual_statement(tail: &[Token]) -> Option<(Option<Statement>, &[Token])> {
    let tail_orig = tail;
    let (parse_result, tail) = parse_statement(tail)?;
    match parse_result {
        ParseResult::Statement(stmt) => Some((Some(stmt), tail)),
        ParseResult::Nop => Some((None, tail)),
        ParseResult::ExitScope => {
            let loc = tail_orig.first().unwrap().loc;
            eprintln!("Got closing brace when expecting statement at {loc}");
            None
        }
    }
}

fn check_statement_terminated(tail: &[Token], start_loc: Location) -> Option<&[Token]> {
    let (_, tail) = expect_token_kind(tail, TokenKind::SEMICOLON, start_loc)?;
    Some(tail)
}

fn parse_call_args(mut tail: &[Token], loc: Location) -> Option<(Vec<String>, &[Token])> {
    let mut args = vec![];
    loop {
        let (paren, tail2) = check_token_kind(tail, TokenKind::RIGHT_PAREN);
        tail = tail2;
        if paren.is_some() {
            return Some((args, tail));
        }
        let (arg, tail2) = expect_token_kind(tail, TokenKind::IDENTIFIER, loc)?; // todo: fix location
        tail = tail2;
        args.push(arg.code.clone()); // todo: check if it's possible not to clone this
        let Some((next, tail2)) = tail.split_first() else {
            eprintln!("Unexpected end of token stream, expected ) or , in arguments list for a function declaration at {}", loc); // todo: fix location
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
        assert!(parse_statement_list_from_string("fun 123() {}").is_none());
        assert!(parse_statement_list_from_string("fun foo(123) {}").is_none());
        assert!(parse_statement_list_from_string("fun foo()").is_none());
        assert!(parse_statement_list_from_string("fun foo();").is_none());
        assert!(parse_statement_list_from_string("fun foo() print 1.0;").is_none());
        assert!(parse_statement_list_from_string("fun foo() {").is_none());
        assert!(parse_statement_list_from_string("fun foo( {}").is_none());
    }
}
