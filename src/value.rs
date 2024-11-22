use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::parser_statements::Scope;
use crate::tokenizer::Location;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Value {
    Number(f64),
    String(Rc<String>), // todo: consider using CoW?
    Bool(bool),
    Nil,
    NativeFunction(&'static str),
    UserFunction(FunctionRef),
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => f.write_fmt(format_args!("{n}")),
            Value::String(s) => f.write_str(s),
            Value::Bool(b) => f.write_fmt(format_args!("{b}")),
            Value::Nil => f.write_str("nil"),
            Value::NativeFunction(name) => f.write_fmt(format_args!("<native fn {name}>")),
            Value::UserFunction(func) => f.write_fmt(format_args!("<fn {}>", func.inner.name))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Declaration {
    NativeFunction(&'static str),
    UserFunction(FunctionRef),
}
impl Declaration {
    pub(crate) fn print_location(&self) -> String {
        match self {
            Declaration::NativeFunction(_) => "this is a native function".to_string(),
            Declaration::UserFunction(x) => format!("function declared at {}", x.inner.loc),
        }
    }
}
impl From<Declaration> for Value {
    fn from(value: Declaration) -> Self {
        match value {
            Declaration::NativeFunction(x) => Self::NativeFunction(x),
            Declaration::UserFunction(x) => Self::UserFunction(x),
        }
    }
}

#[derive(Debug)]
pub(crate) struct FunctionValue {
    pub(crate) name: String,
    pub(crate) args: Vec<String>,
    pub(crate) body: Scope,
    pub(crate) loc: Location,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionRef {
    pub(crate) inner: Rc<FunctionValue>,
}
impl From<Rc<FunctionValue>> for FunctionRef {
    fn from(value: Rc<FunctionValue>) -> Self {
        Self{inner: value}
    }
}
impl PartialEq for FunctionRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

#[cfg(test)]
mod test {
    use crate::parser_statements::{parse_statement_from_string, StatementBody};
    use super::*;

    #[test]
    fn test_clone_and_eq() {
        let str = Value::String(Rc::new("test".to_string()));
        let clone = str.clone();
        let other = Value::String(Rc::new("test".to_string()));
        assert_eq!(str, clone);
        assert_eq!(str, other);
        match (str, clone, other) {
            (Value::String(str), Value::String(clone), Value::String(other)) => {
                assert!(Rc::ptr_eq(&str, &clone));
                assert!(!Rc::ptr_eq(&str, &other));
            },
            _ => unreachable!(),
        }

        let str = Value::NativeFunction("test");
        let clone = str.clone();
        let other = Value::NativeFunction("test");
        assert_eq!(str, clone);
        assert_eq!(str, other);
        
        let code = "fun foo() {}";
        let func = func_value_from_string(code);
        let clone = func.clone();
        let other = func_value_from_string(code);
        assert_eq!(func, clone);
        assert_ne!(func, other);
        match (func, clone) {
            (Value::UserFunction(func), Value::UserFunction(clone)) => {
                assert!(Rc::ptr_eq(&func.inner, &clone.inner));
            },
            _ => unreachable!(),
        }
    }

    fn func_value_from_string(code: &str) -> Value {
        let parsed = parse_statement_from_string(code).unwrap();
        let func = match parsed.body {
            StatementBody::FunctionDeclaration(x) => x,
            _ => unreachable!(),
        };
        Value::UserFunction(func)
    }
}
