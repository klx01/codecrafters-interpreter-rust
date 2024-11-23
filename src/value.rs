use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::ptr;
use std::rc::Rc;
use crate::memory::MemoryScope;
use crate::parser_statements::Scope;
use crate::tokenizer::Location;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Value<'a> {
    Number(f64),
    String(Cow<'a, str>),
    Bool(bool),
    Nil,
    NativeFunction(&'static str),
    UserFunction(FunctionRef<'a>),
}
impl<'a> Display for Value<'a> {
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
pub(crate) enum Declaration<'a> {
    NativeFunction(&'static str),
    UserFunction(FunctionRef<'a>),
}
impl<'a> Declaration<'a> {
    pub(crate) fn print_location(&self) -> String {
        match self {
            Declaration::NativeFunction(_) => "this is a native function".to_string(),
            Declaration::UserFunction(x) => format!("function declared at {}", x.inner.loc),
        }
    }
}
impl<'a> From<Declaration<'a>> for Value<'a> {
    fn from(value: Declaration<'a>) -> Self {
        match value {
            Declaration::NativeFunction(x) => Self::NativeFunction(x),
            Declaration::UserFunction(x) => Self::UserFunction(x),
        }
    }
}

#[derive(Debug)]
pub(crate) struct FunctionValue<'a> {
    pub(crate) name: String,
    pub(crate) args: Vec<String>,
    pub(crate) body: Scope<'a>,
    pub(crate) loc: Location,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionRef<'a> {
    pub(crate) inner: &'a FunctionValue<'a>,
    pub(crate) captures: Option<Rc<RefCell<MemoryScope<'a>>>>,
}
impl<'a> FunctionRef<'a> {
    pub(crate) fn new(inner: &'a FunctionValue<'a>, captures: Option<Rc<RefCell<MemoryScope<'a>>>>) -> Self {
        Self{
            inner,
            captures
        }
    }
}
impl<'a> PartialEq for FunctionRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        if !ptr::eq(self.inner, other.inner) {
            return false;
        }
        match (&self.captures, &other.captures) {
            (None, None) => true,
            (Some(left), Some(right)) => Rc::ptr_eq(left, right),
            _ => false,
        }
    }
}
