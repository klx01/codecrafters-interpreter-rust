use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => f.write_fmt(format_args!("{n}")),
            Value::String(s) => f.write_str(s),
            Value::Bool(b) => f.write_fmt(format_args!("{b}")),
            Value::Nil => f.write_str("nil"),
        }
    }
}
