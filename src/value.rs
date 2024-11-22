use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => f.write_fmt(format_args!("{n}")),
            Literal::String(s) => f.write_str(s),
            Literal::Bool(b) => f.write_fmt(format_args!("{b}")),
            Literal::Nil => f.write_str("nil"),
        }
    }
}
