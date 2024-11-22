use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Value {
    Number(f64),
    String(Rc<String>), // todo: consider using CoW?
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

#[cfg(test)]
mod test {
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
    }
}