use crate::ast::Statement;
use crate::environment::Environment;
use std::fmt;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
    Function(Vec<String>, Box<Statement>, Environment),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(obj) => write!(f, "{}", *obj),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
            Object::Function(params, block, ..) => {
                let mut out = String::from("fn (");

                for p in params {
                    out = format!("{}{}, ", out, p);
                }

                write!(f, "{}){{\n{}\n}}", &out[0..out.len() - 2], block)
            }
        }
    }
}
