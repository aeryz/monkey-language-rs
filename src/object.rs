use std::fmt;
use std::rc::Rc;

pub enum ObjectType {
    Boolean,
    Integer,
}

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Rc<Object>),
    Error(String),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(obj) => write!(f, "{}", *obj),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
        }
    }
}
