use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Null;

impl Display for Null {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Null {{}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectType {
    Integer(i32),
    Boolean(bool),
    Null(Null),
    Return(Box<Object>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object {
    pub object_type: ObjectType,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self.object_type {
            ObjectType::Integer(ref x) => format!("{}", x),
            ObjectType::Boolean(ref x) => format!("{}", x),
            ObjectType::Null(ref x) => format!("{}", x),
            ObjectType::Return(ref x) => format!("{:?}", x),
        }
    }

    pub fn new_i32(x: i32) -> Self {
        Object { object_type: ObjectType::Integer(x) }
    }

    pub fn new_return_value(x: Self) -> Self {
        Object { object_type: ObjectType::Return(Box::new(x)) }
    }

    pub fn to_i32(&self) -> Option<i32> {
        match self.object_type {
            ObjectType::Integer(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self.object_type {
            ObjectType::Boolean(ref x) => Some(x.clone()),
            _ => None,
        }
    }
}

