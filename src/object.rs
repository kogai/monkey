use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone)]
pub struct Null;

impl Display for Null {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Null {{}}")
    }
}

#[derive(Debug, Clone)]
pub enum ObjectType {
    Integer(usize),
    Boolean(bool),
    Null(Null),
}

#[derive(Debug, Clone)]
pub struct Object {
    pub object_type: ObjectType,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self.object_type {
            ObjectType::Integer(ref x) => format!("{}", x),
            ObjectType::Boolean(ref x) => format!("{}", x),
            ObjectType::Null(ref x) => format!("{}", x),
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
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

    pub fn to_null(&self) -> Option<Null> {
        match self.object_type {
            ObjectType::Null(ref x) => Some(x.clone()),
            _ => None,
        }
    }
}

