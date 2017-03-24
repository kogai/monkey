use std::fmt::{Display, Formatter, Result};
use std::collections::HashMap;

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
    Error(String),
}

impl ObjectType {
    pub fn to_type(&self) -> i32 {
        match self {
            &ObjectType::Integer(_) => 0,
            &ObjectType::Boolean(_) => 1,
            &ObjectType::Null(_) => 2,
            &ObjectType::Return(_) => 3,
            &ObjectType::Error(_) => 4,
        }
    }
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
            ObjectType::Error(ref x) => format!("Error: {}", x),
        }
    }

    pub fn new_i32(x: i32) -> Self {
        Object { object_type: ObjectType::Integer(x) }
    }

    pub fn new_return_value(x: Self) -> Self {
        Object { object_type: ObjectType::Return(Box::new(x)) }
    }

    pub fn new_error(x: String) -> Self {
        Object { object_type: ObjectType::Error(x) }
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

    pub fn to_error_message(&self) -> Option<String> {
        match self.object_type {
            ObjectType::Error(ref x) => Some(x.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Enviroment {
    store: HashMap<String, Object>,
}

impl Enviroment {
    pub fn new() -> Self {
        Enviroment { store: HashMap::new() }
    }

    pub fn get(&self, key: &String) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
}

