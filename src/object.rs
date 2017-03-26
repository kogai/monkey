use std::fmt::{Display, Formatter, Result};
use std::collections::HashMap;
use ast::{Identifier, BlockStatement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Null;

impl Display for Null {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Null {{}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Enviroment,
}

pub trait BuildInFunction {
    fn call(&self, Vec<Object>) -> Object;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Len;

impl BuildInFunction for Len {
    fn call(&self, xs: Vec<Object>) -> Object {
        if xs.len() != 1 {
            return Object::new_error(format!("wrong number of arguments. got {} want=1", xs.len()));
        }
        match xs.first() {
            Some(x) => {
                if let ObjectType::StringType(ref s) = x.object_type {
                    return Object::new_i32(s.len() as i32);
                }
                Object::new_error(format!("argument to \"len\" not supported. got {:?}",
                                          x.object_type))
            }
            None => {
                Object::new_error(format!("wrong number of arguments. got {} want=1", xs.len()))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildIn {
    Len(Len),
}

impl BuildIn {
    pub fn set_from_string(function_name: &String) -> Option<Object> {
        match function_name.as_str() {
            "len" => Some(Object { object_type: ObjectType::BuildIn(BuildIn::Len(Len)) }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectType {
    Integer(i32),
    StringType(String),
    Boolean(bool),
    Null(Null),
    Return(Box<Object>),
    Error(String),
    Function(Function),
    BuildIn(BuildIn),
}

impl ObjectType {
    pub fn to_type(&self) -> i32 {
        match self {
            &ObjectType::Integer(_) => 0,
            &ObjectType::Boolean(_) => 1,
            &ObjectType::Null(_) => 2,
            &ObjectType::Return(_) => 3,
            &ObjectType::Error(_) => 4,
            &ObjectType::Function(_) => 5,
            &ObjectType::StringType(_) => 6,
            &ObjectType::BuildIn(_) => 7,
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
            ObjectType::StringType(ref x) => x.clone(),
            ObjectType::Boolean(ref x) => format!("{}", x),
            ObjectType::Null(ref x) => format!("{}", x),
            ObjectType::Return(ref x) => format!("{:?}", x),
            ObjectType::Error(ref x) => format!("Error: {}", x),
            ObjectType::Function(ref x) => format!("Function: {:?}", x),
            ObjectType::BuildIn(ref x) => format!("BuildIn: {:?}", x),
        }
    }

    pub fn new_i32(x: i32) -> Self {
        Object { object_type: ObjectType::Integer(x) }
    }

    pub fn new_string(x: String) -> Self {
        Object { object_type: ObjectType::StringType(x) }
    }

    pub fn new_return_value(x: Self) -> Self {
        Object { object_type: ObjectType::Return(Box::new(x)) }
    }

    pub fn new_error(x: String) -> Self {
        Object { object_type: ObjectType::Error(x) }
    }

    pub fn new_function(p: Vec<Identifier>, b: BlockStatement, e: &mut Enviroment) -> Self {
        Object {
            object_type: ObjectType::Function(Function {
                                                  parameters: p,
                                                  body: b,
                                                  env: e.clone(),
                                              }),
        }
    }

    #[allow(dead_code)]
    pub fn to_i32(&self) -> Option<i32> {
        match self.object_type {
            ObjectType::Integer(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub fn to_string(&self) -> Option<String> {
        match self.object_type {
            ObjectType::StringType(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub fn to_bool(&self) -> Option<bool> {
        match self.object_type {
            ObjectType::Boolean(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub fn to_error_message(&self) -> Option<String> {
        match self.object_type {
            ObjectType::Error(ref x) => Some(x.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enviroment {
    store: HashMap<String, Object>,
    outer: Option<Box<Enviroment>>,
}

impl Enviroment {
    pub fn new() -> Self {
        Enviroment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_enviroment(outer: Self) -> Self {
        Enviroment {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, key: &String) -> Option<&Object> {
        match self.outer {
            Some(ref o) => {
                match o.store.get(key) {
                    Some(v) => Some(v),
                    None => self.store.get(key),
                }
            }
            None => self.store.get(key),
        }
    }

    pub fn set(&mut self, key: String, value: Object) -> Object {
        self.store.insert(key, value.clone());
        value
    }
}

