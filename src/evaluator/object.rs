use std::fmt::{Display, Formatter, Result};
use std::collections::HashMap;

use parser::ast::{Identifier, BlockStatement};
use buildin::BuildIn;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub elements: Vec<Object>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Integer(i32),
    StringType(String),
    Boolean(bool),
}

impl HashKey {
    pub fn new(x: &Object) -> Option<Self> {
        match x.object_type {
            ObjectType::Integer(ref x) => Some(HashKey::Integer(x.clone())),
            ObjectType::StringType(ref x) => Some(HashKey::StringType(x.clone())),
            ObjectType::Boolean(ref x) => Some(HashKey::Boolean(x.clone())),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashType {
    pub pairs: HashMap<HashKey, Object>,
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
    Array(Array),
    HashType(HashType),
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
            &ObjectType::Array(_) => 8,
            &ObjectType::HashType(_) => 9,
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
            ObjectType::Array(ref x) => format!("Array: {:?}", x),
            ObjectType::HashType(ref x) => format!("HashType: {:?}", x),
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

    pub fn new_array(x: Vec<Object>) -> Self {
        Object { object_type: ObjectType::Array(Array { elements: x }) }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_equivalence_hash_key() {
        let k1 = HashKey::StringType("my-key".to_string());
        let k2 = HashKey::StringType("my-key".to_string());
        let v = Object::new_string("monkey".to_string());
        let mut p = HashMap::new();
        p.insert(k1.clone(), v);
        let h = HashType { pairs: p };
        assert_eq!(h.pairs.get(&k1), h.pairs.get(&k2));
    }
}

