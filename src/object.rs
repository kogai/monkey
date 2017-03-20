use std::fmt::Display;

#[derive(Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

#[derive(Debug)]
pub struct Object<T: Display> {
    pub object_type: ObjectType,
    pub value: T,
}

impl<T: Display> Object<T> {
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

