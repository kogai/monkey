use std::fmt::Display;

#[derive(Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

#[derive(Debug)]
pub struct Object<T: Display> {
    object_type: ObjectType,
    value: T,
}

impl<T: Display> Object<T> {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

