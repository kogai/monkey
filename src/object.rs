use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

#[derive(Debug, Clone)]
pub struct Null;

impl Display for Null {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Null {{}}")
    }
}

#[derive(Debug)]
pub enum Objects {
    Integer(Object<usize>),
    Boolean(Object<bool>),
    Null(Object<Null>),
}

impl Objects {
    pub fn to_concrete_usize(&self) -> Option<Object<usize>> {
        match *self {
            Objects::Integer(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn to_concrete_bool(&self) -> Option<Object<bool>> {
        match *self {
            Objects::Boolean(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn to_concrete_null(&self) -> Option<Object<Null>> {
        match *self {
            Objects::Null(ref x) => Some(x.clone()),
            _ => None,
        }
    }

    pub fn to_concrete_type<T: Display>(&self) -> Object<T> {
        // let f = match *self {
        //     Objects::Integer(x) => x,
        //     Objects::Boolean(x) => x,
        //     Objects::Null(x) => x,
        // };
        unimplemented!();
    }
}

#[derive(Debug, Clone)]
pub struct Object<T: Display> {
    pub object_type: ObjectType,
    pub value: T,
}

impl<T: Display> Object<T> {
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

