use object::{Object, ObjectType};

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

