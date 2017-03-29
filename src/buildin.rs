use object::{Object, ObjectType};
use evaluator::NULL;

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
                match x.object_type {
                    ObjectType::StringType(ref s) => Object::new_i32(s.len() as i32),
                    ObjectType::Array(ref a) => Object::new_i32(a.elements.len() as i32),
                    _ => {
                        Object::new_error(format!("argument to \"len\" not supported. got {:?}",
                                                  x.object_type))
                    }
                }
            }
            None => {
                Object::new_error(format!("wrong number of arguments. got {} want=1", xs.len()))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrintLn;

impl BuildInFunction for PrintLn {
    fn call(&self, xs: Vec<Object>) -> Object {
        println!("{}",
                 xs.into_iter()
                     .map(|x| x.inspect())
                     .collect::<Vec<String>>()
                     .join(" "));
        NULL
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildIn {
    Len(Len),
    PrintLn(PrintLn),
}

impl BuildIn {
    pub fn set_from_string(function_name: &String) -> Option<Object> {
        match function_name.as_str() {
            "len" => Some(Object { object_type: ObjectType::BuildIn(BuildIn::Len(Len)) }),
            "puts" => Some(Object { object_type: ObjectType::BuildIn(BuildIn::PrintLn(PrintLn)) }),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_in_let_should_recieve_string() {
        let len = Len {};
        let expect = [Object::new_string("test".to_string())].to_vec();
        assert_eq!(len.call(expect).to_i32().unwrap(), 4);
    }

    #[test]
    fn build_in_let_should_recieve_array() {
        let len = Len {};
        let array = Object::new_array([Object::new_i32(1), Object::new_i32(2), Object::new_i32(3)]
                                          .to_vec());
        let expect = [array].to_vec();
        assert_eq!(len.call(expect).to_i32().unwrap(), 3);
    }
}

