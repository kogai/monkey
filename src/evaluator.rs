use std::fmt::Display;
use ast::{Node, Nodes};
use object::{Object, ObjectType, Objects, Null};

pub fn eval<T: Node>(node: T) -> Objects {
    match node.to_enum() {
        Nodes::IntegerLiteral(n) => {
            Objects::Integer(Object::<usize> {
                                 object_type: ObjectType::Integer,
                                 value: n.value,
                             })
        }
        _ => {
            Objects::Null(Object::<Null> {
                              object_type: ObjectType::Null,
                              value: Null,
                          })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;
    use std::cmp::PartialEq;
    use lexer;
    use parser;

    fn test_eval<T: Display + PartialEq + Debug>(input: String) -> Objects {
        let l = lexer::new(input);
        let mut parser = parser::new(l);
        let program = parser.parse_program();
        eval(program)
    }

    fn test_object<T: Display + PartialEq + Debug>(obj: Object<T>, expected: T) {
        assert_eq!(obj.value, expected);
    }

    #[test]
    fn it_should_evaluate_integer_expression() {
        let expects = [("5", 5), ("10", 10)];
        for expect in expects.iter() {
            let result = test_eval::<usize>(expect.0.to_string()).to_concrete_usize().unwrap();
            test_object(result, expect.1);
        }
    }
}

