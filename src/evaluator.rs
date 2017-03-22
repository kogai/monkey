use ast::{Nodes, Statement};
use object::{Object, ObjectType, Null};

const TRUE: Object = Object { object_type: ObjectType::Boolean(true) };
const FALSE: Object = Object { object_type: ObjectType::Boolean(false) };

fn native_bool_to_boolean_obj(x: bool) -> Object {
    match x {
        true => TRUE,
        false => FALSE,
    }
}

pub fn eval(node: Nodes) -> Object {
    use self::Nodes::*;
    match node {
        Program(x) => eval_statements(&x.statements),
        ExpressionStatement(x) => eval(x.expression.to_enum()),
        IntegerLiteral(n) => Object { object_type: ObjectType::Integer(n.value) },
        Boolean(n) => native_bool_to_boolean_obj(n.value),
        _ => Object { object_type: ObjectType::Null(Null) },
    }
}

pub fn eval_statements(statements: &Vec<Box<Statement>>) -> Object {
    // TODO: iterate statements.
    let statement = statements.first().unwrap();
    eval(statement.to_enum())
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer;
    use parser;
    use ast::Node;

    fn test_eval(input: String) -> Object {
        let l = lexer::new(input);
        let mut parser = parser::new(l);
        let program = parser.parse_program();
        eval(program.to_enum())
    }

    #[test]
    fn it_should_evaluate_integer_expression() {
        let expects = [("5", 5), ("10", 10)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_usize().unwrap(), expect.1);
        }
    }

    #[test]
    fn it_should_evaluate_boolean_expression() {
        let expects = [("true", true), ("false", false)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_bool().unwrap(), expect.1);
        }
    }
}

