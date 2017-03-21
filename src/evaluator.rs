use ast::{Node, Nodes, Statement};
use object::{Object, ObjectType, Null};

pub fn eval(node: Nodes) -> Object {
    use self::Nodes::*;
    match node {
        Program(x) => eval_statements(&x.statements),
        ExpressionStatement(x) => eval(x.expression.to_enum()),
        IntegerLiteral(n) => Object { object_type: ObjectType::Integer(n.value) },
        Boolean(n) => Object { object_type: ObjectType::Boolean(n.value) },
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

    fn test_eval(input: String) -> Object {
        let l = lexer::new(input);
        let mut parser = parser::new(l);
        let program = parser.parse_program();
        eval(program.to_enum())
    }

    fn test_object(obj: Object, expected: String) {
        assert_eq!(obj.inspect(), expected);
    }

    #[test]
    fn it_should_evaluate_integer_expression() {
        let expects = [("5", "5"), ("10", "10")];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            test_object(result, expect.1.to_string());
        }
    }
}

