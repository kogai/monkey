use ast::{Nodes, Statement};
use object::{Object, ObjectType, Null};

const TRUE: Object = Object { object_type: ObjectType::Boolean(true) };
const FALSE: Object = Object { object_type: ObjectType::Boolean(false) };
const NULL: Object = Object { object_type: ObjectType::Null(Null) };

pub fn eval(node: Nodes) -> Object {
    use self::Nodes::*;
    match node {
        Program(x) => eval_statements(&x.statements),
        ExpressionStatement(x) => eval(x.expression.to_enum()),
        IntegerLiteral(n) => Object::new_i32(n.value),
        Boolean(n) => native_bool_to_boolean_obj(n.value),
        PrefixExpression(x) => {
            let operator = x.operator.clone();
            let right = eval(x.right.to_enum());
            eval_prefix_expression(operator, right)
        }
        InfixExpression(x) => {
            let operator = x.operator.clone();
            let left = eval(x.left.to_enum());
            let right = eval(x.right.to_enum());
            eval_infix_expression(operator, left, right)
        }
        _ => NULL,
    }
}

fn native_bool_to_boolean_obj(x: bool) -> Object {
    match x {
        true => TRUE,
        false => FALSE,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    if let ObjectType::Integer(l) = left.object_type {
        if let ObjectType::Integer(r) = right.object_type {
            return eval_integer_infix_expression(operator, l, r);
        }
    }

    match operator.as_str() {
        "==" => native_bool_to_boolean_obj(left == right),
        "!=" => native_bool_to_boolean_obj(left != right),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(operator: String, left: i32, right: i32) -> Object {
    match operator.as_str() {
        "+" => Object::new_i32(left + right),
        "-" => Object::new_i32(left - right),
        "*" => Object::new_i32(left * right),
        "/" => Object::new_i32(left / right),
        "<" => native_bool_to_boolean_obj(left < right),
        ">" => native_bool_to_boolean_obj(left > right),
        "==" => native_bool_to_boolean_obj(left == right),
        "!=" => native_bool_to_boolean_obj(left != right),
        _ => NULL,
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => NULL,
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    match right.object_type {
        ObjectType::Integer(x) => Object::new_i32(-x),
        _ => NULL,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_statements(statements: &Vec<Box<Statement>>) -> Object {
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
        let expects = [("5", 5),
                       ("10", 10),
                       ("-5", -5),
                       ("-10", -10),
                       ("5 + 5 + 5 + 5 - 10", 10),
                       ("2 * 2 * 2 * 2 * 2", 32),
                       ("-50 + 100 + -50", 0),
                       ("5 * 2 + 10", 20),
                       ("5 + 2 * 10", 25),
                       ("20 + 2 * -10", 0),
                       ("50 / 2 * 2 + 10", 60),
                       ("2 * (5 + 10)", 30),
                       ("3 * 3 * 3 + 10", 37),
                       ("3 * (3 * 3) + 10", 37),
                       ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_i32().unwrap(), expect.1);
        }
    }

    #[test]
    fn it_should_evaluate_boolean_expression() {
        let expects = [("true", true),
                       ("false", false),
                       ("1 < 2", true),
                       ("1 > 2", false),
                       ("1 < 1", false),
                       ("1 > 1", false),
                       ("1 == 1", true),
                       ("1 != 1", false),
                       ("1 == 2", false),
                       ("1 != 2", true),
                       ("true == true", true),
                       ("false == false", true),
                       ("true == false", false),
                       ("true != false", true),
                       ("false != true", true),
                       ("(1 < 2) == true", true),
                       ("(1 < 2) == false", false),
                       ("(1 > 2) == true", false),
                       ("(1 > 2) == false", true)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_bool().unwrap(), expect.1);
        }
    }

    #[test]
    fn it_should_evaluate_bang_operator() {
        let expects = [("!true", false), ("!false", true)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_bool().unwrap(), expect.1);
        }
    }
}

