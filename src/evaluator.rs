use ast::{Node, Nodes, Statement, IfExpression, BlockStatement};
use object::{Object, ObjectType, Null};

const TRUE: Object = Object { object_type: ObjectType::Boolean(true) };
const FALSE: Object = Object { object_type: ObjectType::Boolean(false) };
const NULL: Object = Object { object_type: ObjectType::Null(Null) };

fn is_error(x: &Object) -> bool {
    x.object_type.to_type() == Object::new_error("".to_string()).object_type.to_type()
}

pub fn eval(node: Nodes) -> Object {
    use self::Nodes::*;
    match node {
        Program(x) => eval_program(&x.statements),
        BlockStatement(x) => eval_block_statement(x),
        ReturnStatement(x) => {
            let val = eval(x.return_value.to_enum());
            if is_error(&val) {
                return val;
            }
            Object::new_return_value(val)
        }
        IfExpression(x) => eval_if_expression(x),
        ExpressionStatement(x) => eval(x.expression.to_enum()),
        IntegerLiteral(n) => Object::new_i32(n.value),
        Boolean(n) => native_bool_to_boolean_obj(n.value),
        PrefixExpression(x) => {
            let operator = x.operator.clone();
            let right = eval(x.right.to_enum());
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(operator, right)
        }
        InfixExpression(x) => {
            let operator = x.operator.clone();
            let left = eval(x.left.to_enum());
            if is_error(&left) {
                return left;
            }
            let right = eval(x.right.to_enum());
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(operator, left, right)
        }
        _ => NULL,
    }
}

fn eval_program(statements: &Vec<Box<Statement>>) -> Object {
    let mut result: Object = NULL;
    for statement in statements.iter() {
        result = eval(statement.to_enum());
        if let ObjectType::Return(x) = result.object_type {
            return *x;
        }
        if let ObjectType::Error(_) = result.object_type {
            return result;
        }
    }
    result
}

fn eval_block_statement(x: &BlockStatement) -> Object {
    let mut result: Object = NULL;
    for statement in x.statements.iter() {
        result = eval(statement.to_enum());
        if let ObjectType::Return(_) = result.object_type {
            return result;
        }
        if let ObjectType::Error(_) = result.object_type {
            return result;
        }
    }
    result
}

fn eval_if_expression(x: &IfExpression) -> Object {
    let condition = eval(x.condition.to_enum());
    if is_error(&condition) {
        return condition;
    }
    match is_truthy(condition) {
        true => eval(x.consequence.to_enum()),
        false => {
            if let &Some(ref y) = &x.alternative {
                return eval(y.to_enum());
            };
            NULL
        }
    }
}

fn is_truthy(x: Object) -> bool {
    match x {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
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

    if left.object_type.to_type() != right.object_type.to_type() {
        return Object::new_error(format!("type mismatch: {:?} {} {:?}",
                                         left.object_type,
                                         operator,
                                         right.object_type));
    }

    match operator.as_str() {
        "==" => native_bool_to_boolean_obj(left == right),
        "!=" => native_bool_to_boolean_obj(left != right),
        _ => {
            Object::new_error(format!("unknown operator: {:?} {} {:?}",
                                      left.object_type,
                                      operator,
                                      right.object_type))
        }
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
        _ => Object::new_error(format!("unknown operator: Integer {} Integer", operator)),
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Object::new_error(format!("unknown operator: {}{:?}", operator, right.object_type)),
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    match right.object_type {
        ObjectType::Integer(x) => Object::new_i32(-x),
        _ => Object::new_error(format!("unknown operator: -{:?}", right.object_type)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use lexer;
    use parser;
    use ast::Node;

    fn test_eval(input: String) -> Object {
        let l = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(l);
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

    #[test]
    fn it_should_evaluate_if_else_expression() {
        let expects = [("if (true) { 10 }", Some(10)),
                       ("if (false) { 10 }", None),
                       ("if (1) { 11 }", Some(11)),
                       ("if (1 < 2) { 12 }", Some(12)),
                       ("if (1 > 2) { 10 }", None),
                       ("if (1 > 2) { 10 } else { 20 }", Some(20)),
                       ("if (1 < 2) { 13 } else { 20 }", Some(13))];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_i32(), expect.1);
        }
    }

    #[test]
    fn it_should_evaluate_return_expression() {
        let expects = [("return 10;", 10),
                       ("return 10; 9;", 10),
                       ("return 2 * 5; 9;", 10),
                       ("9; return 2 * 5; 9;", 10),
                       ("
                        if (10 > 1) {
                           if (10 > 1) {
                               return 10;
                           }
                        }
                        return 1;
                       ",
                        10)];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_i32().unwrap(), expect.1);
        }
    }

    #[test]
    fn it_should_error_handling() {
        let expects = [("5 + true;", "type mismatch: Integer(5) + Boolean(true)"),
                       ("5 + true; 5;", "type mismatch: Integer(5) + Boolean(true)"),
                       ("-true;", "unknown operator: -Boolean(true)"),
                       ("true + false;", "unknown operator: Boolean(true) + Boolean(false)"),
                       ("5; true + false; 5;", "unknown operator: Boolean(true) + Boolean(false)"),
                       ("if (10 > 1) { true + false; };",
                        "unknown operator: Boolean(true) + Boolean(false)"),
                       ("
                       if (10 > 1) {
                            if (10 > 1) {
                                return true + false;
                            };
                        };
                       ",
                        "unknown operator: Boolean(true) + Boolean(false)")];
        for expect in expects.iter() {
            let result = test_eval(expect.0.to_string());
            assert_eq!(result.to_error_message().unwrap(), expect.1);
        }
    }
}

