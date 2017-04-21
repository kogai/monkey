use std::str::FromStr;
use lexer::token::{Token, TokenType};
use lexer::lexer::Lexer;
use parser::ast::{Program, LetStatement, ReturnStatement, ExpressionStatement, Identifier,
                  PrefixExpression, InfixExpression, IntegerLiteral, Boolean, IfExpression,
                  BlockStatement, FunctionLiteral, CallExpression, Statements, Expressions,
                  StringLiteral, ArrayLiteral, IndexExpression, HashLiteral};

#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
    INDEX,
}

fn precendences(token: TokenType) -> Precedence {
    use self::Precedence::*;
    use self::TokenType::*;

    match token {
        EQ => EQUALS,
        NOTEQ => EQUALS,
        LT => LESSGREATER,
        GT => LESSGREATER,
        PLUS => SUM,
        MINUS => SUM,
        DIVIDE => PRODUCT,
        MULTIPLY => PRODUCT,
        LPAREN => CALL,
        LBRACKET => INDEX,
        _ => LOWEST,
    }
}

fn is_infix_operator(t: TokenType) -> bool {
    use self::TokenType::*;
    match t {
        PLUS | MINUS | DIVIDE | MULTIPLY | EQ | NOTEQ | LT | GT | LPAREN | LBRACKET => true,
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let first = lexer.next_token();
        let second = lexer.next_token();
        Parser {
            lexer: lexer,
            current_token: first,
            peek_token: second,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn peek_precedence(&self) -> Precedence {
        precendences(self.peek_token.token_type.clone())
    }

    fn current_precedence(&self) -> Precedence {
        precendences(self.current_token.token_type.clone())
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statements> = vec![];

        while self.current_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();
            statements.push(statement);
            self.next_token();
        }

        Program { statements: statements }
    }

    fn parse_statement(&mut self) -> Statements {
        match self.current_token.token_type {
            TokenType::LET => Statements::new_let_statement(self.parse_let_statement()),
            TokenType::RETURN => Statements::new_return_statement(self.parse_return_statement()),
            _ => Statements::new_expression_statement(self.parse_expression_statement()),
        }
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        let current_token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        ExpressionStatement {
            token: current_token,
            expression: expression,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expressions {
        let token_type = self.current_token.token_type.clone();
        let mut left = self.parse_prefix(token_type).unwrap();

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let token_type = self.peek_token.token_type.clone();
            self.next_token();
            let infix_expression = self.parse_infix(token_type.clone(), left);

            if !is_infix_operator(token_type) {
                return infix_expression;
            }
            left = infix_expression;
        }
        left
    }

    fn parse_prefix(&mut self, t: TokenType) -> Option<Expressions> {
        use self::TokenType::*;
        match t {
            IDENT(_) => Some(self.parse_identifier()),
            INT(_) => self.parse_integer_literal(),
            STRING(_) => Some(self.parse_string_literal()),
            BANG => Some(self.parse_prefix_expression()),
            MINUS => Some(self.parse_prefix_expression()),
            TRUE => Some(self.parse_boolean()),
            FALSE => Some(self.parse_boolean()),
            LPAREN => self.parse_group_expression(),
            LBRACKET => Some(self.parse_array_literal()),
            LBRACE => Some(self.parse_hash_literal()),
            IF => Some(self.parse_if_expression()),
            FUNCTION => Some(self.parse_function_literal()),
            _ => None,
        }
    }

    fn parse_array_literal(&mut self) -> Expressions {
        let token = self.current_token.clone();
        let elements = self.parse_expression_list(TokenType::RBRACKET);
        Expressions::new_array_literal(ArrayLiteral {
                                           token: token,
                                           elements: elements,
                                       })
    }

    fn parse_hash_literal(&mut self) -> Expressions {
        let token = self.current_token.clone();
        let mut hash_map = HashLiteral::new(token);

        while !self.peek_token_is(TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST);
            self.expect_peek_token(TokenType::COLON);
            self.next_token();

            let value = self.parse_expression(Precedence::LOWEST);
            hash_map.set_pairs(key, value);
            self.expect_peek_token(TokenType::COMMA);
        }
        self.expect_peek_token(TokenType::RBRACE);
        Expressions::HashLiteral(hash_map)
    }

    fn parse_function_literal(&mut self) -> Expressions {
        let token = self.current_token.clone();
        self.expect_peek_token(TokenType::LPAREN);
        let parameters = self.parse_function_parameters();
        self.expect_peek_token(TokenType::LBRACE);
        let body = self.parse_block_statement();

        Expressions::new_function_literal(FunctionLiteral {
                                              token: token,
                                              parameters: parameters,
                                              body: body,
                                          })
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();
        identifiers.push(Identifier {
                             token: self.current_token.clone(),
                             value: self.current_token.literal.clone(),
                         });
        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier {
                                 token: self.current_token.clone(),
                                 value: self.current_token.literal.clone(),
                             });
        }
        self.expect_peek_token(TokenType::RPAREN);
        identifiers
    }

    fn parse_boolean(&self) -> Expressions {
        Expressions::new_boolean(Boolean {
                                     token: self.current_token.clone(),
                                     value: self.current_token_is(TokenType::TRUE),
                                 })
    }

    fn parse_if_expression(&mut self) -> Expressions {
        let token = self.current_token.clone();
        self.expect_peek_token(TokenType::LPAREN);
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST);
        self.expect_peek_token(TokenType::RPAREN);
        self.expect_peek_token(TokenType::LBRACE);
        let consequence = self.parse_block_statement();

        let alternative = match self.peek_token_is(TokenType::ELSE) {
            true => {
                self.next_token();
                self.expect_peek_token(TokenType::LBRACE);
                Some(self.parse_block_statement())
            }
            false => None,
        };

        Expressions::new_if_expression(IfExpression {
                                           token: token,
                                           condition: Box::new(condition),
                                           consequence: consequence,
                                           alternative: alternative,
                                       })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.current_token.clone();
        let mut statements: Vec<Statements> = vec![];
        self.next_token();

        while !self.current_token_is(TokenType::RBRACE) {
            let statement = self.parse_statement();
            statements.push(statement);
            self.next_token();
        }

        BlockStatement {
            token: token,
            statements: statements,
        }
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Box<Expressions>> {
        let mut arguments: Vec<Box<Expressions>> = vec![];
        if self.peek_token_is(end.clone()) {
            self.next_token();
            return arguments;
        }
        self.next_token();
        arguments.push(Box::new(self.parse_expression(Precedence::LOWEST)));

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            arguments.push(Box::new(self.parse_expression(Precedence::LOWEST)));
        }

        self.expect_peek_token(end.clone());
        arguments
    }

    fn parse_infix(&mut self, t: TokenType, left: Expressions) -> Expressions {
        use self::TokenType::*;
        match t {
            PLUS => self.parse_infix_expression(left),
            MINUS => self.parse_infix_expression(left),
            DIVIDE => self.parse_infix_expression(left),
            MULTIPLY => self.parse_infix_expression(left),
            EQ => self.parse_infix_expression(left),
            NOTEQ => self.parse_infix_expression(left),
            LT => self.parse_infix_expression(left),
            GT => self.parse_infix_expression(left),
            LPAREN => self.parse_call_expression(left),
            LBRACKET => self.parse_index_expression(left),
            _ => left,
        }
    }

    fn parse_prefix_expression(&mut self) -> Expressions {
        let current_token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        self.next_token();

        let expression = self.parse_expression(Precedence::PREFIX);
        Expressions::new_prefix_expression(PrefixExpression {
                                               token: current_token,
                                               operator: operator,
                                               right: Box::new(expression),
                                           })
    }

    fn parse_infix_expression(&mut self, left: Expressions) -> Expressions {
        let current_token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        let precendence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precendence);

        Expressions::new_infix_expression(InfixExpression {
                                              token: current_token,
                                              operator: operator,
                                              left: Box::new(left),
                                              right: Box::new(right),
                                          })
    }

    fn parse_index_expression(&mut self, left: Expressions) -> Expressions {
        let token = self.current_token.clone();
        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST);
        self.expect_peek_token(TokenType::RBRACKET);
        Expressions::IndexExpression(IndexExpression {
                                         token: token,
                                         index: Box::new(index),
                                         left: Box::new(left),
                                     })
    }

    fn parse_call_expression(&mut self, function: Expressions) -> Expressions {
        let token = self.current_token.clone();
        let arguments = self.parse_expression_list(TokenType::RPAREN);
        Expressions::new_call_expression(CallExpression {
                                             token: token,
                                             function: Box::new(function),
                                             arguments: arguments,
                                         })
    }

    fn parse_identifier(&mut self) -> Expressions {
        Expressions::new_identifier(Identifier {
                                        token: self.current_token.clone(),
                                        value: self.current_token.literal.clone(),
                                    })
    }

    fn parse_integer_literal(&mut self) -> Option<Expressions> {
        let current_token = self.current_token.clone();
        let value = i32::from_str(self.current_token
                                      .literal
                                      .as_str()
                                      .clone());

        match value {
            Ok(s) => {
                Some(Expressions::new_integer_literal(IntegerLiteral {
                                                          token: current_token,
                                                          value: s,
                                                      }))
            }
            Err(_) => {
                self.errors.push(format!("could not parse {:?} as integer", current_token));
                None
            }
        }
    }

    fn parse_string_literal(&mut self) -> Expressions {
        Expressions::new_string_literal(StringLiteral {
                                            token: self.current_token.clone(),
                                            value: self.current_token.literal.clone(),
                                        })
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let current_token = self.current_token.clone();
        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        ReturnStatement {
            token: current_token,
            return_value: value,
        }
    }

    fn parse_let_statement(&mut self) -> LetStatement {
        let current_token = self.current_token.clone();
        let ident = self.peek_token.literal.clone();

        self.expect_peek_token(TokenType::IDENT(ident));

        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        self.expect_peek_token(TokenType::ASSIGN);

        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        LetStatement {
            name: name,
            token: current_token,
            value: value,
        }
    }

    fn parse_group_expression(&mut self) -> Option<Expressions> {
        self.next_token();
        let expression = self.parse_expression(Precedence::LOWEST);
        match self.expect_peek_token(TokenType::RPAREN) {
            true => Some(expression),
            false => None,
        }
    }

    fn current_token_is(&self, t: TokenType) -> bool {
        self.current_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek_token(&mut self, t: TokenType) -> bool {
        let tok = t.clone();
        match self.peek_token_is(t) {
            true => {
                self.next_token();
                true
            }
            false => {
                self.peek_error(tok);
                false
            }
        }
    }

    fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!("expected next token to be {:?}, got {:?} instead",
                                 t,
                                 self.peek_token.token_type));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use parser::ast::Node;
    use lexer::lexer;

    #[test]
    fn it_should_detect_token_type() {
        let l = lexer::Lexer::new("let x = 5;".to_string());
        let parser = Parser::new(l);
        assert!(parser.current_token_is(TokenType::LET));
        assert!(parser.peek_token_is(TokenType::IDENT("x".to_string())));
    }

    #[test]
    fn is_should_detect_peek_token() {
        let l = lexer::Lexer::new("let x = 5;".to_string());
        let mut parser = Parser::new(l);
        assert!(parser.expect_peek_token(TokenType::IDENT("x".to_string())));
        assert!(parser.expect_peek_token(TokenType::ASSIGN));
        assert!(parser.expect_peek_token(TokenType::INT("5".to_string())));
        assert!(parser.expect_peek_token(TokenType::SEMICOLON));
    }

    #[test]
    fn it_should_parse_let_statement() {
        let expects = [("let x = 5;", "x", "5"),
                       ("let y = true;", "y", "true"),
                       ("let foobar = y;", "foobar", "y")];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());
            let mut parser = Parser::new(l);
            let parsed = parser.parse_let_statement();

            assert_eq!(parsed.token.token_type, TokenType::LET);
            assert_eq!(parsed.name.value, expect.1);
            assert_eq!(parsed.value.token_literal(), expect.2);
        }
    }

    #[test]
    fn it_should_parse_statements() {
        let l = lexer::Lexer::new("
      let x = 5;
      let y = 10;
      let foobar = 838383;
    "
                                          .to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();

        assert_eq!(statements_count, 3);

        let expects = ["x", "y", "foobar"];

        for i in 0..statements_count {
            let expect = expects[i];
            let statement = &statements[i];
            if let Statements::LetStatement(ls) = statement.clone() {
                assert_eq!(statement.token_literal(), "let");
                assert_eq!(ls.name.value, expect);
                assert_eq!(ls.name.to_enum().token_literal(), expect);
            } else {
                assert!(false);
            }
        }
    }

    #[test]
    fn it_should_peek_error_syntax() {
        let l = lexer::Lexer::new("
      let x 5;
      let = 10;
      let 838383;
    "
                                          .to_string());

        let mut parser = Parser::new(l);
        parser.parse_program();
        let errors_count = parser.errors.len();

        assert_eq!(errors_count, 4);
        let expects = ["expected next token to be ASSIGN, got INT(\"5\") instead",
                       "expected next token to be IDENT(\"=\"), got ASSIGN instead",
                       "expected next token to be IDENT(\"838383\"), got INT(\"838383\") instead",
                       "expected next token to be ASSIGN, got INT(\"838383\") instead"];

        for i in 0..errors_count {
            assert_eq!(&parser.errors[i], &expects[i]);
        }
    }

    #[test]
    fn it_should_parse_return_statemtn() {
        let expects = [("return 5;", "5"), ("return 10;", "10"), ("return 993322;", "993322")];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());
            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statement = &statements[0];
            assert_eq!(statement.token_literal(), "return");
            if let Statements::ReturnStatement(x) = statement.clone() {
                assert_eq!(x.return_value.string(), expect.1);
            }
        }
    }

    #[test]
    fn it_should_parse_identifier_expression() {
        let l = lexer::Lexer::new("foobar;".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();

        assert_eq!(statements_count, 1);
        let statement = &statements[0];
        if let Statements::ExpressionStatement(x) = statement.clone() {
            let identifier = x.expression;
            assert_eq!(identifier.token_literal(), "foobar");
            if let Expressions::Identifier(y) = identifier {
                return assert_eq!(y.value, "foobar");
            }
        }
        assert!(false);
    }

    #[test]
    fn it_should_parse_integer_literal_expression() {
        let l = lexer::Lexer::new("5;".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();

        assert_eq!(statements_count, 1);

        let statement = &statements[0];
        if let Statements::ExpressionStatement(x) = statement.clone() {
            let identifier = x.expression;
            assert_eq!(identifier.token_literal(), "5");
            if let Expressions::IntegerLiteral(y) = identifier {
                return assert_eq!(y.value, 5);
            }
        }
        assert!(false);
    }

    #[test]
    fn it_should_parse_string_expression() {
        let l = lexer::Lexer::new("\"hello world.\";".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();

        assert_eq!(statements_count, 1);

        let statement = &statements[0];
        if let Statements::ExpressionStatement(x) = statement.clone() {
            let identifier = x.expression;
            if let Expressions::StringLiteral(y) = identifier {
                return assert_eq!(y.value, "hello world.");
            }
        }
        assert!(false);
    }

    #[test]
    fn it_should_parse_boolean_expression() {
        let expects = [("true;", true), ("false;", false)];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statements_count = statements.len();

            assert_eq!(statements_count, 1);
            let statement = &statements[0];
            if let Statements::ExpressionStatement(x) = statement.clone() {
                let identifier = x.expression;
                if let Expressions::Boolean(y) = identifier {
                    assert_eq!(y.value, expect.1);
                    continue;
                }
            }
            assert!(false);
        }
    }

    #[test]
    fn it_should_parse_if_expression() {
        let l = lexer::Lexer::new("if (x < y) {x}".to_string());
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = (&statements[0]).clone() {
            if let Expressions::IfExpression(statement) = expression.expression {
                if let Expressions::InfixExpression(condition) = *statement.condition {
                    assert_eq!(condition.operator, "<");
                    if let Expressions::Identifier(left) = *condition.left {
                        assert_eq!(left.value, "x");
                    }
                    if let Expressions::Identifier(right) = *condition.right {
                        assert_eq!(right.value, "y");
                    }
                }
                if let Statements::ExpressionStatement(consequence) =
                    (&statement.consequence.statements[0]).clone() {
                    assert_eq!(consequence.token.token_type,
                               TokenType::IDENT("x".to_string()));
                }
            }
        }
    }

    #[test]
    fn it_should_parse_if_else_expression() {
        let l = lexer::Lexer::new("if (x < y) {x} else {y}".to_string());
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::IfExpression(statement) = expression.expression {
                if let Expressions::InfixExpression(condition) = *statement.condition {
                    assert_eq!(condition.operator, "<");
                    if let Expressions::Identifier(left) = *condition.left {
                        assert_eq!(left.value, "x");
                    }
                    if let Expressions::Identifier(right) = *condition.right {
                        assert_eq!(right.value, "y");
                    }
                }
                if let Statements::ExpressionStatement(consequence) =
                    statement.consequence.statements[0].clone() {
                    assert_eq!(consequence.token.token_type,
                               TokenType::IDENT("x".to_string()));
                }
                if let Statements::ExpressionStatement(alternative) =
                    statement.alternative
                        .as_ref()
                        .unwrap()
                        .statements
                        [0]
                            .clone() {
                    assert_eq!(alternative.token.token_type,
                               TokenType::IDENT("y".to_string()));
                }
            }
        }
    }

    #[test]
    fn it_should_parse_function_literal() {
        let l = lexer::Lexer::new("fn(x, y) { x + y };".to_string());
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::IfExpression(statement) = expression.expression {
                if let Expressions::FunctionLiteral(function) = *statement.condition {
                    assert_eq!(function.parameters.len(), 2);
                    assert_eq!(&function.parameters[0].value, "x");
                    assert_eq!(&function.parameters[1].value, "y");
                    assert_eq!(function.body.statements.len(), 1);

                    if let Statements::ExpressionStatement(body_statements) =
                        function.body.statements[0].clone() {
                        if let Expressions::InfixExpression(infix_expression) =
                            body_statements.expression {
                            if let Expressions::Identifier(left) = *infix_expression.left {
                                assert_eq!(left.value, "x");
                            }
                            if let Expressions::Identifier(right) = *infix_expression.right {
                                assert_eq!(right.value, "y");
                            }
                            assert_eq!(infix_expression.operator, "+");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn it_should_parse_array_literal() {
        let l = lexer::Lexer::new("[1, 2 * 2, 3 + 3];".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);
        if let Statements::ExpressionStatement(x) = (&statements[0]).clone() {
            if let Expressions::ArrayLiteral(y) = x.expression {
                assert_eq!(y.elements.len(), 3);
                assert_eq!(&y.elements[0].string(), "1");
                assert_eq!(&y.elements[1].string(), "(2 * 2)");
                assert_eq!(&y.elements[2].string(), "(3 + 3)");
            }
        }
    }

    #[test]
    fn it_should_parse_index_expression() {
        let l = lexer::Lexer::new("myArray[1 + 2];".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);
        if let Statements::ExpressionStatement(x) = (&statements[0]).clone() {
            if let Expressions::IndexExpression(y) = x.expression {
                assert_eq!(y.left.string(), "myArray");
                if let Expressions::InfixExpression(z) = *y.index {
                    assert_eq!(z.left.string(), "1");
                    assert_eq!(z.operator, "+");
                    assert_eq!(z.right.string(), "2");
                }
            }
        }
    }

    #[test]
    fn it_should_parse_call_expression() {
        let l = lexer::Lexer::new("add(1, 2 * 3, 4 + 5);".to_string());
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::CallExpression(call_expression) = expression.expression {
                if let Expressions::Identifier(identifier) = *call_expression.function {
                    assert_eq!(identifier.value, "add");
                }
                assert_eq!(&call_expression.arguments[0].string(), "1");
                assert_eq!(&call_expression.arguments[1].string(), "(2 * 3)");
                assert_eq!(&call_expression.arguments[2].string(), "(4 + 5)");
            }
        }
    }

    #[test]
    fn it_should_parse_prefix_expression() {
        let expects = [("!5;", "!", 5, "5"), ("-15;", "-", 15, "15")];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statements_count = statements.len();
            assert_eq!(statements_count, 1);

            if let Statements::ExpressionStatement(expression) = statements[0].clone() {
                if let Expressions::PrefixExpression(prefix) = expression.expression {
                    assert_eq!(prefix.operator, expect.1);
                    assert_eq!(prefix.right.token_literal(), expect.3);
                    if let Expressions::IntegerLiteral(integer) = *prefix.right {
                        assert_eq!(integer.value, expect.2);
                    }
                }
            }
        }
    }

    #[test]
    fn it_should_parse_prefix_expression_with_boolean() {
        let expects = [("!true;", "!", true), ("!false;", "!", false)];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statements_count = statements.len();
            assert_eq!(statements_count, 1);


            if let Statements::ExpressionStatement(expression) = statements[0].clone() {
                if let Expressions::PrefixExpression(prefix) = expression.expression {
                    assert_eq!(prefix.operator, expect.1);
                    if let Expressions::Boolean(integer) = *prefix.right {
                        assert_eq!(integer.value, expect.2);
                    }
                }
            }
        }
    }

    #[test]
    fn it_should_parse_infix_expression() {
        let expects = [("5 + 5;", 5, "+", 5),
                       ("5 - 5;", 5, "-", 5),
                       ("5 * 5;", 5, "*", 5),
                       ("5 / 5;", 5, "/", 5),
                       ("5 > 5;", 5, ">", 5),
                       ("5 < 5;", 5, "<", 5),
                       ("5 == 5;", 5, "==", 5),
                       ("5 != 5;", 5, "!=", 5)];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statements_count = statements.len();
            assert_eq!(statements_count, 1);

            if let Statements::ExpressionStatement(expression) = statements[0].clone() {
                if let Expressions::InfixExpression(infix_expression) = expression.expression {
                    if let Expressions::IntegerLiteral(left) = *infix_expression.left {
                        assert_eq!(left.value, expect.1);
                    }
                    assert_eq!(infix_expression.operator, expect.2);
                    if let Expressions::IntegerLiteral(right) = *infix_expression.right {
                        assert_eq!(right.value, expect.3);
                    }
                }
            }
        }
    }

    #[test]
    fn it_should_parse_infix_expression_with_boolean() {
        let expects = [("true == true;", true, "==", true),
                       ("true != false;", true, "!=", false),
                       ("false == false;", false, "==", false)];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let statements = program.statements;
            let statements_count = statements.len();
            assert_eq!(statements_count, 1);

            if let Statements::ExpressionStatement(expression) = statements[0].clone() {
                if let Expressions::InfixExpression(infix_expression) = expression.expression {
                    if let Expressions::Boolean(left) = *infix_expression.left {
                        assert_eq!(left.value, expect.1);
                    }
                    assert_eq!(infix_expression.operator, expect.2);
                    if let Expressions::Boolean(right) = *infix_expression.right {
                        assert_eq!(right.value, expect.3);
                    }
                }
            }
        }
    }

    #[test]
    fn it_should_parse_hash_literal_with_string() {
        let l = lexer::Lexer::new("{ \"one\": 1, \"two\": 2, \"three\": 3 }".to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::HashLiteral(x) = expression.expression {
                assert_eq!(x.pairs.len(), 3);
                let mut expected: HashMap<&str, String> = HashMap::new();
                expected.insert("one", "1".to_string());
                expected.insert("two", "2".to_string());
                expected.insert("three", "3".to_string());

                for (key, value) in x.pairs.iter() {
                    assert_eq!(expected.get(key.string().as_str()).unwrap(),
                               &value.string());
                }
            }
        }
    }

    #[test]
    fn it_should_parse_empty_hash_literal() {
        let l = lexer::Lexer::new("{}".to_string());
        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);
        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::HashLiteral(x) = expression.expression {
                assert_eq!(x.pairs.len(), 0);
            }
        }
    }

    #[test]
    fn it_should_parse_hash_expression() {
        let l = lexer::Lexer::new("{ \"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5 }"
                                      .to_string());

        let mut parser = Parser::new(l);
        let program = parser.parse_program();
        let statements = program.statements;
        let statements_count = statements.len();
        assert_eq!(statements_count, 1);

        if let Statements::ExpressionStatement(expression) = statements[0].clone() {
            if let Expressions::HashLiteral(x) = expression.expression {
                assert_eq!(x.pairs.len(), 3);
                let mut expected: HashMap<&str, String> = HashMap::new();
                expected.insert("one", "(0 + 1)".to_string());
                expected.insert("two", "(10 - 8)".to_string());
                expected.insert("three", "(15 / 5)".to_string());

                for (key, value) in x.pairs.iter() {
                    assert_eq!(expected.get(key.string().as_str()).unwrap(),
                               &value.string());
                }
            }
        }
    }

    #[test]
    fn it_should_parse_operator_with_precedence() {
        let expects = [("-a * b", "((-a) * b)"),
                       ("!-a", "(!(-a))"),
                       ("a + b + c", "((a + b) + c)"),
                       ("a + b - c", "((a + b) - c)"),
                       ("a * b * c", "((a * b) * c)"),
                       ("a * b / c", "((a * b) / c)"),
                       ("a + b / c", "(a + (b / c))"),
                       ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
                       ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
                       ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
                       ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
                       ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
                       ("true", "true"),
                       ("false", "false"),
                       ("3 > 5 == false", "((3 > 5) == false)"),
                       ("3 < 5 == true", "((3 < 5) == true)"),
                       ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
                       ("(5 + 5) * 2", "((5 + 5) * 2)"),
                       ("2 / (5 + 5)", "(2 / (5 + 5))"),
                       ("-(5 + 5)", "(-(5 + 5))"),
                       ("!(true == true)", "(!(true == true))"),
                       ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
                       ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
                       ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
                       ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
                       ("add(a * b[2], b[1], 2 * [1, 2][1])",
                        "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))")];

        for expect in expects.iter() {
            let l = lexer::Lexer::new(expect.0.to_string());

            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            let actual = program.to_enum().string();
            println!("{:?}", actual);
            assert_eq!(actual, expect.1);
        }
    }
}

