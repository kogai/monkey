use std::mem;
use token::{Token, TokenType};
use lexer::Lexer;
use ast::{
  Program,
  Statement,
  Expression,
  LetStatement,
  ReturnStatement,
  ExpressionStatement,
  Identifier,
  EmptyExpression,
  Node,
};

#[derive(Debug, PartialOrd, PartialEq, Ord, Eq)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

#[derive(Debug, Clone)]
struct Parser {
  lexer: Lexer,
  current_token: Token,
  peek_token: Token,
  errors: Vec<String>,
}

impl Parser {
  fn next_token(&mut self) {
    self.current_token = self.peek_token.clone();
    self.peek_token = self.lexer.next_token();
  }

  fn parse_program(&mut self) -> Program {
    let mut statements: Vec<Box<Statement>> = vec![];

    while self.current_token.token_type != TokenType::EOF {
      let statement = self.parse_statement();
      if statement.is_some() {
        statements.push(statement.unwrap());
      }
      self.next_token();
    };

    Program {
      statements: statements,
    }
  }

  fn parse_statement(&mut self) -> Option<Box<Statement>> {
    match self.current_token.token_type {
      TokenType::LET => {
        match self.parse_let_statement() {
            Some(x) => Some(Box::new(x)),
            _ => None,
        }
      },
      TokenType::RETURN => Some(Box::new(self.parse_return_statement())),
      _ => Some(Box::new(self.parse_expression_statement())),
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

  fn parse_expression(&self, precedence: Precedence) -> Box<Expression> {
    let token_type = self.current_token.token_type.clone();
    self.parse_prefix(token_type)
  }

  fn parse_prefix(&self, t: TokenType) -> Box<Expression> {
    match t {
        TokenType::IDENT(_) => self.parse_identifier(),
        _ => Box::new(EmptyExpression{}),
    }
  }

  fn parse_identifier(&self) -> Box<Expression> {
    Box::new(Identifier{
      token: self.current_token.clone(),
      value: self.current_token.literal.clone(),
    })
  }

  fn parse_return_statement(&mut self) -> ReturnStatement {
    let current_token = self.current_token.clone();
    let value = Box::new(EmptyExpression {});

    self.next_token();
    // ここにExpressionの解析が入る
    while !self.current_token_is(TokenType::SEMICOLON) {
      self.next_token();
    };

    ReturnStatement {
      token: current_token,
      return_value: value,
    }
  }

  fn parse_let_statement(&mut self) -> Option<LetStatement> {
    let current_token = self.current_token.clone();
    let ident = self.peek_token.literal.clone();

    if !self.expect_peek_token(TokenType::IDENT(ident)) {
      return None
    }

    let name = Identifier {
      token: self.current_token.clone(),
      value: self.current_token.literal.clone(),
    };

    if !self.expect_peek_token(TokenType::ASSIGN) {
      return None
    }
    
    self.next_token();
    // ここにExpressionの解析が入る

    if !self.current_token_is(TokenType::SEMICOLON) {
      self.next_token();
    }

    Some(LetStatement {
      name: name,
      token: current_token,
      value: Box::new(EmptyExpression {}),
    })
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
        },
        false => {
          self.peek_error(tok);
          false
        },
    }
  }

  fn peek_error(&mut self, t: TokenType) {
    self.errors.push(
      format!("expected next token to be {:?}, got {:?} instead", t, self.peek_token.token_type)
    );
  }
}

fn new(mut lexer: Lexer) -> Parser {
  let first = lexer.next_token();
  let second = lexer.next_token();
  Parser {
    lexer: lexer,
    current_token: first,
    peek_token: second,
    errors: vec![],
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use lexer;

  #[test]
  fn it_should_detect_token_type() {
    let l = lexer::new("let x = 5;".to_string());
    let parser = new(l);
    assert!(parser.current_token_is(TokenType::LET));
    assert!(parser.peek_token_is(TokenType::IDENT("x".to_string())));
  }

  #[test]
  fn is_should_detect_peek_token() {
    let l = lexer::new("let x = 5;".to_string());
    let mut parser = new(l);
    assert!(parser.expect_peek_token(TokenType::IDENT("x".to_string())));
    assert!(parser.expect_peek_token(TokenType::ASSIGN));
    assert!(parser.expect_peek_token(TokenType::INT("5".to_string())));
    assert!(parser.expect_peek_token(TokenType::SEMICOLON));
  }

  #[test]
  fn it_should_parse_let_statement() {
    let l = lexer::new("let x = 5;".to_string());
    let mut parser = new(l);
    let parsed = parser.parse_let_statement().unwrap();

    assert_eq!(parsed.token.token_type, TokenType::LET);
    assert_eq!(parsed.name.value, "x");
    assert_eq!(parsed.value.token_literal(), "empty");
  }

  #[test]
  fn it_should_parse_statements() {
    let l = lexer::new("
      let x = 5;
      let y = 10;
      let foobar = 838383;
    ".to_string());

    let mut parser = new(l);
    let program = parser.parse_program();
    let statements = program.statements;
    let statements_count = statements.len();

    assert_eq!(statements_count, 3);

    let expects = [
      "x",
      "y",
      "foobar",
    ];

    for i in 0..statements_count {
      let expect = expects[i];
      let statement = unsafe {
        mem::transmute::<&Box<Statement>, &Box<LetStatement>>(&statements[i])
      };

      assert_eq!(statement.token_literal(), "let");
      assert_eq!(statement.name.value, expect);
      assert_eq!(statement.name.token_literal(), expect);
    }
  }

  #[test]
  fn it_should_peek_error_syntax() {
    let l = lexer::new("
      let x 5;
      let = 10;
      let 838383;
    ".to_string());

    let mut parser = new(l);
    parser.parse_program();
    let errors_count = parser.errors.len();

    assert_eq!(errors_count, 3);
    let expects = [
      "expected next token to be ASSIGN, got INT(\"5\") instead",
      "expected next token to be IDENT(\"=\"), got ASSIGN instead",
      "expected next token to be IDENT(\"838383\"), got INT(\"838383\") instead",
    ];

    for i in 0..errors_count {
      assert_eq!(&parser.errors[i], &expects[i]);
    }
  }

  #[test]
  fn it_should_parse_return_statemtn() {
    let l = lexer::new("
      return 5;
      return 10;
      return 993322;
    ".to_string());

    let mut parser = new(l);
    let program = parser.parse_program();
    let statements = program.statements;
    let statements_count = statements.len();

    assert_eq!(statements_count, 3);

    for i in 0..statements_count {
      let statement = unsafe {
        mem::transmute::<&Box<Statement>, &Box<ReturnStatement>>(&statements[i])
      };

      assert_eq!(statement.token_literal(), "return");
    }
  }

  #[test]
  fn it_should_parse_expression_statement() {
    let l = lexer::new("
      foobar;
    ".to_string());

    let mut parser = new(l);
    let program = parser.parse_program();
    let statements = program.statements;
    let statements_count = statements.len();

    assert_eq!(statements_count, 1);
    let statement = unsafe {
      mem::transmute::<&Box<Statement>, &Box<ExpressionStatement>>(&statements[0])
    };

    let identifier = unsafe {
      mem::transmute::<&Box<Expression>, &Box<Identifier>>(&statement.expression)
    };
    assert_eq!(identifier.token_literal(), "foobar");
  }
}
