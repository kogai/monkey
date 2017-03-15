use std::mem;
use token::{Token, TokenType};
use lexer::Lexer;
use ast::{Program, Statement, Expression, LetStatement, Identifier, EmptyExpression, Node};

#[derive(Debug, Clone)]
struct Parser {
  lexer: Lexer,
  current_token: Token,
  peek_token: Token,
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
      _ => None,
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

    if !self.current_token_is(TokenType::SEMICOLON) {
      self.next_token();
    }

    let stmt = LetStatement {
      name: name,
      token: current_token,
      value: Box::new(EmptyExpression {}),
    };

    Some(stmt)
  }

  fn current_token_is(&self, t: TokenType) -> bool {
      self.current_token.token_type == t
  }

  fn peek_token_is(&self, t: TokenType) -> bool {
      self.peek_token.token_type == t
  }

  fn expect_peek_token(&mut self, t: TokenType) -> bool {
    let is_expect_token = self.peek_token_is(t);
    if is_expect_token {
      self.next_token();
    };
    is_expect_token
  }
}

fn new(mut lexer: Lexer) -> Parser {
  let first = lexer.next_token();
  let second = lexer.next_token();
  Parser {
    lexer: lexer,
    current_token: first,
    peek_token: second,
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
}
