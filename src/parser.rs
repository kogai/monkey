use token::Token;
use lexer::Lexer;
use ast::{Program, Statement};

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

  fn parse_program<T: Statement>(&self) -> Program<T> {
    unimplemented!();
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
  fn it_should_parse_statements() {
    let l = lexer::new("
      let x = 5;
      let y = 10;
      let foobar = 838383;
    ".to_string());

    let parser = new(l);
    let program = parser.parse_program();
    assert_eq!(program.statements.len(), 3);

    let expects = [
      ("x", 0),
      ("y", 1),
      ("foobar", 2),
    ];

    for expect in expects.iter() {
      let i = expect.1;
      let s = program.statements[i];
      assert_eq!(s.token_literal(), "let");
      assert_eq!(s.name.value, expect.0);
      assert_eq!(s.name.token_literal(), expect.0);
    }
  }
}
