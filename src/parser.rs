use token::Token;
use lexer::Lexer;
use ast;

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

  fn parse_program(&self) {
    unimplemented!();
  }
}

fn new(lexer: &mut Lexer) -> Parser {
  let first = lexer.next_token();
  let second = lexer.next_token();
  Parser {
    lexer: lexer.clone(),
    current_token: first,
    peek_token: second,
  }
}
