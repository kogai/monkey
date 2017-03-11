use token::Token;

pub trait Node {
  fn token_literal(&self) -> String;
}

pub trait Statement: Node {
  fn statement_node(&self);
}

pub trait Expression: Node {
  fn expression_node(&self);
}

#[derive(Debug)]
pub struct Program<T: Statement> {
  pub statements: Vec<T>,
}

impl <T: Statement> Node for Program<T> {
    fn token_literal(&self) -> String {
      match self.statements.first() {
          Some(s) => s.token_literal(),
          None => "".to_string(),
      }
    }
}

#[derive(Debug)]
struct LetStatement<T: Expression> {
    token: Token,
    name: Identifier,
    value: T,
}

impl <T: Expression> Node for LetStatement<T> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl <T: Expression> Statement for LetStatement<T> {
    fn statement_node(&self) {}
}

#[derive(Debug)]
struct Identifier {
    token: Token,
    value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {
  fn expression_node(&self) {}
}
