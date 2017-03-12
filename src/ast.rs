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
pub struct Program<S: Statement> {
  pub statements: Vec<S>,
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
struct LetStatement<E: Expression> {
    token: Token,
    name: Identifier,
    value: E,
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
