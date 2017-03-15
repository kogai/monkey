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

pub struct Program {
  pub statements: Vec<Box<Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
      match self.statements.first() {
          Some(s) => s.token_literal(),
          None => "".to_string(),
      }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {
  fn expression_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub struct EmptyExpression {}
impl Node for EmptyExpression {
    fn token_literal(&self) -> String {
        "empty".to_string()
    }
}

impl Expression for EmptyExpression {
    fn expression_node(&self) {}
}