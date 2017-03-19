use std::clone::Clone;
use token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
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

    fn string(&self) -> String {
        let statemnts = &self.statements;

        statemnts.into_iter()
            .fold("".to_string(), |acc, s| format!("{}{}", acc, s.string()))
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let statemnts = &self.statements;
        statemnts.into_iter()
            .fold("".to_string(), |acc, s| format!("{}{}", acc, s.string()))
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
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

    fn string(&self) -> String {
        format!("{} {} = {}",
                self.token_literal(),
                self.name.string(),
                self.value.string())
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("{} {};", self.token_literal(), self.return_value.string())
    }
}

impl Statement for ReturnStatement {
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

    fn string(&self) -> String {
        format!("{}", self.value)
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

    fn string(&self) -> String {
        format!("{}", self.token_literal())
    }
}

impl Expression for EmptyExpression {
    fn expression_node(&self) {}
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("{}", self.expression.string())
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: usize,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("{}", self.value)
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("({}{})", self.operator, self.right.string())
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("({} {} {})",
                self.left.string(),
                self.operator,
                self.right.string())
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("{}", self.value)
    }
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        match self.alternative {
            Some(ref x) => {
                format!("if {} {} else {}",
                        self.condition.string(),
                        self.consequence.string(),
                        x.string())
            }
            None => {
                format!("if {} {}",
                        self.condition.string(),
                        self.consequence.string())
            }
        }
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let parameters = &self.parameters;
        let parameters_string =
            parameters.into_iter().map(|p| p.string()).collect::<Vec<String>>().join(", ");
        format!("{}({}) {}",
                self.token_literal(),
                parameters_string,
                self.body.string())
    }
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

