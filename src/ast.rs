use token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nodes {
    Program(Program),
}

impl Node for Nodes {
    fn token_literal(&self) -> String {
        match self {
            &Nodes::Program(ref x) => {
                match x.statements.first() {
                    Some(s) => s.token_literal(),
                    _ => "".to_string(),
                }
            }
        }
    }

    fn string(&self) -> String {
        match self {
            &Nodes::Program(ref x) => fold_statements(&x.statements),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statements {
    BlockStatement(BlockStatement),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Node for Statements {
    fn token_literal(&self) -> String {
        match self {
            &Statements::BlockStatement(ref x) => x.token.literal.clone(),
            &Statements::LetStatement(ref x) => x.token.literal.clone(),
            &Statements::ReturnStatement(ref x) => x.token.literal.clone(),
            &Statements::ExpressionStatement(ref x) => x.token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            &Statements::BlockStatement(ref x) => fold_statements(&x.statements),
            &Statements::LetStatement(ref x) => {
                format!("{} {} = {}",
                        self.token_literal(),
                        x.name.to_enum().string(),
                        x.value.string())
            }
            &Statements::ReturnStatement(ref x) => {
                format!("{} {};", self.token_literal(), x.return_value.string())
            }
            &Statements::ExpressionStatement(ref x) => format!("{}", x.expression.string()),
        }
    }
}

impl Statement for Statements {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expressions {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Node for Expressions {
    fn token_literal(&self) -> String {
        match self {
            &Expressions::Identifier(ref x) => x.token.literal.clone(),
            &Expressions::IntegerLiteral(ref x) => x.token.literal.clone(),
            &Expressions::PrefixExpression(ref x) => x.token.literal.clone(),
            &Expressions::InfixExpression(ref x) => x.token.literal.clone(),
            &Expressions::Boolean(ref x) => x.token.literal.clone(),
            &Expressions::IfExpression(ref x) => x.token.literal.clone(),
            &Expressions::FunctionLiteral(ref x) => x.token.literal.clone(),
            &Expressions::CallExpression(ref x) => x.token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            &Expressions::Identifier(ref x) => x.value.clone(),
            &Expressions::IntegerLiteral(ref x) => format!("{}", x.value),
            &Expressions::PrefixExpression(ref x) => {
                format!("({}{})", x.operator, x.right.string())
            }
            &Expressions::InfixExpression(ref x) => {
                format!("({} {} {})", x.left.string(), x.operator, x.right.string())
            }
            &Expressions::Boolean(ref x) => format!("{}", x.value),
            &Expressions::IfExpression(ref x) => {
                match x.alternative {
                    Some(ref a) => {
                        format!("if {} {} else {}",
                                x.condition.string(),
                                x.consequence.to_enum().string(),
                                a.to_enum().string())
                    }
                    None => {
                        format!("if {} {}",
                                x.condition.string(),
                                x.consequence.to_enum().string())
                    }
                }
            }
            &Expressions::FunctionLiteral(ref x) => {
                let parameters_string = (&x.parameters)
                    .into_iter()
                    .map(|p| p.to_enum().string())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{}({}) {}",
                        x.to_enum().token_literal(),
                        parameters_string,
                        x.body.to_enum().string())
            }

            &Expressions::CallExpression(ref x) => {
                let arguments_string = (&x.arguments)
                    .into_iter()
                    .map(|p| p.string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", x.function.string(), arguments_string)
            }
        }
    }
}

impl Expression for Expressions {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statements>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statements>,
}

impl BlockStatement {
    fn to_enum(&self) -> Statements {
        Statements::BlockStatement(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    fn to_enum(&self) -> Expressions {
        Expressions::Identifier(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expressions>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expressions>,
    pub right: Box<Expressions>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expressions>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    fn to_enum(&self) -> Expressions {
        Expressions::FunctionLiteral(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expressions>,
    pub arguments: Vec<Box<Expressions>>,
}

fn fold_statements(x: &Vec<Statements>) -> String {
    x.into_iter()
        .fold("".to_string(), |acc, s| format!("{}{}", acc, s.string()))
}

