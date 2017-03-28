use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
    fn to_ast(&self) -> AST;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AST {
    Program(Program),
    BlockStatement(BlockStatement),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IndexExpression(IndexExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    fn to_ast(&self) -> AST {
        match self {
            &Nodes::Program(ref x) => AST::Program(x.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statements {
    BlockStatement(BlockStatement),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statements {
    pub fn new_let_statement(x: LetStatement) -> Self {
        Statements::LetStatement(x)
    }
    pub fn new_return_statement(x: ReturnStatement) -> Self {
        Statements::ReturnStatement(x)
    }
    pub fn new_expression_statement(x: ExpressionStatement) -> Self {
        Statements::ExpressionStatement(x)
    }
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

    fn to_ast(&self) -> AST {
        match self {
            &Statements::BlockStatement(ref x) => AST::BlockStatement(x.clone()),
            &Statements::LetStatement(ref x) => AST::LetStatement(x.clone()),
            &Statements::ReturnStatement(ref x) => AST::ReturnStatement(x.clone()),
            &Statements::ExpressionStatement(ref x) => AST::ExpressionStatement(x.clone()),
        }
    }
}

impl Statement for Statements {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expressions {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IndexExpression(IndexExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Expressions {
    pub fn new_identifier(x: Identifier) -> Self {
        Expressions::Identifier(x)
    }
    pub fn new_integer_literal(x: IntegerLiteral) -> Self {
        Expressions::IntegerLiteral(x)
    }
    pub fn new_string_literal(x: StringLiteral) -> Self {
        Expressions::StringLiteral(x)
    }
    pub fn new_array_literal(x: ArrayLiteral) -> Expressions {
        Expressions::ArrayLiteral(x)
    }
    pub fn new_prefix_expression(x: PrefixExpression) -> Self {
        Expressions::PrefixExpression(x)
    }
    pub fn new_infix_expression(x: InfixExpression) -> Self {
        Expressions::InfixExpression(x)
    }
    pub fn new_boolean(x: Boolean) -> Self {
        Expressions::Boolean(x)
    }
    pub fn new_if_expression(x: IfExpression) -> Self {
        Expressions::IfExpression(x)
    }
    pub fn new_function_literal(x: FunctionLiteral) -> Self {
        Expressions::FunctionLiteral(x)
    }
    pub fn new_call_expression(x: CallExpression) -> Self {
        Expressions::CallExpression(x)
    }
}

impl Node for Expressions {
    fn token_literal(&self) -> String {
        match self {
            &Expressions::Identifier(ref x) => x.token.literal.clone(),
            &Expressions::IntegerLiteral(ref x) => x.token.literal.clone(),
            &Expressions::StringLiteral(ref x) => x.token.literal.clone(),
            &Expressions::ArrayLiteral(ref x) => x.token.literal.clone(),
            &Expressions::HashLiteral(ref x) => x.token.literal.clone(),
            &Expressions::PrefixExpression(ref x) => x.token.literal.clone(),
            &Expressions::InfixExpression(ref x) => x.token.literal.clone(),
            &Expressions::Boolean(ref x) => x.token.literal.clone(),
            &Expressions::IfExpression(ref x) => x.token.literal.clone(),
            &Expressions::FunctionLiteral(ref x) => x.token.literal.clone(),
            &Expressions::CallExpression(ref x) => x.token.literal.clone(),
            &Expressions::IndexExpression(ref x) => x.token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            &Expressions::Identifier(ref x) => x.value.clone(),
            &Expressions::IntegerLiteral(ref x) => format!("{}", x.value),
            &Expressions::StringLiteral(ref x) => x.value.clone(),
            &Expressions::ArrayLiteral(ref x) => {
                let elements = (&x.elements)
                    .into_iter()
                    .map(|p| p.string())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("[{}]", elements)
            }
            &Expressions::HashLiteral(ref x) => {
                let mut pairs: Vec<String> = Vec::new();
                for (key, value) in x.pairs.iter() {
                    pairs.push(format!("{}: {}", key.string(), value.string()));
                }
                format!("{{{}}}", pairs.join(","))
            }
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
            &Expressions::IndexExpression(ref x) => {
                format!("({}[{}])", x.left.string(), x.index.string())
            }
        }
    }

    fn to_ast(&self) -> AST {
        match self {
            &Expressions::Identifier(ref x) => AST::Identifier(x.clone()),
            &Expressions::IntegerLiteral(ref x) => AST::IntegerLiteral(x.clone()),
            &Expressions::StringLiteral(ref x) => AST::StringLiteral(x.clone()),
            &Expressions::ArrayLiteral(ref x) => AST::ArrayLiteral(x.clone()),
            &Expressions::HashLiteral(ref x) => AST::HashLiteral(x.clone()),
            &Expressions::PrefixExpression(ref x) => AST::PrefixExpression(x.clone()),
            &Expressions::InfixExpression(ref x) => AST::InfixExpression(x.clone()),
            &Expressions::Boolean(ref x) => AST::Boolean(x.clone()),
            &Expressions::IfExpression(ref x) => AST::IfExpression(x.clone()),
            &Expressions::FunctionLiteral(ref x) => AST::FunctionLiteral(x.clone()),
            &Expressions::CallExpression(ref x) => AST::CallExpression(x.clone()),
            &Expressions::IndexExpression(ref x) => AST::IndexExpression(x.clone()),
        }
    }
}

impl Expression for Expressions {}

// impl Hash for Expressions {
//     fn hash<H>(&self, state: &mut H)
//         where H: Hasher
//     {
//         unimplemented!();
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program {
    pub statements: Vec<Statements>,
}

impl Program {
    pub fn to_enum(&self) -> Nodes {
        Nodes::Program(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statements>,
}

impl BlockStatement {
    pub fn to_enum(&self) -> Statements {
        Statements::BlockStatement(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expressions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn to_enum(&self) -> Expressions {
        Expressions::Identifier(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<Expressions>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Box<Expressions>, Box<Expressions>>,
    pub keys: Vec<Expressions>,
    pub values: Vec<Expressions>,
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.keys.hash(state);
        self.values.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expressions>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expressions>,
    pub right: Box<Expressions>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expressions>,
    pub index: Box<Expressions>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expressions>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn to_enum(&self) -> Expressions {
        Expressions::FunctionLiteral(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expressions>,
    pub arguments: Vec<Box<Expressions>>,
}

fn fold_statements(x: &Vec<Statements>) -> String {
    x.into_iter().fold("".to_string(), |acc, s| format!("{}{}", acc, s.string()))
}

