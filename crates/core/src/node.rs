use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    ForStatement(String, Box<Node>, Box<Node>), // variable, iterator, body
    VariableDeclaration(String, Option<TypeExpression>, Option<Box<Node>>), // name, type, initial_value
    FunctionDeclaration(FunctionNode), // name, parameters, body
    StructDeclaration(String, Vec<StructPropertyDeclaration>), // name, properties

    // Expressions
    ReturnExpression(Option<Box<Node>>), // return value
    BreakExpression,
    FunctionExpression(FunctionNode), // name, parameters, body
    IfExpression(Box<Node>, Box<Node>, Box<Node>), // condition, true-branch, false-branch
    BlockExpression(Vec<Node>),
    RangeIterator(Box<Node>, Box<Node>), // temporary
    AssignmentExpression(Box<Node>, Box<Node>),
    BinaryExpression(Box<Node>, PunctuationKind, Box<Node>),
    UnaryExpression(PunctuationKind, Box<Node>),
    CallExpression(Box<Node>, Vec<Node>),
    MemberExpression(Box<Node>, String),
    Identifier(String),

    // Literal
    Number(f64),
    Bool(bool),
    String(String),
    Struct(String, Vec<StructPropertyInitializer>),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionNode {
    pub name: String,
    pub parameters: Vec<FunctionParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Identifier(String),
    Optional(Box<TypeExpression>),
    Union(Vec<TypeExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameterDeclaration {
    pub name: String,
    pub type_: TypeExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPropertyDeclaration {
    pub name: String,
    pub type_: TypeExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPropertyInitializer {
    pub name: String,
    pub value: Box<Node>,
}