use crate::punctuator_kind::PunctuatorKind;
use crate::type_::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    ForStatement(String, Box<Node>, Box<Node>), // variable, iterator, body
    VariableDeclaration(String, Option<Type>, Option<Box<Node>>), // name, type, initial_value
    FunctionDeclaration(String, Vec<FunctionParameterDefinition>, Box<Node>), // name, parameters, body

    // Expressions
    ReturnExpression(Option<Box<Node>>), // return value
    BreakExpression,
    FunctionExpression(Option<String>, Vec<FunctionParameterDefinition>, Box<Node>), // name, parameters, body
    IfExpression(Box<Node>, Box<Node>, Box<Node>), // condition, true-branch, false-branch
    BlockExpression(Vec<Node>),
    RangeIterator(Box<Node>, Box<Node>), // temporary
    AssignmentExpression(Box<Node>, Box<Node>),
    BinaryExpression(Box<Node>, PunctuatorKind, Box<Node>),
    UnaryExpression(PunctuatorKind, Box<Node>),
    CallExpression(Box<Node>, Vec<Node>),
    MemberExpression(Box<Node>, Box<Node>),
    Number(f64),
    Bool(bool),
    String(String),
    Object(Vec<Node>), // list of property definitions
    ObjectPropertyDefinition(Box<Node>, Box<Node>), // name, value
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameterDefinition {
    pub name: String,
    pub type_: Type,
}