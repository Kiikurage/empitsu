use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    BlockStatement(Vec<Node>),
    ForStatement(String, Box<Node>, Box<Node>), // variable, iterator, body
    ReturnStatement(Option<Box<Node>>), // return value
    BreakStatement,
    VariableDeclaration(String, Option<Box<Node>>), // name, initial_value
    FunctionDeclaration(String, Vec<String>, Box<Node>), // name, parameters, body

    // Expressions
    ReturnExpression(Option<Box<Node>>), // return value
    BreakExpression,
    FunctionExpression(Option<String>, Vec<String>, Box<Node>), // name, parameters, body
    IfExpression(Box<Node>, Box<Node>, Box<Node>), // condition, true-branch, false-branch
    BlockExpression(Vec<Node>),
    RangeIterator(Box<Node>, Box<Node>), // temporary
    AssignmentExpression(String, Box<Node>),
    BinaryExpression(Box<Node>, PunctuatorKind, Box<Node>),
    UnaryExpression(PunctuatorKind, Box<Node>),
    CallExpression(Box<Node>, Vec<Node>),
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
}

