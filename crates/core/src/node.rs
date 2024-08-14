use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    ForStatement(String, Box<Node>, Box<Node>), // variable, iterator, body
    VariableDeclaration(String, Option<Box<Node>>), // name, initial_value
    FunctionDeclaration(String, Vec<String>, Box<Node>), // name, parameters, body

    // Expressions
    ReturnExpression(Option<Box<Node>>), // return value
    BreakExpression,
    FunctionExpression(Option<String>, Vec<String>, Box<Node>), // name, parameters, body
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

