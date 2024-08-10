use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    BlockStatement(Vec<Node>),
    VariableDeclaration(String, Option<Box<Node>>), // name, initial_value
    ForStatement(String, Box<Node>, Box<Node>), // variable, iterator, body

    // Expressions
    IfExpression(Box<Node>, Box<Node>, Box<Node>), // condition, true-branch, false-branch
    BlockExpression(Vec<Node>),
    RangeIterator(Box<Node>, Box<Node>), // temporary
    AssignmentExpression(String, Box<Node>),
    BinaryExpression(Box<Node>, PunctuatorKind, Box<Node>),
    UnaryExpression(PunctuatorKind, Box<Node>),
    CallExpression(Box<Node>, Vec<Node>),
    Number(f64),
    Bool(bool),
    Identifier(String),
}
