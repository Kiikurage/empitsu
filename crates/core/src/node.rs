use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Vec<Node>),

    // Statements
    EmptyStatement,
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>), // condition, true-branch, false-branch
    ForStatement(ForNode),
    VariableDeclaration(String, Option<TypeExpression>, Option<Box<Node>>), // name, type, initial_value
    FunctionDeclaration(FunctionNode),
    StructDeclaration(StructDeclarationNode),
    InterfaceDeclaration(InterfaceDeclarationNode),
    ImplStatement(ImplStatementNode),

    // Expressions
    ReturnExpression(Option<Box<Node>>), // return value
    BreakExpression,
    FunctionExpression(FunctionNode), // name, parameters, body
    IfExpression(Box<Node>, Box<Node>, Box<Node>), // condition, true-branch, false-branch
    BlockExpression(BlockNode),
    AssignmentExpression(Box<Node>, Box<Node>),
    BinaryExpression(Box<Node>, PunctuationKind, Box<Node>),
    UnaryExpression(PunctuationKind, Box<Node>),
    CallExpression(Box<Node>, Vec<Parameter>),
    MemberExpression(Box<Node>, String),
    Identifier(String),
    Number(f64),
    Bool(bool),
    String(String),

    // temporary
    RangeIterator(Box<Node>, Box<Node>),
}

impl Node {
    pub fn block(nodes: Vec<Node>) -> Node {
        Node::BlockExpression(BlockNode {
            nodes,
        })
    }

    pub fn for_statement(variable: String, iterable: Node, body: Node) -> Node {
        Node::ForStatement(ForNode {
            variable,
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub variable: String,
    pub iterable: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclarationNode {
    pub name: String,
    pub properties: Vec<StructPropertyDeclaration>,
    pub instance_methods: Vec<FunctionNode>, // TODO: InstanceMethodNodeとかに変える
    pub static_methods: Vec<FunctionNode>, // TODO: StaticMethodNodeとかに変える
}

impl StructDeclarationNode {
    pub fn new(name: String,
               properties: Vec<StructPropertyDeclaration>,
               instance_methods: Vec<FunctionNode>,
               static_methods: Vec<FunctionNode>, ) -> Self {
        Self { name, properties, instance_methods, static_methods }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclarationNode {
    pub name: String,
    pub instance_methods: Vec<FunctionInterfaceNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplStatementNode {
    pub struct_name: String,
    pub interface_name: String,
    pub instance_methods: Vec<FunctionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInterfaceNode {
    pub name: String,
    pub parameters: Vec<FunctionParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionNode {
    pub interface: FunctionInterfaceNode,
    pub body: Box<Node>,
}

impl FunctionNode {
    pub fn new(interface: FunctionInterfaceNode, body: Node) -> Self {
        Self { interface, body: Box::new(body) }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockNode {
    pub nodes: Vec<Node>,
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
pub struct Parameter {
    pub name: Option<String>,
    pub value: Box<Node>,
}

impl Parameter {
    pub fn new(name: Option<String>, value: Node) -> Self {
        Self { name, value: Box::new(value) }
    }
}