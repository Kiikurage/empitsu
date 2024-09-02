use crate::ast::assignment_expression::AssignmentExpression;
use crate::ast::binary_expression::BinaryExpression;
use crate::ast::block::Block;
use crate::ast::bool_literal::BoolLiteral;
use crate::ast::break_::Break;
use crate::ast::call_expression::CallExpression;
use crate::ast::for_statement::ForStatement;
use crate::ast::function::Function;
use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression::IfExpression;
use crate::ast::if_statement::IfStatement;
use crate::ast::impl_statement::ImplStatement;
use crate::ast::interface_declaration::InterfaceDeclaration;
use crate::ast::member_expression::MemberExpression;
use crate::ast::number_literal::NumberLiteral;
use crate::ast::parameter::Parameter;
use crate::ast::program::Program;
use crate::ast::property_declaration::PropertyDeclaration;
use crate::ast::return_::Return;
use crate::ast::string_literal::StringLiteral;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::get_range::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::ast::unary_expression::UnaryExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Program),

    // Statements
    IfStatement(IfStatement),
    ForStatement(ForStatement),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(Function),
    StructDeclaration(StructDeclaration),
    InterfaceDeclaration(InterfaceDeclaration),
    ImplStatement(ImplStatement),

    // Expressions
    Return(Return),
    Break(Break),
    FunctionExpression(Function),
    IfExpression(IfExpression),
    Block(Block),
    AssignmentExpression(AssignmentExpression),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    CallExpression(CallExpression),
    MemberExpression(MemberExpression),
    Identifier(Identifier),
    NumberLiteral(NumberLiteral),
    BoolLiteral(BoolLiteral),
    StringLiteral(StringLiteral),

    // Type
    TypeExpression(TypeExpression),
}

impl Node {
    pub fn program(statements: Vec<Node>) -> Node {
        Node::Program(Program::new(statements))
    }

    pub fn if_statement(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Option<Node>) -> Node {
        Node::IfStatement(IfStatement::new(condition, true_branch, false_branch, range))
    }

    pub fn for_statement(range: Range<Position>, variable: Identifier, iterable: Node, body: Node) -> Node {
        Node::ForStatement(ForStatement::new(variable, iterable, body, range))
    }

    pub fn variable_declaration(range: Range<Position>, name: Identifier, type_: Option<TypeExpression>, initializer: Option<Node>) -> Node {
        Node::VariableDeclaration(VariableDeclaration::new(name, type_, initializer, range))
    }

    pub fn function_declaration(
        range: Range<Position>,
        interface: FunctionInterface,
        body: Vec<Node>,
    ) -> Node {
        Node::FunctionDeclaration(Function::new(range, interface, body))
    }

    pub fn struct_declaration(
        range: Range<Position>,
        name: Identifier,
        properties: Vec<PropertyDeclaration>,
        instance_methods: Vec<Function>,
        static_methods: Vec<Function>,
    ) -> Node {
        Node::StructDeclaration(StructDeclaration::new(range, name, properties, instance_methods, static_methods))
    }

    pub fn interface_declaration(range: Range<Position>, name: Identifier, instance_methods: Vec<FunctionInterface>) -> Node {
        Node::InterfaceDeclaration(InterfaceDeclaration::new(range, name, instance_methods))
    }

    pub fn impl_statement(
        range: Range<Position>,
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
    ) -> Node {
        Node::ImplStatement(ImplStatement::new(range, struct_name, interface_name, instance_methods))
    }

    pub fn return_(range: Range<Position>, value: Option<Node>) -> Node {
        Node::Return(Return::new(range, value))
    }

    pub fn break_(range: Range<Position>) -> Node {
        Node::Break(Break::new(range))
    }

    pub fn function_expression(
        range: Range<Position>,
        interface: FunctionInterface,
        body: Vec<Node>,
    ) -> Node {
        Node::FunctionExpression(Function::new(range, interface, body))
    }

    pub fn if_expression(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Node) -> Node {
        Node::IfExpression(IfExpression::new(range, condition, true_branch, false_branch))
    }

    pub fn block(range: Range<Position>, nodes: Vec<Node>) -> Node {
        Node::Block(Block::new(range, nodes))
    }

    pub fn assignment_expression(lhs: Node, rhs: Node) -> Node {
        Node::AssignmentExpression(AssignmentExpression::new(lhs, rhs))
    }

    pub fn binary_expression(lhs: Node, operator: PunctuationKind, rhs: Node) -> Node {
        Node::BinaryExpression(BinaryExpression::new(lhs, operator, rhs))
    }

    pub fn unary_expression(range: Range<Position>, operator: PunctuationKind, operand: Node) -> Node {
        Node::UnaryExpression(UnaryExpression::new(range, operator, operand))
    }

    pub fn call_expression(callee: Node, parameters: Vec<Parameter>) -> Node {
        Node::CallExpression(CallExpression::new(callee, parameters))
    }

    pub fn member_expression(object: Node, member: Identifier) -> Node {
        Node::MemberExpression(MemberExpression::new(object, member))
    }

    pub fn identifier(range: Range<Position>, name: impl Into<String>) -> Node {
        Node::Identifier(Identifier::new(range, name))
    }

    pub fn number_literal(range: Range<Position>, value: f64) -> Node {
        Node::NumberLiteral(NumberLiteral::new(range, value))
    }

    pub fn bool_literal(range: Range<Position>, value: bool) -> Node {
        Node::BoolLiteral(BoolLiteral::new(range, value))
    }

    pub fn string_literal(range: Range<Position>, value: impl Into<String>) -> Node {
        Node::StringLiteral(StringLiteral::new(range, value))
    }

    pub fn type_expression(range: Range<Position>, name: impl Into<String>) -> Node {
        Node::TypeExpression(TypeExpression::new(range, name))
    }
}

impl GetRange for Node {
    fn range(&self) -> Range<Position> {
        match self {
            Node::Program(node) => node.range(),
            Node::IfStatement(node) => node.range(),
            Node::ForStatement(node) => node.range(),
            Node::VariableDeclaration(node) => node.range(),
            Node::FunctionDeclaration(node) => node.range(),
            Node::StructDeclaration(node) => node.range(),
            Node::InterfaceDeclaration(node) => node.range(),
            Node::ImplStatement(node) => node.range(),
            Node::Return(node) => node.range(),
            Node::Break(node) => node.range(),
            Node::FunctionExpression(node) => node.range(),
            Node::IfExpression(node) => node.range(),
            Node::Block(node) => node.range(),
            Node::AssignmentExpression(node) => node.range(),
            Node::BinaryExpression(node) => node.range(),
            Node::UnaryExpression(node) => node.range(),
            Node::CallExpression(node) => node.range(),
            Node::MemberExpression(node) => node.range(),
            Node::Identifier(node) => node.range(),
            Node::NumberLiteral(node) => node.range(),
            Node::BoolLiteral(node) => node.range(),
            Node::StringLiteral(node) => node.range(),
            Node::TypeExpression(node) => node.range(),
        }
    }
}
