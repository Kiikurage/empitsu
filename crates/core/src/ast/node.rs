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
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::program::Program;
use crate::ast::property_declaration::PropertyDeclaration;
use crate::ast::return_::Return;
use crate::ast::string_literal::StringLiteral;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::ast::unary_expression::UnaryExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::position::{pos, Position};
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

    // Node factories

    pub fn program_node(statements: Vec<Node>) -> Node {
        Node::Program(Node::program(statements))
    }

    pub fn if_statement_node(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Option<Node>) -> Node {
        Node::IfStatement(Node::if_statement(range, condition, true_branch, false_branch))
    }

    pub fn for_statement_node(range: Range<Position>, variable: Identifier, iterable: Node, body: Node) -> Node {
        Node::ForStatement(Node::for_statement(range, variable, iterable, body))
    }

    pub fn variable_declaration_node(range: Range<Position>, name: Identifier, type_: Option<TypeExpression>, initializer: Option<Node>) -> Node {
        Node::VariableDeclaration(Node::variable_declaration(range, name, type_, initializer))
    }

    pub fn function_declaration_node(
        range: Range<Position>,
        interface: FunctionInterface,
        body: Vec<Node>,
    ) -> Node {
        Node::FunctionDeclaration(Node::function(range, interface, body))
    }

    pub fn struct_declaration_node(
        range: Range<Position>,
        name: Identifier,
        properties: Vec<PropertyDeclaration>,
        instance_methods: Vec<Function>,
        static_methods: Vec<Function>,
    ) -> Node {
        Node::StructDeclaration(Node::struct_declaration(range, name, properties, instance_methods, static_methods))
    }

    pub fn interface_declaration_node(range: Range<Position>, name: Identifier, instance_methods: Vec<FunctionInterface>) -> Node {
        Node::InterfaceDeclaration(Node::interface_declaration(range, name, instance_methods))
    }

    pub fn impl_statement_node(
        range: Range<Position>,
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
    ) -> Node {
        Node::ImplStatement(Node::impl_statement(range, struct_name, interface_name, instance_methods))
    }

    pub fn return_node(range: Range<Position>, value: Option<Node>) -> Node {
        Node::Return(Node::return_(range, value))
    }

    pub fn break_node(range: Range<Position>) -> Node {
        Node::Break(Node::break_(range))
    }

    pub fn function_expression_node(
        range: Range<Position>,
        interface: FunctionInterface,
        body: Vec<Node>,
    ) -> Node {
        Node::FunctionExpression(Node::function(range, interface, body))
    }

    pub fn if_expression_node(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Node) -> Node {
        Node::IfExpression(Node::if_expression(range, condition, true_branch, false_branch))
    }

    pub fn block_node(range: Range<Position>, nodes: Vec<Node>) -> Node {
        Node::Block(Node::block(range, nodes))
    }

    pub fn assignment_expression_node(lhs: Node, rhs: Node) -> Node {
        Node::AssignmentExpression(Node::assignment_expression(lhs, rhs))
    }

    pub fn binary_expression_node(lhs: Node, operator: PunctuationKind, rhs: Node) -> Node {
        Node::BinaryExpression(Node::binary_expression(lhs, operator, rhs))
    }

    pub fn unary_expression_node(range: Range<Position>, operator: PunctuationKind, operand: Node) -> Node {
        Node::UnaryExpression(Node::unary_expression(range, operator, operand))
    }

    pub fn call_expression_node(callee: Node, parameters: Vec<Parameter>) -> Node {
        Node::CallExpression(Node::call_expression(callee, parameters))
    }

    pub fn member_expression_node(object: Node, member: Identifier) -> Node {
        Node::MemberExpression(Node::member_expression(object, member))
    }

    pub fn identifier_node(range: Range<Position>, name: impl Into<String>) -> Node {
        Node::Identifier(Node::identifier(range, name))
    }

    pub fn number_literal_node(range: Range<Position>, value: f64) -> Node {
        Node::NumberLiteral(Node::number_literal(range, value))
    }

    pub fn bool_literal_node(range: Range<Position>, value: bool) -> Node {
        Node::BoolLiteral(Node::bool_literal(range, value))
    }

    pub fn string_literal_node(range: Range<Position>, value: impl Into<String>) -> Node {
        Node::StringLiteral(Node::string_literal(range, value))
    }

    pub fn type_expression_node(range: Range<Position>, name: impl Into<String>) -> Node {
        Node::TypeExpression(Node::type_expression(range, name))
    }

    // Internal model factories

    pub fn program(statements: Vec<Node>) -> Program {
        if statements.is_empty() {
            return Program {
                statements: vec![],
                range: pos(0, 0)..pos(0, 0),
            };
        }

        let start = statements.first().unwrap().start();
        let end = statements.first().unwrap().end();

        Program { statements, range: start..end }
    }

    pub fn if_statement(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Option<Node>) -> IfStatement {
        IfStatement {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: false_branch.map(Box::new),
            range,
        }
    }

    pub fn for_statement(range: Range<Position>, variable: Identifier, iterable: Node, body: Node) -> ForStatement {
        ForStatement {
            variable,
            iterable: Box::new(iterable),
            body: Box::new(body),
            range,
        }
    }

    pub fn variable_declaration(range: Range<Position>, name: Identifier, type_: Option<TypeExpression>, initializer: Option<Node>) -> VariableDeclaration {
        VariableDeclaration {
            name,
            type_,
            initializer: initializer.map(Box::new),
            range,
        }
    }

    pub fn function(
        range: Range<Position>,
        interface: FunctionInterface,
        body: Vec<Node>,
    ) -> Function {
        Function {
            interface,
            body,
            range,
        }
    }

    pub fn function_interface(
        range: Range<Position>,
        name: Option<Identifier>,
        parameters: Vec<ParameterDeclaration>,
        return_type: TypeExpression,
    ) -> FunctionInterface {
        FunctionInterface {
            name,
            parameters,
            return_type: Box::new(return_type),
            range,
        }
    }

    pub fn parameter_declaration(name: Identifier, type_: TypeExpression) -> ParameterDeclaration {
        ParameterDeclaration { name, type_ }
    }

    pub fn struct_declaration(
        range: Range<Position>,
        name: Identifier,
        properties: Vec<PropertyDeclaration>,
        instance_methods: Vec<Function>,
        static_methods: Vec<Function>,
    ) -> StructDeclaration {
        StructDeclaration {
            name,
            properties,
            instance_methods,
            static_methods,
            range,
        }
    }

    pub fn property_declaration(name: Identifier, type_: TypeExpression) -> PropertyDeclaration {
        PropertyDeclaration { name, type_ }
    }

    pub fn interface_declaration(range: Range<Position>, name: Identifier, instance_methods: Vec<FunctionInterface>) -> InterfaceDeclaration {
        InterfaceDeclaration {
            name,
            instance_methods,
            range,
        }
    }

    pub fn impl_statement(
        range: Range<Position>,
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
    ) -> ImplStatement {
        ImplStatement {
            struct_name,
            interface_name,
            instance_methods,
            range,
        }
    }

    pub fn return_(range: Range<Position>, value: Option<Node>) -> Return {
        Return {
            value: value.map(Box::new),
            range,
        }
    }

    pub fn break_(range: Range<Position>) -> Break {
        Break {
            range,
        }
    }

    pub fn if_expression(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Node) -> IfExpression {
        IfExpression {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
            range,
        }
    }

    pub fn block(range: Range<Position>, nodes: Vec<Node>) -> Block {
        Block {
            nodes,
            range,
        }
    }

    pub fn assignment_expression(lhs: Node, rhs: Node) -> AssignmentExpression {
        AssignmentExpression {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn binary_expression(lhs: Node, operator: PunctuationKind, rhs: Node) -> BinaryExpression {
        BinaryExpression {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    }

    pub fn unary_expression(range: Range<Position>, operator: PunctuationKind, operand: Node) -> UnaryExpression {
        UnaryExpression {
            operator,
            operand: Box::new(operand),
            range,
        }
    }

    pub fn call_expression(callee: Node, parameters: Vec<Parameter>) -> CallExpression {
        CallExpression {
            callee: Box::new(callee),
            parameters,
        }
    }

    pub fn parameter(name: Option<Identifier>, value: Node) -> Parameter {
        Parameter {
            name,
            value: Box::new(value),
        }
    }

    pub fn member_expression(object: Node, member: Identifier) -> MemberExpression {
        MemberExpression {
            object: Box::new(object),
            member,
        }
    }

    pub fn identifier(range: Range<Position>, name: impl Into<String>) -> Identifier {
        Identifier {
            name: name.into(),
            range,
        }
    }

    pub fn number_literal(range: Range<Position>, value: f64) -> NumberLiteral {
        NumberLiteral {
            value,
            range,
        }
    }

    pub fn bool_literal(range: Range<Position>, value: bool) -> BoolLiteral {
        BoolLiteral {
            value,
            range,
        }
    }

    pub fn string_literal(range: Range<Position>, value: impl Into<String>) -> StringLiteral {
        StringLiteral {
            value: value.into(),
            range,
        }
    }

    pub fn type_expression(range: Range<Position>, name: impl Into<String>) -> TypeExpression {
        TypeExpression {
            name: name.into(),
            range,
        }
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
