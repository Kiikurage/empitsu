use crate::ast::assignment_expression::AssignmentExpression;
use crate::ast::binary_expression::BinaryExpression;
use crate::ast::block::Block;
use crate::ast::bool_literal::BoolLiteral;
use crate::ast::break_expression::BreakExpression;
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
use crate::ast::return_expression::ReturnExpression;
use crate::ast::string_literal::StringLiteral;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::ast::unary_expression::UnaryExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;

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
    ReturnExpression(ReturnExpression),
    BreakExpression(BreakExpression),
    FunctionExpression(Function),
    IfExpression(IfExpression),
    BlockExpression(Block),
    AssignmentExpression(AssignmentExpression),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    CallExpression(CallExpression),
    MemberExpression(MemberExpression),
    Identifier(Identifier),
    NumberLiteral(NumberLiteral),
    BoolLiteral(BoolLiteral),
    StringLiteral(StringLiteral),
}

impl Node {

    // Node factories

    pub fn program_node(statements: Vec<impl Into<Node>>) -> Node {
        Node::Program(Node::program(statements))
    }

    pub fn if_statement_node(position: impl Into<Position>, condition: impl Into<Node>, true_branch: impl Into<Node>, false_branch: Option<impl Into<Node>>) -> Node {
        Node::IfStatement(Node::if_statement(position, condition, true_branch, false_branch))
    }

    pub fn for_statement_node(variable: Identifier, iterable: impl Into<Node>, body: impl Into<Node>, position: impl Into<Position>) -> Node {
        Node::ForStatement(Node::for_statement(position, variable, iterable, body))
    }

    pub fn variable_declaration_node(name: Identifier, type_: Option<TypeExpression>, initializer: Option<impl Into<Node>>, position: impl Into<Position>) -> Node {
        Node::VariableDeclaration(Node::variable_declaration(position, name, type_, initializer))
    }

    pub fn function_declaration_node(
        interface: FunctionInterface,
        body: Vec<impl Into<Node>>,
    ) -> Node {
        Node::FunctionDeclaration(Node::function(interface, body))
    }

    pub fn struct_declaration_node(
        name: Identifier,
        properties: Vec<PropertyDeclaration>,
        instance_methods: Vec<Function>,
        static_methods: Vec<Function>,
        position: impl Into<Position>,
    ) -> Node {
        Node::StructDeclaration(Node::struct_declaration(position, name, properties, instance_methods, static_methods))
    }

    pub fn interface_declaration_node(name: Identifier, instance_methods: Vec<FunctionInterface>, position: impl Into<Position>) -> Node {
        Node::InterfaceDeclaration(Node::interface_declaration(position, name, instance_methods))
    }

    pub fn impl_statement_node(
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
        position: impl Into<Position>,
    ) -> Node {
        Node::ImplStatement(Node::impl_statement(position, struct_name, interface_name, instance_methods))
    }

    pub fn return_expression_node(value: Option<impl Into<Node>>, position: impl Into<Position>) -> Node {
        Node::ReturnExpression(Node::return_expression(position, value))
    }

    pub fn break_expression_node(position: impl Into<Position>) -> Node {
        Node::BreakExpression(Node::break_expression(position))
    }

    pub fn function_expression_node(
        interface: FunctionInterface,
        body: Vec<impl Into<Node>>,
    ) -> Node {
        Node::FunctionExpression(Node::function(interface, body))
    }

    pub fn if_expression_node(condition: impl Into<Node>, true_branch: impl Into<Node>, false_branch: impl Into<Node>, position: impl Into<Position>) -> Node {
        Node::IfExpression(Node::if_expression(position, condition, true_branch, false_branch))
    }

    pub fn block_expression_node(nodes: Vec<impl Into<Node>>, position: impl Into<Position>) -> Node {
        Node::BlockExpression(Node::block(position, nodes))
    }

    pub fn assignment_expression_node(lhs: impl Into<Node>, rhs: impl Into<Node>) -> Node {
        Node::AssignmentExpression(Node::assignment_expression(lhs, rhs))
    }

    pub fn binary_expression_node(lhs: impl Into<Node>, operator: PunctuationKind, rhs: impl Into<Node>) -> Node {
        Node::BinaryExpression(Node::binary_expression(lhs, operator, rhs))
    }

    pub fn unary_expression_node(operator: PunctuationKind, operand: impl Into<Node>, position: impl Into<Position>) -> Node {
        Node::UnaryExpression(Node::unary_expression(position, operator, operand))
    }

    pub fn call_expression_node(callee: impl Into<Node>, parameters: Vec<Parameter>) -> Node {
        Node::CallExpression(Node::call_expression(callee, parameters))
    }

    pub fn member_expression_node(object: impl Into<Node>, member: Identifier) -> Node {
        Node::MemberExpression(Node::member_expression(object, member))
    }

    pub fn identifier_node(name: impl Into<String>, position: impl Into<Position>) -> Node {
        Node::Identifier(Node::identifier(position, name))
    }

    pub fn number_literal_node(value: f64, position: impl Into<Position>) -> Node {
        Node::NumberLiteral(Node::number_literal(position, value))
    }

    pub fn bool_literal_node(value: bool, position: impl Into<Position>) -> Node {
        Node::BoolLiteral(Node::bool_literal(position, value))
    }

    pub fn string_literal_node(value: impl Into<String>, position: impl Into<Position>) -> Node {
        Node::StringLiteral(Node::string_literal(position, value))
    }

    // Internal model factories

    pub fn program(statements: Vec<impl Into<Node>>) -> Program {
        Program {
            statements: statements.into_iter().map(Into::into).collect(),
            position: Position::new(0, 0),
        }
    }

    pub fn if_statement(position: impl Into<Position>, condition: impl Into<Node>, true_branch: impl Into<Node>, false_branch: Option<impl Into<Node>>) -> IfStatement {
        IfStatement {
            condition: Box::new(condition.into()),
            true_branch: Box::new(true_branch.into()),
            false_branch: false_branch.map(Into::into).map(Box::new),
            position: position.into(),
        }
    }

    pub fn for_statement(position: impl Into<Position>, variable: Identifier, iterable: impl Into<Node>, body: impl Into<Node>) -> ForStatement {
        ForStatement {
            variable,
            iterable: Box::new(iterable.into()),
            body: Box::new(body.into()),
            position: position.into(),
        }
    }

    pub fn variable_declaration(position: impl Into<Position>, name: Identifier, type_: Option<TypeExpression>, initializer: Option<impl Into<Node>>) -> VariableDeclaration {
        VariableDeclaration {
            name,
            type_,
            initializer: initializer.map(Into::into).map(Box::new),
            position: position.into(),
        }
    }

    pub fn function(
        interface: FunctionInterface,
        body: Vec<impl Into<Node>>,
    ) -> Function {
        Function {
            interface,
            body: body.into_iter().map(Into::into).collect(),
        }
    }

    pub fn function_interface(
        position: impl Into<Position>,
        name: Option<Identifier>,
        parameters: Vec<ParameterDeclaration>,
        return_type: TypeExpression,
    ) -> FunctionInterface {
        FunctionInterface {
            name,
            parameters,
            return_type: Box::new(return_type),
            position: position.into(),
        }
    }

    pub fn parameter_declaration(name: Identifier, type_: TypeExpression) -> ParameterDeclaration {
        ParameterDeclaration { name, type_ }
    }

    pub fn struct_declaration(
        position: impl Into<Position>,
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
            position: position.into(),
        }
    }

    pub fn property_declaration(name: Identifier, type_: TypeExpression) -> PropertyDeclaration {
        PropertyDeclaration { name, type_ }
    }

    pub fn interface_declaration(position: impl Into<Position>, name: Identifier, instance_methods: Vec<FunctionInterface>) -> InterfaceDeclaration {
        InterfaceDeclaration {
            name,
            instance_methods,
            position: position.into(),
        }
    }

    pub fn impl_statement(
        position: impl Into<Position>,
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
    ) -> ImplStatement {
        ImplStatement {
            struct_name,
            interface_name,
            instance_methods,
            position: position.into(),
        }
    }

    pub fn return_expression(position: impl Into<Position>, value: Option<impl Into<Node>>) -> ReturnExpression {
        ReturnExpression {
            value: value.map(Into::into).map(Box::new),
            position: position.into(),
        }
    }

    pub fn break_expression(position: impl Into<Position>) -> BreakExpression {
        BreakExpression {
            position: position.into(),
        }
    }

    pub fn if_expression(position: impl Into<Position>, condition: impl Into<Node>, true_branch: impl Into<Node>, false_branch: impl Into<Node>) -> IfExpression {
        IfExpression {
            condition: Box::new(condition.into()),
            true_branch: Box::new(true_branch.into()),
            false_branch: Box::new(false_branch.into()),
            position: position.into(),
        }
    }

    pub fn block(position: impl Into<Position>, nodes: Vec<impl Into<Node>>) -> Block {
        Block {
            nodes: nodes.into_iter().map(Into::into).collect(),
            position: position.into(),
        }
    }

    pub fn assignment_expression(lhs: impl Into<Node>, rhs: impl Into<Node>) -> AssignmentExpression {
        AssignmentExpression {
            lhs: Box::new(lhs.into()),
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn binary_expression(lhs: impl Into<Node>, operator: PunctuationKind, rhs: impl Into<Node>) -> BinaryExpression {
        BinaryExpression {
            lhs: Box::new(lhs.into()),
            operator,
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn unary_expression(position: impl Into<Position>, operator: PunctuationKind, operand: impl Into<Node>) -> UnaryExpression {
        UnaryExpression {
            operator,
            operand: Box::new(operand.into()),
            position: position.into(),
        }
    }

    pub fn call_expression(callee: impl Into<Node>, parameters: Vec<Parameter>) -> CallExpression {
        CallExpression {
            callee: Box::new(callee.into()),
            parameters,
        }
    }

    pub fn parameter(name: Option<Identifier>, value: impl Into<Node>) -> Parameter {
        Parameter {
            name,
            value: Box::new(value.into()),
        }
    }

    pub fn member_expression(object: impl Into<Node>, member: Identifier) -> MemberExpression {
        MemberExpression {
            object: Box::new(object.into()),
            member,
        }
    }

    pub fn identifier(position: impl Into<Position>, name: impl Into<String>) -> Identifier {
        Identifier {
            name: name.into(),
            position: position.into(),
        }
    }

    pub fn number_literal(position: impl Into<Position>, value: f64) -> NumberLiteral {
        NumberLiteral {
            value,
            position: position.into(),
        }
    }

    pub fn bool_literal(position: impl Into<Position>, value: bool) -> BoolLiteral {
        BoolLiteral {
            value,
            position: position.into(),
        }
    }

    pub fn string_literal(position: impl Into<Position>, value: impl Into<String>) -> StringLiteral {
        StringLiteral {
            value: value.into(),
            position: position.into(),
        }
    }
}

impl GetPosition for Node {
    fn position(&self) -> &Position {
        match self {
            Node::Program(node) => node.position(),
            Node::IfStatement(node) => node.position(),
            Node::ForStatement(node) => node.position(),
            Node::VariableDeclaration(node) => node.position(),
            Node::FunctionDeclaration(node) => node.position(),
            Node::StructDeclaration(node) => node.position(),
            Node::InterfaceDeclaration(node) => node.position(),
            Node::ImplStatement(node) => node.position(),
            Node::ReturnExpression(node) => node.position(),
            Node::BreakExpression(node) => node.position(),
            Node::FunctionExpression(node) => node.position(),
            Node::IfExpression(node) => node.position(),
            Node::BlockExpression(node) => node.position(),
            Node::AssignmentExpression(node) => node.position(),
            Node::BinaryExpression(node) => node.position(),
            Node::UnaryExpression(node) => node.position(),
            Node::CallExpression(node) => node.position(),
            Node::MemberExpression(node) => node.position(),
            Node::Identifier(node) => node.position(),
            Node::NumberLiteral(node) => node.position(),
            Node::BoolLiteral(node) => node.position(),
            Node::StringLiteral(node) => node.position(),
        }
    }
}
