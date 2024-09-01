use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclaration {
    pub name: Identifier,
    pub instance_methods: Vec<FunctionInterface>,
    pub position: Position,
}

impl From<InterfaceDeclaration> for Node {
    fn from(value: InterfaceDeclaration) -> Node {
        Node::InterfaceDeclaration(value)
    }
}

impl GetPosition for InterfaceDeclaration {
    fn position(&self) -> &Position {
        &self.position
    }
}