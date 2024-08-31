use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::property_declaration::PropertyDeclaration;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub properties: Vec<PropertyDeclaration>,
    pub instance_methods: Vec<Function>,
    pub static_methods: Vec<Function>,
    pub position: Position,
}

impl Into<Node> for StructDeclaration {
    fn into(self) -> Node {
        Node::StructDeclarationNode(self)
    }
}

impl GetPosition for StructDeclaration {
    fn position(&self) -> &Position {
        &self.position
    }
}