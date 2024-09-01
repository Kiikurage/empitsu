use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_: Option<TypeExpression>,
    pub initializer: Option<Box<Node>>,
    pub position: Position,
}

impl From<VariableDeclaration> for Node {
    fn from(value: VariableDeclaration) -> Node {
        Node::VariableDeclaration(value)
    }
}

impl GetPosition for VariableDeclaration {
    fn position(&self) -> &Position {
        &self.position
    }
}