use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct ImplStatement {
    pub struct_name: Identifier,
    pub interface_name: Identifier,
    pub instance_methods: Vec<Function>,
    pub position: Position,
}

impl From<ImplStatement> for Node {
    fn from(value: ImplStatement) -> Node {
        Node::ImplStatement(value)
    }
}

impl GetPosition for ImplStatement {
    fn position(&self) -> &Position {
        &self.position
    }
}
