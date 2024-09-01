use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub object: Box<Node>,
    pub member: Identifier,
}

impl From<MemberExpression> for Node {
    fn from(value: MemberExpression) -> Node {
        Node::MemberExpression(value)
    }
}

impl GetPosition for MemberExpression {
    fn position(&self) -> &Position {
        self.object.position()
    }
}