use crate::ast::node::Node;
use crate::ast::parameter::Parameter;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Node>,
    pub parameters: Vec<Parameter>,
}

impl Into<Node> for CallExpression {
    fn into(self) -> Node {
        Node::CallExpressionNode(self)
    }
}

impl GetPosition for CallExpression {
    fn position(&self) -> &Position {
        self.callee.position()
    }
}