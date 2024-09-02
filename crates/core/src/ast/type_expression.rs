use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpression {
    pub name: String,
    pub range: Range<Position>,
}

impl From<TypeExpression> for Node {
    fn from(value: TypeExpression) -> Node {
        Node::TypeExpression(value)
    }
}

impl GetRange for TypeExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
