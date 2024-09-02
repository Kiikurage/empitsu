use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: Identifier,
    pub iterable: Box<Node>,
    pub body: Box<Node>,
    pub range: Range<Position>,
}

impl From<ForStatement> for Node {
    fn from(value: ForStatement) -> Node {
        Node::ForStatement(value)
    }
}

impl GetRange for ForStatement {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}