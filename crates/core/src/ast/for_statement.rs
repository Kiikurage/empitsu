use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: Identifier,
    pub iterable: Box<Node>,
    pub body: Box<Node>,
    pub range: Range,
}

impl From<ForStatement> for Node {
    fn from(value: ForStatement) -> Node {
        Node::ForStatement(value)
    }
}

impl GetRange for ForStatement {
    fn range(&self) -> Range {
        self.range
    }
}