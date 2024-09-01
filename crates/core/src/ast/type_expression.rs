use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpression {
    pub name: String,
    pub range: Range,
}

impl From<TypeExpression> for Node {
    fn from(value: TypeExpression) -> Node {
        Node::TypeExpression(value)
    }
}

impl GetRange for TypeExpression {
    fn range(&self) -> Range {
        self.range
    }
}
