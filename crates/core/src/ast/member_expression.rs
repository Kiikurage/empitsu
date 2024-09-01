use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

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

impl GetRange for MemberExpression {
    fn range(&self) -> Range {
        Range::new(self.object.start(), self.member.end())
    }
}