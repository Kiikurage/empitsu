use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpression {
    pub object: Box<Node>,
    pub member: Identifier,
}

impl MemberExpression {
    pub fn new(object: Node, member: Identifier) -> Self {
        Self {
            object: Box::new(object),
            member,
        }
    }
}

impl GetRange for MemberExpression {
    fn range(&self) -> Range<Position> {
        self.object.start()..self.member.end()
    }
}