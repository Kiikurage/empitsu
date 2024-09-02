use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Option<Identifier>,
    pub value: Box<Node>,
}

impl Parameter {
    pub fn new(name: Option<Identifier>, value: Node) -> Self {
        Self {
            name,
            value: Box::new(value),
        }
    }
}

impl GetRange for Parameter {
    fn range(&self) -> Range<Position> {
        if let Some(name) = &self.name {
            name.range()
        } else {
            self.value.range()
        }
    }
}