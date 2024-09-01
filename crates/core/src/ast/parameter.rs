use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Option<Identifier>,
    pub value: Box<Node>,
}

impl GetRange for Parameter {
    fn range(&self) -> Range {
        if let Some(name) = &self.name {
            name.range()
        } else {
            self.value.range()
        }
    }
}