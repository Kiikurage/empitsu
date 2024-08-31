use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Option<Identifier>,
    pub value: Box<Node>,
}

impl GetPosition for Parameter {
    fn position(&self) -> &Position {
        if let Some(name) = &self.name {
            name.position()
        } else {
            self.value.position()
        }
    }
}