use crate::ast::function_interface::FunctionInterface;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub interface: FunctionInterface,
    pub body: Vec<Node>,
}

impl GetPosition for Function {
    fn position(&self) -> &Position {
        &self.interface.position
    }
}