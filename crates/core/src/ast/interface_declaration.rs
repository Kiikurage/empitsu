use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclaration {
    range: Range<Position>,
    pub name: Identifier,
    pub instance_methods: Vec<FunctionInterface>,
}

impl InterfaceDeclaration {
    pub fn new(
        range: Range<Position>,
        name: Identifier,
        instance_methods: Vec<FunctionInterface>,
    ) -> Self {
        Self {
            range,
            name,
            instance_methods,
        }
    }
}

impl GetRange for InterfaceDeclaration {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}