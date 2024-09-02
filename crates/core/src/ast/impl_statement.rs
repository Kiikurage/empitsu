use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct ImplStatement {
    range: Range<Position>,
    pub struct_name: Identifier,
    pub interface_name: Identifier,
    pub instance_methods: Vec<Function>,
}

impl ImplStatement {
    pub fn new(
        range: Range<Position>,
        struct_name: Identifier,
        interface_name: Identifier,
        instance_methods: Vec<Function>,
    ) -> Self {
        Self {
            range,
            struct_name,
            interface_name,
            instance_methods,
        }
    }
}

impl GetRange for ImplStatement {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
