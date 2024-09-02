use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::property_declaration::PropertyDeclaration;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    range: Range<Position>,
    pub name: Identifier,
    pub properties: Vec<PropertyDeclaration>,
    pub instance_methods: Vec<Function>,
    pub static_methods: Vec<Function>,
}

impl StructDeclaration {
    pub fn new(
        range: Range<Position>,
        name: Identifier,
        properties: Vec<PropertyDeclaration>,
        instance_methods: Vec<Function>,
        static_methods: Vec<Function>,
    ) -> Self {
        Self {
            range,
            name,
            properties,
            instance_methods,
            static_methods,
        }
    }
}

impl GetRange for StructDeclaration {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}