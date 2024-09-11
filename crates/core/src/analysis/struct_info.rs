use crate::analysis::type_::{StructType, Type};
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct StructInfo {
    range: Range<Position>,
    pub name: String,
    pub struct_type: StructType,
}

impl StructInfo {
    pub fn new(
        range: Range<Position>,
        name: impl Into<String>,
        properties: Vec<(String, Type)>,
        instance_methods: Vec<(String, Type)>,
    ) -> Self {
        StructInfo {
            range,
            name: name.into(),
            struct_type: StructType {
                properties,
                instance_methods,
            },
        }
    }

    pub fn constructor_type(&self) -> Type {
        Type::function(
            self.struct_type.properties.iter().map(|(_, type_)| type_.clone()).collect(),
            self.instance_type(),
            None,
        )
    }

    pub fn instance_type(&self) -> Type {
        Type::Struct(self.struct_type.clone())
    }
}

impl GetRange for StructInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
