use crate::analysis::type_::{FunctionType, Type};
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    range: Range<Position>,
    pub name: String,
    pub function_type: FunctionType,
}

impl FunctionInfo {
    pub fn new(
        range: Range<Position>,
        name: impl Into<String>,
        parameter_types: Vec<Type>,
        return_type: Type,
        receiver_type: Option<Type>,
    ) -> Self {
        FunctionInfo {
            range,
            name: name.into(),
            function_type: FunctionType {
                parameters: parameter_types.clone(),
                return_type: Box::new(return_type.clone()),
                receiver_type: receiver_type.map(Box::new),
            },
        }
    }

    pub fn type_(&self) -> Type {
        Type::Function(self.function_type.clone())
    }
}

impl GetRange for FunctionInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
