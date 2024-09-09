use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;
use crate::ast::type_expression::TypeExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    range: Range<Position>,
    pub parameters: Vec<TypeExpression>,
    pub return_: Box<TypeExpression>
}

impl FunctionType {
    pub fn new(range: Range<Position>, parameters: Vec<TypeExpression>, return_: TypeExpression) -> Self {
        Self { range, parameters, return_: Box::new(return_) }
    }
}

impl GetRange for FunctionType {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
