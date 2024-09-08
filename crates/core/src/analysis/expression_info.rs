use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionInfo {
    Variable(SymbolRef),
    Function(SymbolRef),
    TempValue(TempValueInfo),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TempValueInfo {
    range: Range<Position>,
    pub type_: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolRef {
    range: Range<Position>,
    pub defined_at: Option<Range<Position>>, // Optional because invalid code may use undefined symbols 
}

impl ExpressionInfo {
    pub fn variable(range: Range<Position>, defined_at: Option<Range<Position>>) -> Self {
        Self::Variable(SymbolRef { range, defined_at })
    }

    pub fn function(range: Range<Position>, defined_at: Option<Range<Position>>) -> Self {
        Self::Function(SymbolRef { range, defined_at })
    }

    pub fn temp_value(range: Range<Position>, type_: Type) -> Self {
        Self::TempValue(TempValueInfo { range, type_ })
    }
}

impl GetRange for ExpressionInfo {
    fn range(&self) -> Range<Position> {
        match self {
            Self::Variable(symbol_ref) => symbol_ref.range.clone(),
            Self::Function(symbol_ref) => symbol_ref.range.clone(),
            Self::TempValue(temp_value) => temp_value.range.clone(),
        }
    }
}
