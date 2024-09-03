use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::collections::HashMap;
use std::ops::Range;
use crate::analysis::type_::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableInitializationInfo {
    range: Range<Position>,
    pub type_: Type,
}

impl VariableInitializationInfo {
    pub fn new(range: Range<Position>, type_: Type) -> Self {
        Self { range, type_ }
    }
}

impl GetRange for VariableInitializationInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeInfo {
    /// Range of the scope.
    range: Range<Position>,

    /// Variables declared in this scope, and the declaration's position.
    pub declared_variables: HashMap<String, Range<Position>>,

    /// Variables initialized in this scope, and the position where initialization is happened.
    pub initialized_variables: HashMap<String, VariableInitializationInfo>,

    /// If this scope is able to be exit by "break".
    pub is_breakable: bool,

    /// If this scope is exited by "break".
    pub exited_by_break: bool,
}

impl ScopeInfo {
    pub fn new(range: Range<Position>, is_breakable: bool) -> Self {
        Self {
            range,
            declared_variables: HashMap::new(),
            initialized_variables: HashMap::new(),
            is_breakable,
            exited_by_break: false,
        }
    }
}

impl GetRange for ScopeInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}