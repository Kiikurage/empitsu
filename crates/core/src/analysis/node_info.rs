use crate::analysis::break_info::BreakInfo;
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::identifier_info::IdentifierInfo;
use crate::analysis::type_::Type;
use crate::analysis::type_expression_info::TypeExpressionInfo;
use crate::analysis::variable_info::VariableInfo;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum NodeInfo {
    Variable(VariableInfo),
    Identifier(IdentifierInfo),
    Break(BreakInfo),
    Expression(ExpressionInfo),
    TypeExpression(TypeExpressionInfo),
}

impl NodeInfo {
    pub fn variable(range: Range<Position>, name: impl Into<String>, type_: Type) -> Self {
        NodeInfo::Variable(VariableInfo::new(range, name, type_))
    }

    pub fn identifier(range: Range<Position>, name: impl Into<String>, defined_at: Option<Range<Position>>) -> Self {
        NodeInfo::Identifier(IdentifierInfo::new(range, name, defined_at))
    }

    pub fn break_(range: Range<Position>, scope_range: Range<Position>) -> Self {
        NodeInfo::Break(BreakInfo::new(range, scope_range))
    }

    pub fn expression(range: Range<Position>, type_: Type) -> Self {
        NodeInfo::Expression(ExpressionInfo::new(range, type_))
    }

    pub fn type_expression(range: Range<Position>, type_: Type) -> Self {
        NodeInfo::TypeExpression(TypeExpressionInfo::new(range, type_))
    }
}

impl GetRange for NodeInfo {
    fn range(&self) -> Range<Position> {
        match self {
            NodeInfo::Variable(variable) => variable.range(),
            NodeInfo::Identifier(identifier) => identifier.range(),
            NodeInfo::Break(break_) => break_.range(),
            NodeInfo::Expression(expression) => expression.range(),
            NodeInfo::TypeExpression(type_) => type_.range(),
        }
    }
}
