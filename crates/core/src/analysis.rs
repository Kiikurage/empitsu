pub mod node_info;
pub mod identifier_info;
pub mod break_info;
pub mod variable_info;
pub mod type_;
pub mod scope_info;
pub mod expression_info;
pub mod type_expression_info;

use crate::analysis::break_info::BreakInfo;
use crate::analysis::node_info::NodeInfo;
use crate::analysis::type_::Type;
use crate::analysis::variable_info::VariableInfo;
use crate::error::Error;
use crate::position::Position;
use std::collections::HashMap;
use std::ops::Range;

///
/// AnalyzeResultへのクエリ
/// - RangeMap<Range, SymbolInfo>
///     - range: (シンボル全体の)範囲
///     - 名前
///     - 種類: 値、変数、関数、型、etc.
///     - declaration: 宣言されている場所
///     - definition: 定義されている場所
///     - implementation: 実装されている場所
///     - reference: プロジェクト内でのこのシンボルの参照
///     - type: 型
/// - (Position) -> ScopeInfo
///     - 一覧(ネストが深い方からルートへ順番に)
///     - break時に抜けるスコープ
///
#[derive(Debug)]
pub struct Analysis {
    tokens: HashMap<Range<Position>, NodeInfo>,
    pub errors: Vec<Error>,
}

impl Analysis {
    pub fn new(
        tokens: HashMap<Range<Position>, NodeInfo>,
        errors: Vec<Error>,
    ) -> Self {
        Self { tokens, errors }
    }

    pub fn get_variable_info(&self, range: &Range<Position>) -> Option<&VariableInfo> {
        match self.tokens.get(range) {
            Some(NodeInfo::Variable(variable)) => Some(variable),
            Some(NodeInfo::Identifier(identifier)) => {
                let defined_at = match identifier.defined_at {
                    Some(ref defined_at) => defined_at,
                    None => return None,
                };
                match self.tokens.get(defined_at) {
                    Some(NodeInfo::Variable(variable)) => Some(variable),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn get_break_info(&self, range: &Range<Position>) -> Option<&BreakInfo> {
        match self.tokens.get(range) {
            Some(NodeInfo::Break(break_info)) => Some(break_info),
            _ => None,
        }
    }

    pub fn get_expression_type(&self, range: &Range<Position>) -> Option<&Type> {
        match self.tokens.get(range) {
            Some(NodeInfo::Identifier(identifier_info)) => {
                let defined_at = match identifier_info.defined_at {
                    Some(ref defined_at) => defined_at,
                    None => return None,
                };
                match self.tokens.get(defined_at) {
                    Some(NodeInfo::Variable(variable_info)) => Some(&variable_info.type_),
                    _ => None,
                }
            }
            Some(NodeInfo::Expression(expression_info)) => Some(&expression_info.type_),
            _ => None,
        }
    }
}