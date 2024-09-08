pub mod identifier_info;
pub mod break_info;
pub mod variable_info;
pub mod type_;
pub mod scope_info;
pub mod type_expression_info;
pub mod function_info;
pub mod return_info;
pub mod analyzer_context;
pub mod env;
pub mod expression_info;

use crate::analysis::break_info::BreakInfo;
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::function_info::FunctionInfo;
use crate::analysis::return_info::ReturnInfo;
use crate::analysis::type_::Type;
use crate::analysis::type_expression_info::TypeExpressionInfo;
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
    variables: HashMap<Range<Position>, VariableInfo>,
    breaks: HashMap<Range<Position>, BreakInfo>,
    expressions: HashMap<Range<Position>, ExpressionInfo>,
    type_expressions: HashMap<Range<Position>, TypeExpressionInfo>,
    functions: HashMap<Range<Position>, FunctionInfo>,
    returns: HashMap<Range<Position>, ReturnInfo>,
    pub errors: Vec<Error>,
}

impl Analysis {
    pub fn new(
        variables: HashMap<Range<Position>, VariableInfo>,
        breaks: HashMap<Range<Position>, BreakInfo>,
        expressions: HashMap<Range<Position>, ExpressionInfo>,
        type_expressions: HashMap<Range<Position>, TypeExpressionInfo>,
        functions: HashMap<Range<Position>, FunctionInfo>,
        returns: HashMap<Range<Position>, ReturnInfo>,
        errors: Vec<Error>,
    ) -> Self {
        Self {
            variables,
            breaks,
            expressions,
            type_expressions,
            functions,
            returns,
            errors,
        }
    }

    pub fn get_expression_info(&self, range: &Range<Position>) -> Option<&ExpressionInfo> {
        self.expressions.get(range)
    }

    pub fn get_function_info(&self, range: &Range<Position>) -> Option<&FunctionInfo> {
        self.functions.get(range)
    }

    pub fn get_variable_info(&self, range: &Range<Position>) -> Option<&VariableInfo> {
        if self.variables.contains_key(range) {
            return self.variables.get(range);
        }

        if let Some(ExpressionInfo::Variable(symbol_ref)) = self.expressions.get(range) {
            if let Some(ref defined_at) = symbol_ref.defined_at {
                return self.variables.get(defined_at);
            }
        }

        None
    }

    pub fn get_break_info(&self, range: &Range<Position>) -> Option<&BreakInfo> {
        self.breaks.get(range)
    }

    pub fn get_expression_type(&self, range: &Range<Position>) -> Option<&Type> {
        match self.expressions.get(range) {
            Some(ExpressionInfo::TempValue(temp_value)) => Some(&temp_value.type_),
            Some(ExpressionInfo::Variable(symbol_ref)) => {
                if let Some(ref defined_at) = symbol_ref.defined_at {
                    if let Some(variable) = self.variables.get(defined_at) {
                        return Some(&variable.type_);
                    }
                }
                None
            }
            _ => None,
        }
    }
}