use crate::analysis::break_info::BreakInfo;
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::function_info::FunctionInfo;
use crate::analysis::return_info::ReturnInfo;
use crate::analysis::type_::Type;
use crate::analysis::type_expression_info::TypeExpressionInfo;
use crate::analysis::variable_info::VariableInfo;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExitReason {
    Normal,
    Break,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub defined_at: Range<Position>,
    pub kind: SymbolKind,
}

pub struct Env {
    pub is_loop: bool,
    pub is_function: bool,

    symbols: Vec<Symbol>,
    variables_initialized_at: HashMap<Range<Position>, Range<Position>>,
    variable_types: HashMap<Range<Position>, Type>,
    pub exit_reason: ExitReason,
    pub pending_returns: Vec<Range<Position>>,

    pub variables: HashMap<Range<Position>, VariableInfo>,
    pub functions: HashMap<Range<Position>, FunctionInfo>,
    pub expressions: HashMap<Range<Position>, ExpressionInfo>,
    pub breaks: HashMap<Range<Position>, BreakInfo>,
    pub type_expressions: HashMap<Range<Position>, TypeExpressionInfo>,
    pub returns: HashMap<Range<Position>, ReturnInfo>,
}

impl Env {
    pub fn new(
        is_loop: bool,
        is_function: bool,
    ) -> Self {
        Env {
            is_loop,
            is_function,
            exit_reason: ExitReason::Normal,

            symbols: Vec::new(),
            variables_initialized_at: HashMap::new(),
            variable_types: HashMap::new(),
            pending_returns: Vec::new(),

            variables: HashMap::new(),
            breaks: HashMap::new(),
            expressions: HashMap::new(),
            type_expressions: HashMap::new(),
            functions: HashMap::new(),
            returns: HashMap::new(),
        }
    }

    pub fn merge(&mut self, mut child_env: Env) {
        child_env.encapsulate();

        self.variables_initialized_at.extend(child_env.variables_initialized_at);
        self.variable_types.extend(child_env.variable_types);

        self.exit_reason = child_env.exit_reason;
        self.pending_returns.extend(child_env.pending_returns);

        self.variables.extend(child_env.variables);
        self.breaks.extend(child_env.breaks);
        self.expressions.extend(child_env.expressions);
        self.type_expressions.extend(child_env.type_expressions);
        self.functions.extend(child_env.functions);
        self.returns.extend(child_env.returns);
    }

    /// Encapsulate local symbols so that they are not accessible from the outer scope.
    pub fn encapsulate(&mut self) {
        // Local information
        for symbol in self.symbols.iter() {
            match symbol.kind {
                SymbolKind::Function => {}
                SymbolKind::Variable => {
                    let type_ = self.variable_types.get(&symbol.defined_at).unwrap_or(&Type::Unknown).clone();
                    self.variables.insert(symbol.defined_at.clone(), VariableInfo::new(
                        symbol.defined_at.clone(),
                        symbol.name.clone(),
                        type_,
                    ));
                    self.variable_types.remove(&symbol.defined_at);
                    self.variables_initialized_at.remove(&symbol.defined_at);
                }
            }
        }
        self.symbols.clear();
    }

    pub fn define_symbol(&mut self, defined_at: Range<Position>, name: impl Into<String>, kind: SymbolKind) {
        self.symbols.push(Symbol {
            name: name.into(),
            defined_at,
            kind,
        });
    }

    pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
        self.symbols.iter().rev().find(|&symbol| symbol.name == name)
    }

    pub fn is_symbol_defined(&self, defined_at: &Range<Position>) -> bool {
        for symbol in self.symbols.iter().rev() {
            if &symbol.defined_at == defined_at {
                return true;
            }
        }
        false
    }

    pub fn set_variable_initialized(&mut self, defined_at: Range<Position>, initialized_at: Range<Position>) {
        self.variables_initialized_at.insert(defined_at, initialized_at);
    }

    pub fn is_variable_initialized(&self, defined_at: &Range<Position>) -> Option<&Range<Position>> {
        self.variables_initialized_at.get(defined_at)
    }

    pub fn initialized_variables(&self) -> impl Iterator<Item=(&Range<Position>, &Range<Position>)> {
        self.variables_initialized_at.iter()
    }

    pub fn set_variable_type(&mut self, defined_at: Range<Position>, type_: Type) {
        self.variable_types.insert(defined_at, type_);
    }

    pub fn get_variable_type(&self, defined_at: &Range<Position>) -> Option<&Type> {
        self.variable_types.get(defined_at)
    }

    pub fn add_break(&mut self, range: Range<Position>) {
        self.breaks.insert(range.clone(), BreakInfo::new(range));
    }

    pub fn get_break(&self, range: &Range<Position>) -> Option<&BreakInfo> {
        self.breaks.get(range)
    }

    pub fn add_return(&mut self, range: Range<Position>, return_value_type: Type) {
        self.pending_returns.push(range.clone());
        self.returns.insert(range.clone(), ReturnInfo::new(range, return_value_type));
    }

    pub fn get_return(&self, range: &Range<Position>) -> Option<&ReturnInfo> {
        self.returns.get(range)
    }

    pub fn add_expression(&mut self, info: ExpressionInfo) {
        self.expressions.insert(info.range(), info);
    }

    pub fn get_expression(&self, range: &Range<Position>) -> Option<&ExpressionInfo> {
        self.expressions.get(range)
    }

    pub fn add_type_expression(&mut self, range: Range<Position>, type_: Type) {
        self.type_expressions.insert(range.clone(), TypeExpressionInfo::new(range.clone(), type_));
    }

    pub fn get_type_expression(&self, range: &Range<Position>) -> Option<&TypeExpressionInfo> {
        self.type_expressions.get(range)
    }

    pub fn add_function(&mut self, range: Range<Position>, name: impl Into<String>, type_: Type) {
        let name = name.into();
        self.define_symbol(range.clone(), name.clone(), SymbolKind::Function);
        self.functions.insert(range.clone(), FunctionInfo::new(range.clone(), name, type_));
        self.add_expression(ExpressionInfo::function(range.clone(), Some(range.clone())));
    }

    pub fn get_function(&self, range: &Range<Position>) -> Option<&FunctionInfo> {
        self.functions.get(range)
    }
}
