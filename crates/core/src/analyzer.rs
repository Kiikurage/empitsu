use crate::ast::assignment_expression::AssignmentExpression;
use crate::ast::binary_expression::BinaryExpression;
use crate::ast::block::Block;
use crate::ast::bool_literal::BoolLiteral;
use crate::ast::break_::Break;
use crate::ast::call_expression::CallExpression;
use crate::ast::for_statement::ForStatement;
use crate::ast::function::Function;
use crate::ast::get_range::GetRange;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression::IfExpression;
use crate::ast::if_statement::IfStatement;
use crate::ast::impl_statement::ImplStatement;
use crate::ast::interface_declaration::InterfaceDeclaration;
use crate::ast::member_expression::MemberExpression;
use crate::ast::node::Node;
use crate::ast::number_literal::NumberLiteral;
use crate::ast::program::Program;
use crate::ast::return_::Return;
use crate::ast::string_literal::StringLiteral;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::type_expression::TypeExpression;
use crate::ast::unary_expression::UnaryExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::error::Error;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use crate::range_map::RangeMap;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, Range};

#[derive(Clone, Debug, PartialEq)]
pub enum AnalyzedType {
    /// Type for variables not yet initialized.
    NotInitialized,

    /// Type cannot be determined due to an error.
    Any,

    Number,
    Bool,
}

/// TODO:
///     - 種類: 値、変数、関数、型、etc.
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolInfo {
    pub range: Range<Position>,
    pub name: String,
    pub defined_at: Option<Range<Position>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableInfo {
    pub name: String,
    pub defined_at: Range<Position>,
    pub type_: AnalyzedType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionInfo {
    pub type_: AnalyzedType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    /// Variables declared in this scope.
    declared_variables: HashMap<String, VariableInfo>,

    /// Variables initialized in this scope.
    initialized_variables: HashSet<String>,

    /// If this scope is able to be exit by "break".
    is_breakable: bool,

    /// If VM exited this scope by "return" or "break".
    exited: bool,
}

struct Context {
    symbols: RangeMap<Position, SymbolInfo>,
    variables: HashMap<Range<Position>, VariableInfo>,
    expressions: HashMap<Range<Position>, ExpressionInfo>,
    errors: Vec<Error>,

    scopes: Vec<Scope>,
}

impl Context {
    fn new() -> Context {
        Context {
            symbols: RangeMap::new(),
            variables: HashMap::new(),
            expressions: HashMap::new(),
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn analyze_node(&mut self, node: &Node) {
        match node {
            Node::Program(program) => self.analyze_program(program),
            Node::IfStatement(if_statement) => self.analyze_if_statement(if_statement),
            Node::ForStatement(for_statement) => self.analyze_for_statement(for_statement),
            Node::VariableDeclaration(variable_declaration) => self.analyze_variable_declaration(variable_declaration),
            Node::FunctionDeclaration(function) => self.analyze_function(function),
            Node::StructDeclaration(struct_) => self.analyze_struct_declaration(struct_),
            Node::InterfaceDeclaration(interface) => self.analyze_interface(interface),
            Node::ImplStatement(impl_statement) => self.analyze_impl_statement(impl_statement),
            Node::Return(return_) => self.analyze_return_(return_),
            Node::Break(break_) => self.analyze_break(break_),
            Node::FunctionExpression(function) => self.analyze_function(function),
            Node::IfExpression(if_expression) => self.analyze_if_expression(if_expression),
            Node::Block(block) => self.analyze_block(block),
            Node::AssignmentExpression(assignment_expression) => self.analyze_assignment_expression(assignment_expression),
            Node::BinaryExpression(binary_expression) => self.analyze_binary_expression(binary_expression),
            Node::UnaryExpression(unary_expression) => self.analyze_unary_expression(unary_expression),
            Node::CallExpression(call_expression) => self.analyze_call_expression(call_expression),
            Node::MemberExpression(member_expression) => self.analyze_member_expression(member_expression),
            Node::Identifier(identifier) => self.analyze_identifier(identifier),
            Node::NumberLiteral(number_literal) => self.analyze_number_literal(number_literal),
            Node::BoolLiteral(bool_literal) => self.analyze_bool_literal(bool_literal),
            Node::StringLiteral(string_literal) => self.analyze_string_literal(string_literal),
            Node::TypeExpression(type_expression) => self.analyze_type_expression(type_expression),
        }
    }

    fn analyze_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            if self.scopes.last().unwrap().exited {
                self.errors.push(Error::unreachable_code(node.range()));
                break;
            }
            self.analyze_node(node);
        }
    }

    fn analyze_program(&mut self, program: &Program) {
        self.enter_scope(false);
        self.analyze_nodes(&program.statements);
        self.exit_scope();
    }

    fn analyze_if_statement(&mut self, if_statement: &IfStatement) {
        self.analyze_node(&if_statement.condition);
        self.analyze_node(&if_statement.true_branch);
        if let Some(false_branch) = &if_statement.false_branch {
            self.analyze_node(false_branch.deref());
        }
    }

    fn analyze_for_statement(&mut self, for_statement: &ForStatement) {
        self.enter_scope(true);
        self.define_variable(&for_statement.variable);
        self.register_variable_type(&for_statement.variable, AnalyzedType::Number);
        self.initialize_variable(&for_statement.variable);
        self.register_symbol_info(
            for_statement.variable.range(),
            for_statement.variable.name.clone(),
            Some(for_statement.variable.range()),
        );

        // self.analyze_node(&for_statement.iterable); // TODO
        self.analyze_node(&for_statement.body);
        self.exit_scope();
    }

    fn analyze_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) {
        if let Some(initializer) = &variable_declaration.initializer {
            self.analyze_node(initializer);
        }

        self.define_variable(&variable_declaration.name);
        self.register_symbol_info(
            variable_declaration.name.range(),
            variable_declaration.name.name.clone(),
            Some(variable_declaration.name.range()),
        );
        if let Some(type_expression) = &variable_declaration.type_ {
            let type_ = self.evaluate_type_expression(type_expression);
            self.register_variable_type(&variable_declaration.name, type_);
        }
        if let Some(initializer) = &variable_declaration.initializer {
            let initializer_type = self.get_node_type(initializer);
            if let Some(type_expression) = &variable_declaration.type_ {
                let expected_type = self.evaluate_type_expression(type_expression);
                self.report_if_not_assignable(&initializer_type, &expected_type, &initializer.range());
            }
            self.register_variable_type(&variable_declaration.name, initializer_type);
            self.initialize_variable(&variable_declaration.name);
        }
    }

    fn analyze_function(&mut self, _function: &Function) {
        unimplemented!("Function analysis is not implemented yet");
    }

    fn analyze_struct_declaration(&mut self, _struct_: &StructDeclaration) {
        unimplemented!("Struct analysis is not implemented yet");
    }

    fn analyze_interface(&mut self, _interface: &InterfaceDeclaration) {
        unimplemented!("Interface analysis is not implemented yet");
    }

    fn analyze_impl_statement(&mut self, _impl_statement: &ImplStatement) {
        unimplemented!("Impl analysis is not implemented yet");
    }

    fn analyze_return_(&mut self, _return: &Return) {
        // Check if return value is valid
        // Check if return value type is correct
        // Check if it is inside a function
        unimplemented!("Return analysis is not implemented yet");
    }

    fn analyze_break(&mut self, break_: &Break) {
        if self.is_inside_of_loop() {
            self.mark_scopes_as_exited_by_break();
        } else {
            self.errors.push(Error::invalid_syntax(
                break_.range(),
                "Break statement is not inside a loop",
            ));
        }
    }

    fn analyze_if_expression(&mut self, if_expression: &IfExpression) {
        self.analyze_node(&if_expression.condition);
        self.analyze_node(&if_expression.true_branch);
        self.analyze_node(&if_expression.false_branch);

        let condition_type = self.get_node_type(&if_expression.condition);
        if !self.is_assignable(&condition_type, &AnalyzedType::Bool) {
            self.errors.push(Error::unexpected_type_in_if_condition(if_expression.condition.range(), &condition_type));
        }

        let true_branch_type_ = self.get_node_type(&if_expression.true_branch);
        let false_branch_type_ = self.get_node_type(&if_expression.false_branch);
        if self.report_if_not_assignable(
            &true_branch_type_,
            &false_branch_type_,
            &if_expression.false_branch.range(),
        ) {
            self.register_expression_type(if_expression.range(), true_branch_type_);
        } else {
            self.register_expression_type(if_expression.range(), AnalyzedType::Any);
        }
    }

    fn analyze_block(&mut self, block: &Block) {
        self.enter_scope(false);
        self.analyze_nodes(&block.nodes);
        self.exit_scope();
    }

    fn analyze_assignment_expression(&mut self, assignment_expression: &AssignmentExpression) {
        self.analyze_node(&assignment_expression.rhs);
        let rhs_type = self.get_node_type(&assignment_expression.rhs);

        match assignment_expression.lhs.deref() {
            Node::Identifier(identifier) => {
                match self.get_variable_definition(&identifier.name) {
                    Some(variable_info) => {
                        self.register_symbol_info(
                            identifier.range(),
                            identifier.name.clone(),
                            Some(variable_info.defined_at.clone()),
                        );
                    }
                    None => {
                        self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                        self.define_variable(identifier);
                    }
                }
                if self.is_variable_initialized(&identifier.name) {
                    let lhs_type = self.get_variable_type(&identifier.name);
                    self.report_if_not_assignable(&rhs_type, &lhs_type, &assignment_expression.rhs.range());
                } else {
                    self.register_variable_type(identifier, rhs_type.clone());
                    self.initialize_variable(identifier);
                }
            }
            _ => {
                self.errors.push(Error::invalid_syntax(
                    assignment_expression.range(),
                    "Left-hand side of an assignment must be a variable",
                ));
            }
        }

        self.register_expression_type(assignment_expression.range(), rhs_type);
    }

    fn analyze_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.analyze_node(&binary_expression.lhs);
        self.analyze_node(&binary_expression.rhs);
        let lhs_type = self.get_node_type(&binary_expression.lhs);
        let rhs_type = self.get_node_type(&binary_expression.rhs);
        let (expected_lhs_type, expected_rhs_type, result_type) = match binary_expression.operator {
            PunctuationKind::Plus => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::Minus => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::Asterisk => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::Slash => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::AndAnd => (AnalyzedType::Bool, AnalyzedType::Bool, AnalyzedType::Bool),
            PunctuationKind::VerticalLineVerticalLine => (AnalyzedType::Bool, AnalyzedType::Bool, AnalyzedType::Bool),

            // TODO
            PunctuationKind::EqualEqual => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),
            PunctuationKind::ExclamationEqual => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),

            PunctuationKind::LeftChevron => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),
            PunctuationKind::LeftChevronEqual => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),
            PunctuationKind::RightChevron => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),
            PunctuationKind::RightChevronEqual => (AnalyzedType::Number, AnalyzedType::Number, AnalyzedType::Bool),

            _ => panic!("Unexpected binary operator {:?}", binary_expression.operator),
        };

        self.report_if_not_assignable(&lhs_type, &expected_lhs_type, &binary_expression.lhs.range());
        self.report_if_not_assignable(&rhs_type, &expected_rhs_type, &binary_expression.rhs.range());

        self.register_expression_type(binary_expression.range(), result_type);
    }

    fn analyze_unary_expression(&mut self, unary_expression: &UnaryExpression) {
        self.analyze_node(&unary_expression.operand);
        let operand_type = self.get_node_type(&unary_expression.operand);

        let (expected_operand_type, result_type) = match unary_expression.operator {
            PunctuationKind::Minus => (AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::Exclamation => (AnalyzedType::Bool, AnalyzedType::Bool),
            _ => panic!("Unexpected unary operator {:?}", unary_expression.operator),
        };

        self.report_if_not_assignable(&operand_type, &expected_operand_type, &unary_expression.operand.range());

        self.register_expression_type(unary_expression.range(), result_type);
    }

    fn analyze_call_expression(&mut self, _call_expression: &CallExpression) {
        unimplemented!("Call analysis is not implemented yet");
    }

    fn analyze_member_expression(&mut self, _member_expression: &MemberExpression) {
        unimplemented!("Member analysis is not implemented yet");
    }

    fn analyze_identifier(&mut self, identifier: &Identifier) {
        match self.get_variable_definition(&identifier.name) {
            Some(variable_info) => {
                self.register_symbol_info(
                    identifier.range(),
                    identifier.name.clone(),
                    Some(variable_info.defined_at.clone()),
                );

                if !self.is_variable_initialized(&identifier.name) {
                    self.errors.push(Error::uninitialized_variable(identifier.range(), identifier.name.clone()));
                    self.initialize_variable(identifier);
                }
            }
            None => {
                self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                self.define_variable(identifier);
            }
        }

        self.register_expression_type(identifier.range(), self.get_variable_type(&identifier.name));
    }

    fn analyze_number_literal(&mut self, number_literal: &NumberLiteral) {
        self.register_expression_type(number_literal.range(), AnalyzedType::Number);
    }

    fn analyze_bool_literal(&mut self, bool_literal: &BoolLiteral) {
        self.register_expression_type(bool_literal.range(), AnalyzedType::Bool);
    }

    fn analyze_string_literal(&mut self, string_literal: &StringLiteral) {
        // TODO
        self.register_expression_type(string_literal.range(), AnalyzedType::Any);
    }

    fn analyze_type_expression(&mut self, _type_expression: &TypeExpression) {
        unimplemented!("Type analysis is not implemented yet");
    }

    // Scope

    fn enter_scope(&mut self, is_breakable: bool) {
        self.scopes.push(Scope {
            declared_variables: HashMap::new(),
            initialized_variables: HashSet::new(),
            is_breakable,
            exited: false,
        });
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    // Symbol

    fn register_symbol_info(
        &mut self,
        range: Range<Position>,
        name: String,
        defined_at: Option<Range<Position>>,
    ) {
        self.symbols.insert(range.clone(), SymbolInfo {
            range,
            name,
            defined_at,
        }).expect("Failed to insert symbol info");
    }

    // Variable

    fn define_variable(&mut self, identifier: &Identifier) {
        let frame = self.scopes.last_mut().unwrap();

        frame.declared_variables.insert(identifier.name.clone(), VariableInfo {
            name: identifier.name.clone(),
            defined_at: identifier.range(),
            type_: AnalyzedType::NotInitialized,
        });
    }

    fn get_variable_definition(&self, name: &String) -> Option<&VariableInfo> {
        for frame in self.scopes.iter().rev() {
            let option = frame.declared_variables.get(name);
            if option.is_some() {
                return option;
            }
        }
        None
    }

    fn initialize_variable(&mut self, identifier: &Identifier) {
        if !self.get_variable_definition(&identifier.name).is_some() {
            self.errors.push(Error::uninitialized_variable(identifier.range(), identifier.name.clone()));
        }
        self.scopes.last_mut().unwrap().initialized_variables.insert(identifier.name.clone());
    }

    fn is_variable_initialized(&self, name: &String) -> bool {
        for frame in self.scopes.iter().rev() {
            if frame.initialized_variables.contains(name) {
                return true;
            }
            if frame.declared_variables.contains_key(name) {
                return false;
            }
        }
        false
    }

    // Loop

    fn is_inside_of_loop(&self) -> bool {
        for frame in self.scopes.iter().rev() {
            if frame.is_breakable {
                return true;
            }
        }
        false
    }

    fn mark_scopes_as_exited_by_break(&mut self) {
        for frame in self.scopes.iter_mut().rev() {
            frame.exited = true;
            if frame.is_breakable { break; }
        }
    }

    // Type

    fn register_variable_type(&mut self, identifier: &Identifier, type_: AnalyzedType) {
        for frame in self.scopes.iter_mut().rev() {
            if let Some(symbol) = frame.declared_variables.get_mut(&identifier.name) {
                symbol.type_ = type_;
                self.variables.insert(symbol.defined_at.clone(), symbol.clone());
                return;
            }
        }
        panic!("Symbol {} is not declared", identifier.name);
    }

    // TODO: SymbolInfoに寄せる
    fn register_expression_type(&mut self, range: Range<Position>, type_: AnalyzedType) {
        self.expressions.insert(range, ExpressionInfo {
            type_,
        });
    }

    fn is_assignable(&self, from: &AnalyzedType, to: &AnalyzedType) -> bool {
        match (from, to) {
            (AnalyzedType::NotInitialized, _) => false,
            (_, AnalyzedType::NotInitialized) => false,
            (AnalyzedType::Any, _) => true,
            (_, AnalyzedType::Any) => true,
            (AnalyzedType::Number, AnalyzedType::Number) => true,
            (AnalyzedType::Bool, AnalyzedType::Bool) => true,
            _ => false,
        }
    }

    fn report_if_not_assignable(&mut self, from: &AnalyzedType, to: &AnalyzedType, range: &Range<Position>) -> bool {
        let is_assignable = self.is_assignable(from, to);
        if !is_assignable {
            self.errors.push(Error::unexpected_type(range.clone(), to, from));
        }

        is_assignable
    }

    fn get_variable_type(&self, name: &String) -> AnalyzedType {
        for frame in self.scopes.iter().rev() {
            if let Some(symbol) = frame.declared_variables.get(name) {
                return symbol.type_.clone();
            }
        }
        panic!("Symbol {:?} is not declared", name);
    }

    fn get_node_type(&self, node: &Node) -> AnalyzedType {
        let cached_info = self.expressions.get(&node.range());
        if let Some(cache) = cached_info {
            return cache.type_.clone();
        }

        panic!("Expression {:?} is not yet analyzed", node.range());
    }

    fn evaluate_type_expression(&self, type_expression: &TypeExpression) -> AnalyzedType {
        match type_expression.name.as_str() {
            "number" => AnalyzedType::Number,
            "bool" => AnalyzedType::Bool,
            _ => AnalyzedType::Any,
        }
    }
}

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
pub struct AnalyzeResult {
    pub symbols: RangeMap<Position, SymbolInfo>,
    pub variables: HashMap<Range<Position>, VariableInfo>,
    pub errors: Vec<Error>,
}

impl AnalyzeResult {
    pub fn get_symbol_type(&self, position: &Position) -> Option<&AnalyzedType> {
        let symbol = match self.symbols.find(position) {
            Some(symbol) => symbol,
            None => return None,
        };
        let defined_at = match symbol.defined_at {
            Some(ref defined_at) => defined_at,
            None => return None,
        };
        
        let variable = self.variables.get(defined_at)?;

        Some(&variable.type_)
    }
}

pub fn analyze(program: &Program) -> AnalyzeResult {
    let mut context = Context::new();

    context.analyze_program(program);

    AnalyzeResult {
        symbols: context.symbols,
        variables: context.variables,
        errors: context.errors,
    }
}

#[cfg(test)]
mod test {
    use crate::analyzer::{analyze, AnalyzeResult};
    use crate::error::Error;
    use crate::parser::parse;

    mod use_undefined_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn use_undefined_variable() {
            test(r#"let x = y"#).assert_errors(vec![
                Error::undefined_symbol(pos(0, 8)..pos(0, 9), "y")
            ])
        }
    }

    mod use_uninitialized_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn use_uninitialized_variable() {
            test(r#"let x:number; let y = x"#).assert_errors(vec![
                Error::uninitialized_variable(pos(0, 22)..pos(0, 23), "x")
            ])
        }
    }

    mod type_analyze_in_initialization {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 15)..pos(0, 20), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_assignment {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2; x = 3"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = 2; x = false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 22)..pos(0, 27), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_binary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"1 + 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type_in_rhs() {
            test(r#"1 + false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 4)..pos(0, 9), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_lhs() {
            test(r#"true + 1"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 0)..pos(0, 4), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_both() {
            test(r#"true + false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 0)..pos(0, 4), &AnalyzedType::Number, &AnalyzedType::Bool),
                Error::unexpected_type(pos(0, 7)..pos(0, 12), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_unary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"!true"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"!1"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 1)..pos(0, 2), &AnalyzedType::Bool, &AnalyzedType::Number)
            ])
        }
    }

    mod type_analyze_for_loop_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"
            let x = 1;
            for (i in iterable) {
                x = i
            }
            "#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"
            let x = true;
            for (i in iterable) {
                x = i
            }"#).assert_errors(vec![
                Error::unexpected_type(pos(3, 20)..pos(3, 21), &AnalyzedType::Bool, &AnalyzedType::Number)
            ])
        }
    }

    mod unreachable_code_due_to_break {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn unreachable_code_in_block() {
            test(r#"for (i in iterable) {
            break;
            let x = 1;
        }"#).assert_errors(vec![
                Error::unreachable_code(pos(2, 12)..pos(2, 21))
            ])
        }

        #[test]
        fn unreachable_code_in_different_block() {
            test(r#"for (i in iterable) {
            {
                break;
                let x = 1;
            }
            let y = 1;
        }"#).assert_errors(vec![
                Error::unreachable_code(pos(3, 16)..pos(3, 25)),
                Error::unreachable_code(pos(5, 12)..pos(5, 21))
            ])
        }

        #[test]
        fn unreachable_code_must_be_reported_only_once() {
            test(r#"for (i in iterable) {
            break;
            let x = 1;
            let y = 1;
        }"#).assert_errors(vec![
                Error::unreachable_code(pos(2, 12)..pos(2, 21))
            ])
        }
    }

    mod test {
        use crate::analyzer::test::test;

        #[test]
        fn f() {
            let result = test(r#"
let x = 1;
let y = false;
{
    let y = z;
    let x = y;
}
"#);

            for value in result.symbols.values() {
                println!("{:?}", value.name);
                println!("  - type: {:?}", result.get_symbol_type(&value.range.start));
                println!("  - range: {:?}", value.range);
                println!("  - defined_at: {:?}", value.defined_at);
            }
        }
    }

    trait AssertMethods {
        fn assert_errors(&self, expected: Vec<Error>);
    }

    impl AssertMethods for AnalyzeResult {
        fn assert_errors(&self, expected: Vec<Error>) {
            assert_eq!(self.errors, expected);
        }
    }

    fn test(src: &str) -> AnalyzeResult {
        let parse_result = parse(src);
        if !parse_result.errors.is_empty() {
            for error in parse_result.errors {
                eprintln!("{:?}", error);
            }
            panic!("Failed to parse");
        }

        analyze(&parse_result.program)
    }
}