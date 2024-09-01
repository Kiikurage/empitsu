use crate::ast::assignment_expression::AssignmentExpression;
use crate::ast::binary_expression::BinaryExpression;
use crate::ast::block::Block;
use crate::ast::bool_literal::BoolLiteral;
use crate::ast::break_expression::BreakExpression;
use crate::ast::call_expression::CallExpression;
use crate::ast::for_statement::ForStatement;
use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression::IfExpression;
use crate::ast::if_statement::IfStatement;
use crate::ast::impl_statement::ImplStatement;
use crate::ast::interface_declaration::InterfaceDeclaration;
use crate::ast::member_expression::MemberExpression;
use crate::ast::node::Node;
use crate::ast::number_literal::NumberLiteral;
use crate::ast::program::Program;
use crate::ast::return_expression::ReturnExpression;
use crate::ast::string_literal::StringLiteral;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::ast::unary_expression::UnaryExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::error::Error;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq)]
pub enum AnalyzedType {
    /// Type for variables not yet initialized.
    NotInitialized,

    /// Type cannot be determined due to an error.
    Any,

    Number,
    Bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolInfo {
    pub name: String,
    pub declared_at: Position,
    pub type_: AnalyzedType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionInfo {
    pub type_: AnalyzedType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    /// Variables declared in this scope.
    declared_variables: HashMap<String, SymbolInfo>,

    /// Variables initialized in this scope.
    initialized_variables: HashSet<String>,

    /// If this scope is able to be exit by "break".
    is_breakable: bool,

    /// If VM exited this scope by "return" or "break".
    exited: bool,
}

struct Context {
    variables: HashMap<Position, SymbolInfo>,
    expressions: HashMap<Position, ExpressionInfo>,
    errors: Vec<Error>,

    scopes: Vec<Scope>,
}

impl Context {
    fn new() -> Context {
        Context {
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
            Node::ReturnExpression(return_expression) => self.analyze_return_expression(return_expression),
            Node::BreakExpression(break_expression) => self.analyze_break_expression(break_expression),
            Node::FunctionExpression(function) => self.analyze_function(function),
            Node::IfExpression(if_expression) => self.analyze_if_expression(if_expression),
            Node::BlockExpression(block_expression) => self.analyze_block_expression(block_expression),
            Node::AssignmentExpression(assignment_expression) => self.analyze_assignment_expression(assignment_expression),
            Node::BinaryExpression(binary_expression) => self.analyze_binary_expression(binary_expression),
            Node::UnaryExpression(unary_expression) => self.analyze_unary_expression(unary_expression),
            Node::CallExpression(call_expression) => self.analyze_call_expression(call_expression),
            Node::MemberExpression(member_expression) => self.analyze_member_expression(member_expression),
            Node::Identifier(identifier) => self.analyze_identifier(identifier),
            Node::NumberLiteral(number_literal) => self.analyze_number_literal(number_literal),
            Node::BoolLiteral(bool_literal) => self.analyze_bool_literal(bool_literal),
            Node::StringLiteral(string_literal) => self.analyze_string_literal(string_literal),
        }
    }

    fn analyze_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            if self.scopes.last().unwrap().exited {
                self.errors.push(Error::unreachable_code(node.position().clone()));
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

        // self.analyze_node(&for_statement.iterable); // TODO
        self.analyze_node(&for_statement.body);
        self.exit_scope();
    }

    fn analyze_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) {
        self.define_variable(&variable_declaration.name);
        if let Some(type_expression) = &variable_declaration.type_ {
            let type_ = self.evaluate_type_expression(type_expression);
            self.register_variable_type(&variable_declaration.name, type_);
        }
        if let Some(initializer) = &variable_declaration.initializer {
            self.analyze_node(initializer);
            let initializer_type = self.get_node_type(initializer);
            if let Some(type_expression) = &variable_declaration.type_ {
                let expected_type = self.evaluate_type_expression(type_expression);
                self.report_if_not_assignable(&initializer_type, &expected_type, initializer.position());
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

    fn analyze_return_expression(&mut self, _return_statement: &ReturnExpression) {
        // Check if return value is valid
        // Check if return value type is correct
        // Check if it is inside a function
        unimplemented!("Return analysis is not implemented yet");
    }

    fn analyze_break_expression(&mut self, break_statement: &BreakExpression) {
        if self.is_inside_of_loop() {
            self.mark_scopes_as_exited_by_break();
        } else {
            self.errors.push(Error::invalid_syntax(
                break_statement.position().clone(),
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
            self.errors.push(Error::unexpected_type_in_if_condition(if_expression.condition.position().clone(), &condition_type));
        }

        let true_branch_type_ = self.get_node_type(&if_expression.true_branch);
        let false_branch_type_ = self.get_node_type(&if_expression.false_branch);
        if self.report_if_not_assignable(
            &true_branch_type_,
            &false_branch_type_,
            if_expression.false_branch.position(),
        ) {
            self.register_expression_type(if_expression.position().clone(), true_branch_type_);
        } else {
            self.register_expression_type(if_expression.position().clone(), AnalyzedType::Any);
        }
    }

    fn analyze_block_expression(&mut self, block_expression: &Block) {
        self.enter_scope(false);
        self.analyze_nodes(&block_expression.nodes);
        self.exit_scope();
    }

    fn analyze_assignment_expression(&mut self, assignment_expression: &AssignmentExpression) {
        self.analyze_node(&assignment_expression.rhs);
        let rhs_type = self.get_node_type(&assignment_expression.rhs);

        match assignment_expression.lhs.deref() {
            Node::Identifier(identifier) => {
                if !self.is_variable_defined(&identifier.name) {
                    self.errors.push(Error::undefined_symbol(identifier.position().clone(), identifier.name.clone()));
                    self.define_variable(identifier);
                }
                if self.is_variable_initialized(&identifier.name) {
                    let lhs_type = self.get_variable_type(&identifier.name);
                    self.report_if_not_assignable(&rhs_type, &lhs_type, assignment_expression.rhs.position());
                } else {
                    self.register_variable_type(identifier, rhs_type.clone());
                    self.initialize_variable(identifier);
                }
            }
            _ => {
                self.errors.push(Error::invalid_syntax(
                    assignment_expression.position().clone(),
                    "Left-hand side of an assignment must be a variable",
                ));
            }
        }

        self.register_expression_type(assignment_expression.position().clone(), rhs_type);
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

        self.report_if_not_assignable(&lhs_type, &expected_lhs_type, binary_expression.lhs.position());
        self.report_if_not_assignable(&rhs_type, &expected_rhs_type, binary_expression.rhs.position());

        self.register_expression_type(binary_expression.position().clone(), result_type);
    }

    fn analyze_unary_expression(&mut self, unary_expression: &UnaryExpression) {
        self.analyze_node(&unary_expression.operand);
        let operand_type = self.get_node_type(&unary_expression.operand);

        let (expected_operand_type, result_type) = match unary_expression.operator {
            PunctuationKind::Minus => (AnalyzedType::Number, AnalyzedType::Number),
            PunctuationKind::Exclamation => (AnalyzedType::Bool, AnalyzedType::Bool),
            _ => panic!("Unexpected unary operator {:?}", unary_expression.operator),
        };

        self.report_if_not_assignable(&operand_type, &expected_operand_type, unary_expression.operand.position());

        self.register_expression_type(unary_expression.position().clone(), result_type);
    }

    fn analyze_call_expression(&mut self, _call_expression: &CallExpression) {
        unimplemented!("Call analysis is not implemented yet");
    }

    fn analyze_member_expression(&mut self, _member_expression: &MemberExpression) {
        unimplemented!("Member analysis is not implemented yet");
    }

    fn analyze_identifier(&mut self, identifier: &Identifier) {
        if !self.is_variable_defined(&identifier.name) {
            self.errors.push(Error::undefined_symbol(identifier.position().clone(), identifier.name.clone()));
            self.define_variable(identifier);
        } else if !self.is_variable_initialized(&identifier.name) {
            self.errors.push(Error::uninitialized_variable(identifier.position().clone(), identifier.name.clone()));
            self.initialize_variable(identifier);
        }

        self.register_expression_type(identifier.position().clone(), self.get_variable_type(&identifier.name));
    }

    fn analyze_number_literal(&mut self, number_literal: &NumberLiteral) {
        self.register_expression_type(number_literal.position().clone(), AnalyzedType::Number);
    }

    fn analyze_bool_literal(&mut self, bool_literal: &BoolLiteral) {
        self.register_expression_type(bool_literal.position().clone(), AnalyzedType::Bool);
    }

    fn analyze_string_literal(&mut self, string_literal: &StringLiteral) {
        // TODO
        self.register_expression_type(string_literal.position().clone(), AnalyzedType::Any);
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

    // Variable

    fn define_variable(&mut self, identifier: &Identifier) {
        let frame = self.scopes.last_mut().unwrap();

        frame.declared_variables.insert(identifier.name.clone(), SymbolInfo {
            name: identifier.name.clone(),
            declared_at: identifier.position().clone(),
            type_: AnalyzedType::NotInitialized,
        });
    }

    fn is_variable_defined(&self, name: &String) -> bool {
        for frame in self.scopes.iter().rev() {
            if frame.declared_variables.contains_key(name) {
                return true;
            }
        }
        false
    }

    fn initialize_variable(&mut self, identifier: &Identifier) {
        if !self.is_variable_defined(&identifier.name) {
            self.errors.push(Error::uninitialized_variable(identifier.position().clone(), identifier.name.clone()));
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
                self.variables.insert(symbol.declared_at.clone(), symbol.clone());
                return;
            }
        }
        panic!("Symbol {} is not declared", identifier.name);
    }

    fn register_expression_type(&mut self, position: Position, type_: AnalyzedType) {
        self.expressions.insert(position, ExpressionInfo {
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

    fn report_if_not_assignable(&mut self, from: &AnalyzedType, to: &AnalyzedType, position: &Position) -> bool {
        let is_assignable = self.is_assignable(from, to);
        if !is_assignable {
            self.errors.push(Error::unexpected_type(position.clone(), to, from));
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
        let cached_info = self.expressions.get(node.position());
        if let Some(cache) = cached_info {
            return cache.type_.clone();
        }

        panic!("Expression {:?} is not yet analyzed", node.position());
    }

    fn evaluate_type_expression(&self, _type_expression: &TypeExpression) -> AnalyzedType {
        match _type_expression {
            TypeExpression::Identifier(name) => {
                match name.as_str() {
                    "number" => AnalyzedType::Number,
                    "bool" => AnalyzedType::Bool,
                    _ => AnalyzedType::Any,
                }
            }
            TypeExpression::Optional(_) => AnalyzedType::Any,
            TypeExpression::Union(_) => AnalyzedType::Any,
        }
    }
}

pub struct AnalyzeResult {
    pub variables: HashMap<Position, SymbolInfo>,
    pub errors: Vec<Error>,
}

pub fn analyze(program: &Program) -> AnalyzeResult {
    let mut context = Context::new();

    context.analyze_program(program);

    AnalyzeResult {
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

        #[test]
        fn use_undefined_variable() {
            test(r#"let x = y"#).assert_errors(vec![
                Error::undefined_symbol((0, 8), "y")
            ])
        }
    }

    mod use_uninitialized_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;

        #[test]
        fn use_uninitialized_variable() {
            test(r#"let x:number; let y = x"#).assert_errors(vec![
                Error::uninitialized_variable((0, 22), "x")
            ])
        }
    }

    mod type_analyze_in_initialization {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = false"#).assert_errors(vec![
                Error::unexpected_type((0, 15), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_assignment {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2; x = 3"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = 2; x = false"#).assert_errors(vec![
                Error::unexpected_type((0, 22), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_binary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;

        #[test]
        fn correct_type() {
            test(r#"1 + 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type_in_rhs() {
            test(r#"1 + false"#).assert_errors(vec![
                Error::unexpected_type((0, 4), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_lhs() {
            test(r#"true + 1"#).assert_errors(vec![
                Error::unexpected_type((0, 0), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_both() {
            test(r#"true + false"#).assert_errors(vec![
                Error::unexpected_type((0, 0), &AnalyzedType::Number, &AnalyzedType::Bool),
                Error::unexpected_type((0, 7), &AnalyzedType::Number, &AnalyzedType::Bool)
            ])
        }
    }

    mod type_analyze_in_unary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;

        #[test]
        fn correct_type() {
            test(r#"!true"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"!1"#).assert_errors(vec![
                Error::unexpected_type((0, 1), &AnalyzedType::Bool, &AnalyzedType::Number)
            ])
        }
    }

    mod type_analyze_for_loop_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::AnalyzedType;
        use crate::error::Error;

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
                Error::unexpected_type((3, 20), &AnalyzedType::Bool, &AnalyzedType::Number)
            ])
        }
    }

    mod unreachable_code_due_to_break {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;

        #[test]
        fn unreachable_code_in_block() {
            test(r#"for (i in iterable) {
            break;
            let x = 1;
        }"#).assert_errors(vec![
                Error::unreachable_code((2, 12))
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
                Error::unreachable_code((3, 16)),
                Error::unreachable_code((5, 12))
            ])
        }

        #[test]
        fn unreachable_code_must_be_reported_only_once() {
            test(r#"for (i in iterable) {
            break;
            let x = 1;
            let y = 1;
        }"#).assert_errors(vec![
                Error::unreachable_code((2, 12))
            ])
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