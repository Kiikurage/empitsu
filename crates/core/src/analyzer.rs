use crate::analysis::leaf_node_info::NodeInfo;
use crate::analysis::scope_info::{ScopeInfo, VariableInitializationInfo};
use crate::analysis::type_::Type;
use crate::analysis::variable_info::VariableInfo;
use crate::analysis::Analysis;
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
use crate::parser::parse;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::collections::HashMap;
use std::ops::{Deref, Range};

struct Context {
    tokens: HashMap<Range<Position>, NodeInfo>,
    errors: Vec<Error>,

    /// Temporary values for analysis
    scopes: HashMap<Position, ScopeInfo>,
    scope_stack: Vec<Position>,
}

impl Context {
    fn new() -> Context {
        Context {
            tokens: HashMap::new(),
            errors: Vec::new(),

            scopes: HashMap::new(),
            scope_stack: Vec::new(),
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
            if self.get_current_scope().exited_by_break {
                self.errors.push(Error::unreachable_code(node.range()));
                break;
            }
            self.analyze_node(node);
        }
    }

    fn analyze_program(&mut self, program: &Program) {
        self.enter_scope(program.range(), false);
        self.analyze_nodes(&program.statements);
        self.exit_scope();
    }

    fn analyze_if_statement(&mut self, if_statement: &IfStatement) {
        self.analyze_node(&if_statement.condition);

        // dummy scope
        let true_branch_scope_range = if_statement.start()..if_statement.true_branch.end();
        self.enter_scope(true_branch_scope_range.clone(), false);

        self.analyze_node(&if_statement.true_branch);

        self.exit_scope();
        let dummy_true_branch = self.scopes.remove(&true_branch_scope_range.start).unwrap();
        let true_branch_scope = self.scopes.get(&if_statement.true_branch.start()).unwrap_or(&dummy_true_branch)
            .clone();

        let false_branch_scope_range = if_statement.true_branch.end()..if_statement.end();

        self.enter_scope(false_branch_scope_range.clone(), false);
        let false_branch_scope = match &if_statement.false_branch {
            Some(false_branch) => {
                self.analyze_node(false_branch.deref());
                match self.scopes.get(&false_branch.start()) {
                    Some(scope) => Some(scope.clone()),
                    None => None
                }
            }
            None => None
        };
        self.exit_scope();

        let dummy_false_branch = self.scopes.remove(&false_branch_scope_range.start).unwrap();
        let false_branch_scope = false_branch_scope.unwrap_or(dummy_false_branch);

        for (name, true_initialization_info) in true_branch_scope.initialized_variables.iter() {
            match false_branch_scope.initialized_variables.get(name) {
                Some(false_initialization_info) => {
                    if !self.is_assignable(&false_initialization_info.type_, &true_initialization_info.type_) {
                        self.errors.push(Error::conditionally_initialized_as_different_type(false_initialization_info.range(), name.clone()));
                        self.set_variable_initialized(name, true_initialization_info.range(), Type::Any);
                    } else {
                        self.set_variable_initialized(name, true_initialization_info.range(), true_initialization_info.type_.clone());
                    }
                }
                None => {
                    self.errors.push(Error::conditionally_initialized_variable(true_initialization_info.range(), name.clone()));
                }
            }
        }
        for (name, initialization_info) in false_branch_scope.initialized_variables.iter() {
            if !true_branch_scope.initialized_variables.contains_key(name) {
                self.errors.push(Error::conditionally_initialized_variable(initialization_info.range(), name.clone()));
            }
        }

        let condition_type = self.get_expression_type(&if_statement.condition);
        if !self.is_assignable(&condition_type, &Type::Bool) {
            self.errors.push(Error::unexpected_type_in_if_condition(if_statement.condition.range(), &condition_type));
        }
    }

    fn analyze_for_statement(&mut self, for_statement: &ForStatement) {
        self.enter_scope(for_statement.range(), true);
        self.define_variable(&for_statement.variable);
        self.set_variable_type(&for_statement.variable.name, Type::Number);
        self.set_variable_initialized(&for_statement.variable.name, for_statement.variable.range(), Type::Number);

        // self.analyze_node(&for_statement.iterable); // TODO
        self.analyze_node(&for_statement.body);
        self.exit_scope();
    }

    fn analyze_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) {
        if let Some(initializer) = &variable_declaration.initializer {
            self.analyze_node(initializer);
        }

        self.define_variable(&variable_declaration.name);
        if let Some(type_expression) = &variable_declaration.type_ {
            let type_ = self.evaluate_type_expression(type_expression);
            self.set_variable_type(&variable_declaration.name.name, type_);
        }
        if let Some(initializer) = &variable_declaration.initializer {
            let initializer_type = self.get_expression_type(initializer);
            if let Some(type_expression) = &variable_declaration.type_ {
                let expected_type = self.evaluate_type_expression(type_expression);
                self.report_if_not_assignable(&initializer_type, &expected_type, &initializer.range());
            }
            self.set_variable_type(&variable_declaration.name.name, initializer_type.clone());
            self.set_variable_initialized(&variable_declaration.name.name, variable_declaration.name.range(), initializer_type);
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
        match self.get_current_breakable() {
            Some(scope) => {
                let scope_range = scope.range().clone();
                self.tokens.insert(
                    break_.range(),
                    NodeInfo::break_(break_.range(), scope_range),
                );

                self.get_current_scope_mut().exited_by_break = true;
            }
            None => {
                self.errors.push(Error::invalid_syntax(
                    break_.range(),
                    "Break statement is not inside a loop",
                ));
            }
        }
    }

    fn analyze_if_expression(&mut self, if_expression: &IfExpression) {
        self.analyze_node(&if_expression.condition);
        self.analyze_node(&if_expression.true_branch);
        self.analyze_node(&if_expression.false_branch);

        let condition_type = self.get_expression_type(&if_expression.condition);
        if !self.is_assignable(&condition_type, &Type::Bool) {
            self.errors.push(Error::unexpected_type_in_if_condition(if_expression.condition.range(), &condition_type));
        }

        let true_branch_type_ = self.get_expression_type(&if_expression.true_branch);
        let false_branch_type_ = self.get_expression_type(&if_expression.false_branch);
        if self.report_if_not_assignable(
            &true_branch_type_,
            &false_branch_type_,
            &if_expression.false_branch.range(),
        ) {
            self.set_expression_type(if_expression.range(), true_branch_type_);
        } else {
            self.set_expression_type(if_expression.range(), Type::Any);
        }
    }

    fn analyze_block(&mut self, block: &Block) {
        self.enter_scope(block.range(), false);
        self.analyze_nodes(&block.nodes);
        let exited_by_break = self.get_current_scope().exited_by_break;
        self.exit_scope();

        if exited_by_break {
            self.set_expression_type(block.range(), Type::Void);
            self.get_current_scope_mut().exited_by_break = true;
        } else {
            let last_type = block.nodes.last()
                .map(|last_node| self.get_expression_type(last_node))
                .unwrap_or(Type::Void);

            self.set_expression_type(block.range(), last_type)
        }
    }

    fn analyze_assignment_expression(&mut self, assignment_expression: &AssignmentExpression) {
        self.analyze_node(&assignment_expression.rhs);
        let rhs_type = self.get_expression_type(&assignment_expression.rhs);

        match assignment_expression.lhs.deref() {
            Node::Identifier(identifier) => {
                match self.get_variable_definition(&identifier.name) {
                    Some(variable) => {
                        self.tokens.insert(identifier.range(), NodeInfo::identifier(
                            identifier.range(),
                            identifier.name.clone(),
                            Some(variable.range()),
                        ));
                    }
                    None => {
                        self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                        self.tokens.insert(identifier.range(), NodeInfo::identifier(
                            identifier.range(),
                            identifier.name.clone(),
                            None,
                        ));
                    }
                }

                if self.is_variable_initialized(&identifier.name) {
                    let lhs_type = self.get_variable_type(&identifier.name);
                    self.report_if_not_assignable(&rhs_type, &lhs_type, &assignment_expression.rhs.range());
                } else {
                    self.set_variable_type(&identifier.name, rhs_type.clone());
                    self.set_variable_initialized(&identifier.name, identifier.range(), rhs_type.clone());
                }
            }
            _ => {
                self.errors.push(Error::invalid_syntax(
                    assignment_expression.range(),
                    "Left-hand side of an assignment must be a variable",
                ));
            }
        }

        self.set_expression_type(assignment_expression.range(), rhs_type);
    }

    fn analyze_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.analyze_node(&binary_expression.lhs);
        self.analyze_node(&binary_expression.rhs);
        let lhs_type = self.get_expression_type(&binary_expression.lhs);
        let rhs_type = self.get_expression_type(&binary_expression.rhs);
        let (expected_lhs_type, expected_rhs_type, result_type) = match binary_expression.operator {
            PunctuationKind::Plus => (Type::Number, Type::Number, Type::Number),
            PunctuationKind::Minus => (Type::Number, Type::Number, Type::Number),
            PunctuationKind::Asterisk => (Type::Number, Type::Number, Type::Number),
            PunctuationKind::Slash => (Type::Number, Type::Number, Type::Number),
            PunctuationKind::AndAnd => (Type::Bool, Type::Bool, Type::Bool),
            PunctuationKind::VerticalLineVerticalLine => (Type::Bool, Type::Bool, Type::Bool),

            // TODO
            PunctuationKind::EqualEqual => (Type::Number, Type::Number, Type::Bool),
            PunctuationKind::ExclamationEqual => (Type::Number, Type::Number, Type::Bool),

            PunctuationKind::LeftChevron => (Type::Number, Type::Number, Type::Bool),
            PunctuationKind::LeftChevronEqual => (Type::Number, Type::Number, Type::Bool),
            PunctuationKind::RightChevron => (Type::Number, Type::Number, Type::Bool),
            PunctuationKind::RightChevronEqual => (Type::Number, Type::Number, Type::Bool),

            _ => panic!("Unexpected binary operator {:?}", binary_expression.operator),
        };

        self.report_if_not_assignable(&lhs_type, &expected_lhs_type, &binary_expression.lhs.range());
        self.report_if_not_assignable(&rhs_type, &expected_rhs_type, &binary_expression.rhs.range());

        self.set_expression_type(binary_expression.range(), result_type);
    }

    fn analyze_unary_expression(&mut self, unary_expression: &UnaryExpression) {
        self.analyze_node(&unary_expression.operand);
        let operand_type = self.get_expression_type(&unary_expression.operand);

        let (expected_operand_type, result_type) = match unary_expression.operator {
            PunctuationKind::Minus => (Type::Number, Type::Number),
            PunctuationKind::Exclamation => (Type::Bool, Type::Bool),
            _ => panic!("Unexpected unary operator {:?}", unary_expression.operator),
        };

        self.report_if_not_assignable(&operand_type, &expected_operand_type, &unary_expression.operand.range());

        self.set_expression_type(unary_expression.range(), result_type);
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
                self.tokens.insert(identifier.range(), NodeInfo::identifier(
                    identifier.range(),
                    identifier.name.clone(),
                    Some(variable_info.range().clone()),
                ));

                if !self.is_variable_initialized(&identifier.name) {
                    self.errors.push(Error::uninitialized_variable(identifier.range(), identifier.name.clone()));
                    self.set_variable_initialized(&identifier.name, identifier.range(), Type::Any);
                }
            }
            None => {
                self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                self.define_variable(identifier);
            }
        }
    }

    fn analyze_number_literal(&mut self, number_literal: &NumberLiteral) {
        self.set_expression_type(number_literal.range(), Type::Number);
    }

    fn analyze_bool_literal(&mut self, bool_literal: &BoolLiteral) {
        self.set_expression_type(bool_literal.range(), Type::Bool);
    }

    fn analyze_string_literal(&mut self, string_literal: &StringLiteral) {
        // TODO
        self.set_expression_type(string_literal.range(), Type::Any);
    }

    fn analyze_type_expression(&mut self, _type_expression: &TypeExpression) {
        unimplemented!("Type analysis is not implemented yet");
    }

    // Scope

    fn get_current_scope_mut(&mut self) -> &mut ScopeInfo {
        self.scopes.get_mut(self.scope_stack.last().unwrap()).expect("Failed to get current scope")
    }

    fn get_current_scope(&self) -> &ScopeInfo {
        self.scopes.get(self.scope_stack.last().unwrap()).expect("Failed to get current scope")
    }

    fn enter_scope(&mut self, range: Range<Position>, is_breakable: bool) {
        let scope = ScopeInfo::new(range, is_breakable);
        self.scope_stack.push(scope.range().start);
        self.scopes.insert(scope.range().start, scope);
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop().expect("Failed to pop scope");
    }

    // Variable

    fn define_variable(&mut self, identifier: &Identifier) {
        self.tokens.insert(identifier.range(), NodeInfo::variable(
            identifier.range(),
            identifier.name.clone(),
            Type::Never,
        ));

        let scope = self.get_current_scope_mut();
        scope.declared_variables.insert(identifier.name.clone(), identifier.range());
    }

    fn get_variable_definition(&self, name: &str) -> Option<&VariableInfo> {
        for scope_position in self.scope_stack.iter().rev() {
            let scope = self.scopes.get(scope_position).expect("Failed to get scope");
            let defined_at = match scope.declared_variables.get(name) {
                Some(defined_at) => defined_at,
                None => continue,
            };
            match self.tokens.get(defined_at) {
                Some(NodeInfo::Variable(variable_info)) => return Some(variable_info),
                _ => panic!("Failed to get variable info"),
            }
        }
        None
    }

    fn get_variable_definition_mut(&mut self, name: &str) -> Option<&mut VariableInfo> {
        for scope_position in self.scope_stack.iter().rev() {
            let scope = self.scopes.get(scope_position).expect("Failed to get scope");
            let defined_at = match scope.declared_variables.get(name) {
                Some(defined_at) => defined_at,
                None => continue,
            };
            match self.tokens.get_mut(defined_at) {
                Some(NodeInfo::Variable(ref mut variable_info)) => return Some(variable_info),
                _ => panic!("Failed to get variable info"),
            }
        }
        None
    }

    fn set_variable_initialized(&mut self, name: &str, range: Range<Position>, type_: Type) {
        if self.get_variable_definition(&name).is_none() {
            self.errors.push(Error::uninitialized_variable(range.clone(), name));
        }
        self.get_current_scope_mut().initialized_variables.insert(
            name.to_string(), VariableInitializationInfo::new(range, type_));
    }

    fn is_variable_initialized(&self, name: &String) -> bool {
        for scope_position in self.scope_stack.iter().rev() {
            let scope = self.scopes.get(scope_position).expect("Failed to get scope");
            if scope.initialized_variables.contains_key(name) {
                return true;
            }
            if scope.declared_variables.contains_key(name) {
                return false;
            }
        }
        false
    }

    // Loop

    fn get_current_breakable(&self) -> Option<&ScopeInfo> {
        for scope_position in self.scope_stack.iter().rev() {
            let scope = self.scopes.get(scope_position).expect("Failed to get scope");
            if scope.is_breakable {
                return Some(scope);
            }
        }
        None
    }

    // Type

    fn is_assignable(&self, from: &Type, to: &Type) -> bool {
        match (from, to) {
            (Type::Never, _) => false,
            (_, Type::Never) => false,
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::Number, Type::Number) => true,
            (Type::Bool, Type::Bool) => true,
            _ => false,
        }
    }

    fn report_if_not_assignable(&mut self, from: &Type, to: &Type, range: &Range<Position>) -> bool {
        let is_assignable = self.is_assignable(from, to);
        if !is_assignable {
            self.errors.push(Error::unexpected_type(range.clone(), to, from));
        }

        is_assignable
    }

    fn get_variable_type(&self, name: &str) -> Type {
        match self.get_variable_definition(name) {
            Some(variable) => {
                variable.type_.clone()
            }
            None => panic!("Symbol {:?} is not declared", name)
        }
    }

    fn set_variable_type(&mut self, name: &str, type_: Type) {
        match self.get_variable_definition_mut(name) {
            Some(variable) => {
                variable.type_ = type_;
            }
            None => panic!("Symbol {:?} is not declared", name)
        }
    }

    /// If the given node is an expression, return its type.
    /// Otherwise, return Type::Void.
    fn get_expression_type(&self, node: &Node) -> Type {
        match self.tokens.get(&node.range()) {
            Some(type_) => match type_ {
                NodeInfo::Identifier(identifier) => self.get_variable_type(&identifier.name),
                NodeInfo::Expression(expression) => expression.type_.clone(),
                _ => Type::Void,
            },
            None => Type::Void,
        }
    }

    fn set_expression_type(&mut self, node_range: Range<Position>, type_: Type) {
        self.tokens.insert(node_range.clone(), NodeInfo::expression(node_range, type_));
    }

    fn evaluate_type_expression(&self, type_expression: &TypeExpression) -> Type {
        match type_expression.name.as_str() {
            "number" => Type::Number,
            "bool" => Type::Bool,
            _ => Type::Any,
        }
    }
}

pub fn analyze_program(program: &Program) -> Analysis {
    let mut context = Context::new();

    context.analyze_program(program);

    Analysis {
        tokens: context.tokens,
        errors: context.errors,
    }
}

pub fn analyze(source: &str) -> Analysis {
    let parse_result = parse(source);
    if !parse_result.errors.is_empty() {
        for error in parse_result.errors {
            eprintln!("{:?}", error);
        }
        panic!("Failed to parse");
    }

    analyze_program(&parse_result.program)
}

#[cfg(test)]
mod test {
    use crate::analyzer::{analyze_program, Analysis};
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
        use crate::analyzer::Type;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 15)..pos(0, 20), &Type::Number, &Type::Bool)
            ])
        }
    }

    mod type_analyze_in_assignment {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::Type;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"let x:number = 2; x = 3"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"let x:number = 2; x = false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 22)..pos(0, 27), &Type::Number, &Type::Bool)
            ])
        }
    }

    mod type_analyze_in_binary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::Type;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"1 + 2"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type_in_rhs() {
            test(r#"1 + false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 4)..pos(0, 9), &Type::Number, &Type::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_lhs() {
            test(r#"true + 1"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 0)..pos(0, 4), &Type::Number, &Type::Bool)
            ])
        }

        #[test]
        fn invalid_type_in_both() {
            test(r#"true + false"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 0)..pos(0, 4), &Type::Number, &Type::Bool),
                Error::unexpected_type(pos(0, 7)..pos(0, 12), &Type::Number, &Type::Bool)
            ])
        }
    }

    mod type_analyze_in_unary_expression {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::Type;
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn correct_type() {
            test(r#"!true"#).assert_errors(vec![])
        }

        #[test]
        fn invalid_type() {
            test(r#"!1"#).assert_errors(vec![
                Error::unexpected_type(pos(0, 1)..pos(0, 2), &Type::Bool, &Type::Number)
            ])
        }
    }

    mod type_analyze_for_loop_variable {
        use crate::analyzer::test::{test, AssertMethods};
        use crate::analyzer::Type;
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
                Error::unexpected_type(pos(3, 20)..pos(3, 21), &Type::Bool, &Type::Number)
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

    mod conditional_initialization {
        use crate::analysis::type_::Type;
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn conditional_initialization() {
            test(r#"let x
            if (true) { x = 1 }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_variable(pos(1, 24)..pos(1, 25), "x"),
                Error::uninitialized_variable(pos(2, 12)..pos(2, 13), "x")
            ])
        }

        #[test]
        fn full_initialization() {
            test(r#"let x
            if (true) { x = 1 } else { x = 2 }
            x"#).assert_errors(vec![])
        }

        #[test]
        fn nested_conditional_initialization() {
            test(r#"let x
            if (true) {
                if (true) { x = 1 } else { x = 2 }
            } else {
                if (true) { x = 3 }
            }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_variable(pos(4, 28)..pos(4, 29), "x"),
                Error::conditionally_initialized_variable(pos(2, 28)..pos(2, 29), "x"),
                Error::uninitialized_variable(pos(6, 12)..pos(6, 13), "x")
            ])
        }

        #[test]
        fn nested_full_initialization() {
            test(r#"let x
            if (true) {
                if (true) { x = 1 } else { x = 2 }
            } else {
                if (true) { x = 3 } else { x = 4 }
            }
            x"#).assert_errors(vec![])
        }

        #[test]
        fn full_initialization_but_incompatible_type() {
            test(r#"let x
            if (true) { x = 1 } else { x = true }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_as_different_type(pos(1, 39)..pos(1, 40), "x")
            ])
        }

        #[test]
        fn nested_full_initialization_but_incompatible_type() {
            test(r#"let x
            if (true) {
                if (true) { x = true } else { x = 2 }
            } else {
                if (true) { x = 3 } else { x = true }
            }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_as_different_type(pos(2, 46)..pos(2, 47), "x"),
                Error::conditionally_initialized_as_different_type(pos(4, 43)..pos(4, 44), "x"),
            ])
        }
    }

    trait AssertMethods {
        fn assert_errors(&self, expected: Vec<Error>);
    }

    impl AssertMethods for Analysis {
        fn assert_errors(&self, expected: Vec<Error>) {
            assert_eq!(self.errors, expected);
        }
    }

    fn test(src: &str) -> Analysis {
        let parse_result = parse(src);
        if !parse_result.errors.is_empty() {
            for error in parse_result.errors {
                eprintln!("{:?}", error);
            }
            panic!("Failed to parse");
        }

        analyze_program(&parse_result.program)
    }
}