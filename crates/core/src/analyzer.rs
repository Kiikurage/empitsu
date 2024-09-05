use crate::analysis::break_info::BreakInfo;
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::function_info::FunctionInfo;
use crate::analysis::identifier_info::IdentifierInfo;
use crate::analysis::node_info::NodeInfo;
use crate::analysis::return_info::ReturnInfo;
use crate::analysis::type_::Type;
use crate::analysis::type_expression_info::TypeExpressionInfo;
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
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, Range};

struct LocalVariable {
    name: String,
    defined_at: Range<Position>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExitReason {
    Normal,
    Break,
    Return,
}

struct Env {
    /// Scope metadata
    is_scope: bool,
    is_loop: bool,
    is_function: bool,
    exit_reason: ExitReason,

    /// Local information: not available from outer scopes
    // Variable
    variables_defined: HashSet<Range<Position>>,
    variables_defined_at: Vec<LocalVariable>,
    variables_initialized_at: HashMap<Range<Position>, Range<Position>>,
    variable_types: HashMap<Range<Position>, Type>,

    // return expressions not yet handled by the parent env
    return_expressions: Vec<Range<Position>>,

    // Variables that are not accessible from the current scope.
    inaccessible_symbols: HashMap<Range<Position>, NodeInfo>,

    /// Global information

    // TODO: Merge these into a single hashmap
    // Node-level information
    breaks: HashMap<Range<Position>, BreakInfo>,
    returns: HashMap<Range<Position>, ReturnInfo>,
    identifiers: HashMap<Range<Position>, IdentifierInfo>,
    expressions: HashMap<Range<Position>, ExpressionInfo>,
    type_expressions: HashMap<Range<Position>, TypeExpressionInfo>,
    functions: HashMap<Range<Position>, FunctionInfo>,
}

impl Env {
    fn new(
        is_scope: bool,
        is_loop: bool,
        is_function: bool,
    ) -> Self {
        Env {
            is_scope,
            is_loop,
            is_function,
            exit_reason: ExitReason::Normal,

            variables_defined: HashSet::new(),
            variables_defined_at: Vec::new(),
            variables_initialized_at: HashMap::new(),
            variable_types: HashMap::new(),
            return_expressions: Vec::new(),
            inaccessible_symbols: HashMap::new(),

            breaks: HashMap::new(),
            returns: HashMap::new(),
            identifiers: HashMap::new(),
            expressions: HashMap::new(),
            type_expressions: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn merge(&mut self, mut child_env: Env) {
        if child_env.is_scope {
            child_env.encapsulate();
        }

        self.variables_defined.extend(child_env.variables_defined);
        self.variables_defined_at.extend(child_env.variables_defined_at);
        self.variables_initialized_at.extend(child_env.variables_initialized_at);
        self.variable_types.extend(child_env.variable_types);

        self.breaks.extend(child_env.breaks);
        self.returns.extend(child_env.returns);
        self.identifiers.extend(child_env.identifiers);
        self.expressions.extend(child_env.expressions);
        self.type_expressions.extend(child_env.type_expressions);
        self.inaccessible_symbols.extend(child_env.inaccessible_symbols);
        self.exit_reason = child_env.exit_reason;
        self.return_expressions.extend(child_env.return_expressions);
    }

    /// Encapsulate variables so that they are not accessible from the outer scope.
    fn encapsulate(&mut self) {
        // Copy variables that are going to be inaccessible
        for LocalVariable { name, defined_at } in self.variables_defined_at.iter() {
            let type_ = self.variable_types.get(defined_at).unwrap_or(&Type::Unknown).clone();
            self.inaccessible_symbols.insert(defined_at.clone(), NodeInfo::variable(
                defined_at.clone(),
                name.clone(),
                type_,
            ));
            self.variable_types.remove(defined_at);
            self.variables_initialized_at.remove(defined_at);
        }
        for function_info in self.functions.values() {
            self.inaccessible_symbols.insert(
                function_info.range().clone(),
                NodeInfo::FunctionInfo(function_info.clone()),
            );
        }

        self.variables_defined.clear();
        self.variables_defined_at.clear();

        if self.is_loop && matches!(self.exit_reason, ExitReason::Break) {
            self.exit_reason = ExitReason::Normal;
        }
        if self.is_function && matches!(self.exit_reason, ExitReason::Return) {
            self.exit_reason = ExitReason::Normal;
        }
    }

    fn into_tokens(mut self) -> HashMap<Range<Position>, NodeInfo> {
        self.encapsulate();

        let mut tokens = HashMap::new();
        for (range, node_info) in self.inaccessible_symbols {
            tokens.insert(range, node_info);
        }
        for (range, break_info) in self.breaks {
            tokens.insert(range, NodeInfo::Break(break_info));
        }
        for (range, return_info) in self.returns {
            tokens.insert(range, NodeInfo::Return(return_info));
        }
        for (range, identifier_info) in self.identifiers {
            tokens.insert(range, NodeInfo::Identifier(identifier_info));
        }
        for (range, expression_info) in self.expressions {
            tokens.insert(range, NodeInfo::Expression(expression_info));
        }
        for (range, type_expression_info) in self.type_expressions {
            tokens.insert(range, NodeInfo::TypeExpression(type_expression_info));
        }
        tokens
    }
}

struct Analyzer {
    /// Environment can be forked to analyze conditional branches.
    envs: Vec<Env>,
    errors: Vec<Error>,
}

impl Analyzer {
    pub fn new() -> Analyzer {
        Analyzer {
            envs: vec![Env::new(false, false, false)],
            errors: vec![],
        }
    }

    pub fn into_analysis(mut self) -> Analysis {
        while self.envs.len() > 1 {
            self.exit_env();
        }
        let env = self.envs.pop().unwrap();

        Analysis::new(env.into_tokens(), self.errors)
    }

    // Main analysis functions

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
            if !matches!(self.current_env().exit_reason, ExitReason::Normal) {
                self.errors.push(Error::unreachable_code(node.range()));
                break;
            }
            self.analyze_node(node);
        }
    }

    pub fn analyze_program(&mut self, program: &Program) {
        self.enter_env(Env::new(true, false, false));

        self.analyze_nodes(&program.statements);

        self.exit_env();
    }

    fn analyze_if_statement(&mut self, if_statement: &IfStatement) {
        self.analyze_node(&if_statement.condition);
        let condition_type = self.get_expression_type_or_unknown(&if_statement.condition.range());
        if !self.is_assignable(&condition_type, &Type::Bool) {
            self.errors.push(Error::unexpected_type_in_if_condition(if_statement.condition.range(), &condition_type));
        }

        self.enter_env(Env::new(false, false, false));
        self.analyze_node(&if_statement.true_branch);
        let true_env = self.exit_env_without_merge();

        self.enter_env(Env::new(false, false, false));
        if let Some(false_branch) = &if_statement.false_branch {
            self.analyze_node(false_branch.deref());
        }
        let false_env = self.exit_env_without_merge();

        let child_env = self.reconcile_branched_envs(true_env, false_env);
        self.merge_env(child_env);
    }

    fn analyze_for_statement(&mut self, for_statement: &ForStatement) {
        self.enter_env(Env::new(true, true, false));

        self.define_variable(for_statement.variable.range(), for_statement.variable.name.clone());
        self.set_variable_type(for_statement.variable.range(), Type::Number);
        self.set_variable_initialized(for_statement.variable.range(), for_statement.variable.range());

        // self.analyze_node(&for_statement.iterable); // TODO
        self.analyze_node(&for_statement.body);

        self.exit_env();
    }

    fn analyze_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) {
        if let Some(initializer) = &variable_declaration.initializer {
            self.analyze_node(initializer);
        }

        self.define_variable(variable_declaration.range(), variable_declaration.name.name.clone());
        if let Some(type_expression) = &variable_declaration.type_ {
            self.analyze_type_expression(type_expression);
            let type_ = self.get_type_expression_type(&type_expression.range());
            self.set_variable_type(variable_declaration.range(), type_);
        }
        if let Some(initializer) = &variable_declaration.initializer {
            let initializer_type = self.get_expression_type_or_unknown(&initializer.range());
            let expected_type = self.get_variable_type(&variable_declaration.range());
            self.report_if_not_assignable(&initializer_type, &expected_type, &initializer.range());

            if variable_declaration.type_.is_none() {
                self.set_variable_type(variable_declaration.range(), initializer_type.clone());
            }

            self.set_variable_initialized(variable_declaration.range(), variable_declaration.range());
        }
    }

    fn analyze_function(&mut self, function: &Function) {
        let name = function.interface.name.clone()
            .map(|id| id.name)
            .unwrap_or("anonymous".to_string())
            .clone();

        let mut parameter_types = vec![];
        for parameter in &function.interface.parameters {
            self.analyze_type_expression(&parameter.type_);
            let type_ = self.get_type_expression_type(&parameter.type_.range());
            parameter_types.push(type_);
        }

        self.analyze_type_expression(function.interface.return_type.deref());
        let return_type = self.get_type_expression_type(&function.interface.return_type.range());

        let function_type_ = Type::Function(parameter_types, Box::new(return_type.clone()));
        let function_info = FunctionInfo::new(function.range(), name, function_type_.clone());

        self.current_env_mut().functions.insert(function.range(), function_info);

        // Function body
        self.enter_env(Env::new(true, false, true));
        self.analyze_nodes(&function.body);

        // Check return expression's type
        let mut errors = Vec::new();
        for return_range in self.current_env().return_expressions.iter() {
            let return_range = return_range.clone();
            let return_info = self.current_env()
                .returns.get(&return_range).unwrap();

            if !self.is_assignable(&return_info.return_value_type, &return_type) {
                errors.push(Error::unexpected_type(
                    return_range,
                    &return_type,
                    &return_info.return_value_type,
                ));
            }
        }
        self.errors.extend(errors);

        // Check the last expression's type
        if !matches!(self.current_env().exit_reason, ExitReason::Return) {
            assert!(
                matches!(self.current_env().exit_reason, ExitReason::Normal),
                "Unexpected exit reason {:?}",
                self.current_env().exit_reason,
            );
            let (actual_return_type, return_range) = match function.body.last() {
                Some(node) => (
                    self.get_expression_type_or_void(&node.range()),
                    node.range(),
                ),
                _ => (Type::Void, function.range()),
            };
            if !self.is_assignable(&actual_return_type, &return_type) {
                self.errors.push(Error::unexpected_type(
                    return_range,
                    &return_type,
                    &actual_return_type,
                ));
            }
        }

        self.exit_env();

        self.set_expression_type(function.range(), function_type_);
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

    fn analyze_return_(&mut self, return_: &Return) {
        if self.in_function_env() {
            self.current_env_mut().exit_reason = ExitReason::Return;
        } else {
            self.errors.push(Error::invalid_syntax(return_.range(), "Return statement is not inside a function"));
        }

        let return_type = match &return_.value {
            Some(value) => {
                self.analyze_node(value);
                self.get_expression_type_or_unknown(&value.range())
            }
            None => Type::Void,
        };
        self.current_env_mut().return_expressions.push(return_.range());

        self.current_env_mut().returns.insert(
            return_.range(),
            ReturnInfo::new(return_.range(), return_type),
        );
        self.set_expression_type(return_.range(), Type::Never);
    }

    fn analyze_break(&mut self, break_: &Break) {
        if self.in_loop_env() {
            self.current_env_mut().exit_reason = ExitReason::Break;
        } else {
            self.errors.push(Error::break_not_in_loop(break_.range()));
        }

        self.current_env_mut().breaks.insert(
            break_.range(),
            BreakInfo::new(break_.range()),
        );
        self.set_expression_type(break_.range(), Type::Never);
    }

    fn analyze_if_expression(&mut self, if_expression: &IfExpression) {
        self.analyze_node(&if_expression.condition);
        let condition_type = self.get_expression_type_or_unknown(&if_expression.condition.range());
        if !self.is_assignable(&condition_type, &Type::Bool) {
            self.errors.push(Error::unexpected_type_in_if_condition(if_expression.condition.range(), &condition_type));
        }

        self.enter_env(Env::new(false, false, false));
        self.analyze_node(&if_expression.true_branch);
        let true_branch_type_ = self.get_expression_type_or_void(&if_expression.true_branch.range());
        let true_env = self.exit_env_without_merge();

        self.enter_env(Env::new(false, false, false));
        self.analyze_node(&if_expression.false_branch);
        let false_branch_type_ = self.get_expression_type_or_void(&if_expression.false_branch.range());
        let false_env = self.exit_env_without_merge();

        let child_env = self.reconcile_branched_envs(true_env, false_env);
        self.merge_env(child_env);

        if self.is_assignable(
            &false_branch_type_,
            &true_branch_type_,
        ) {
            self.set_expression_type(if_expression.range(), true_branch_type_);
        } else {
            self.errors.push(Error::conditional_if_expression_type(
                if_expression.range(),
                true_branch_type_,
                false_branch_type_,
            ));
            self.set_expression_type(if_expression.range(), Type::Unknown);
        }
    }

    fn analyze_block(&mut self, block: &Block) {
        self.enter_env(Env::new(true, false, false));
        self.analyze_nodes(&block.nodes);

        let exit_reason = {
            let env = self.exit_env_without_merge();
            let exit_reason = env.exit_reason.clone();
            self.merge_env(env);
            exit_reason
        };

        let block_type = if !matches!(exit_reason, ExitReason::Normal) {
            Type::Void
        } else {
            block.nodes.last()
                .map(|last_node| self.get_expression_type_or_void(&last_node.range()))
                .unwrap_or(Type::Void)
        };
        self.set_expression_type(block.range(), block_type);
    }

    fn analyze_assignment_expression(&mut self, assignment_expression: &AssignmentExpression) {
        self.analyze_node(&assignment_expression.rhs);
        let rhs_type = self.get_expression_type_or_unknown(&assignment_expression.rhs.range());

        match assignment_expression.lhs.deref() {
            Node::Identifier(identifier) => {
                match self.get_variable_definition_position(&identifier.name) {
                    Some(defined_at) => {
                        let defined_at = defined_at.clone();
                        self.set_identifier_info(assignment_expression.lhs.range(), identifier.name.clone(), Some(defined_at.clone()));

                        let lhs_type = self.get_variable_type(&defined_at);
                        self.report_if_not_assignable(&rhs_type, &lhs_type, &assignment_expression.rhs.range());

                        if !self.is_variable_initialized(defined_at.clone()) {
                            self.set_variable_initialized(defined_at.clone(), assignment_expression.range());
                            self.set_variable_type(defined_at.clone(), rhs_type.clone());
                        }
                    }
                    None => {
                        self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                        self.define_variable(identifier.range(), identifier.name.clone());
                        self.set_variable_initialized(identifier.range(), assignment_expression.range());
                        self.set_variable_type(identifier.range(), rhs_type.clone());
                    }
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
        let lhs_type = self.get_expression_type_or_unknown(&binary_expression.lhs.range());
        let rhs_type = self.get_expression_type_or_unknown(&binary_expression.rhs.range());
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
        let operand_type = self.get_expression_type_or_unknown(&unary_expression.operand.range());

        let (expected_operand_type, result_type) = match unary_expression.operator {
            PunctuationKind::Minus => (Type::Number, Type::Number),
            PunctuationKind::Exclamation => (Type::Bool, Type::Bool),
            _ => panic!("Unexpected unary operator {:?}", unary_expression.operator),
        };

        self.report_if_not_assignable(&operand_type, &expected_operand_type, &unary_expression.operand.range());

        self.set_expression_type(unary_expression.range(), result_type);
    }

    fn analyze_call_expression(&mut self, _call_expression: &CallExpression) {
        // 引数リストが正しく埋められているか
        unimplemented!("Call analysis is not implemented yet");
    }

    fn analyze_member_expression(&mut self, _member_expression: &MemberExpression) {
        unimplemented!("Member analysis is not implemented yet");
    }

    fn analyze_identifier(&mut self, identifier: &Identifier) {
        let defined_at = match self.get_variable_definition_position(&identifier.name) {
            Some(defined_at) => {
                let defined_at = defined_at.clone();
                self.set_identifier_info(identifier.range(), identifier.name.clone(), Some(defined_at.clone()));
                defined_at
            }
            None => {
                self.errors.push(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                self.define_variable(identifier.range(), identifier.name.clone());
                self.set_variable_type(identifier.range(), Type::Unknown);
                self.set_variable_initialized(identifier.range(), identifier.range());
                identifier.range()
            }
        };

        if !self.is_variable_initialized(defined_at.clone()) {
            self.errors.push(Error::uninitialized_variable(identifier.range(), identifier.name.clone()));
            self.set_variable_type(identifier.range(), Type::Unknown);
            self.set_variable_initialized(identifier.range(), identifier.range());
        }
    }

    fn set_identifier_info(&mut self, range: Range<Position>, name: String, defined_at: Option<Range<Position>>) {
        self.current_env_mut().identifiers.insert(range.clone(), IdentifierInfo::new(range, name, defined_at));
    }

    fn analyze_number_literal(&mut self, number_literal: &NumberLiteral) {
        self.set_expression_type(number_literal.range(), Type::Number);
    }

    fn analyze_bool_literal(&mut self, bool_literal: &BoolLiteral) {
        self.set_expression_type(bool_literal.range(), Type::Bool);
    }

    fn analyze_string_literal(&mut self, string_literal: &StringLiteral) {
        // TODO
        self.set_expression_type(string_literal.range(), Type::Unknown);
    }

    fn analyze_type_expression(&mut self, type_expression: &TypeExpression) {
        let type_ = match type_expression.name.as_str() {
            "number" => Type::Number,
            "bool" => Type::Bool,
            _ => {
                unimplemented!("Type analysis for identifier is not implemented yet");
            }
        };

        self.set_type_expression_type(type_expression.range(), type_);
    }

    // Break

    /// Get the position of the env that can be exited by break expression.
    fn in_loop_env(&self) -> bool {
        for env in self.envs.iter().rev() {
            if env.is_loop {
                return true;
            }
            if env.is_function {
                return false;
            }
        }
        false
    }

    /// Check if the current env is in function env.
    fn in_function_env(&self) -> bool {
        for env in self.envs.iter().rev() {
            if env.is_function {
                return true;
            }
        }
        false
    }

    // Env

    /// Get the current env
    fn current_env(&self) -> &Env {
        self.envs.last().unwrap()
    }

    /// Get the current env as mutable
    fn current_env_mut(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap()
    }

    /// Enter a new env.
    fn enter_env(&mut self, env: Env) {
        self.envs.push(env);
    }

    /// Exit current env and merge the result to the parent env.
    fn exit_env(&mut self) {
        let env = self.exit_env_without_merge();
        self.merge_env(env);
    }

    /// Exit current env, but do not merge the result to the parent env.
    /// This is useful when you want to emulate branched env like if-statement.
    fn exit_env_without_merge(&mut self) -> Env {
        self.envs.pop().unwrap()
    }

    /// Merge the given env to the parent env.
    fn merge_env(&mut self, env: Env) {
        self.current_env_mut().merge(env)
    }

    /// Reconcile two branched envs.
    fn reconcile_branched_envs(&mut self, mut env1: Env, mut env2: Env) -> Env {
        let mut result = Env::new(false, false, false);

        // States inaccessible from outer scope are not needed to be reconciled
        env1.encapsulate();
        env2.encapsulate();

        // Variables defined in the outer scope must be initialized to the same type in both branches
        for (defined_at, env1_initialized_at) in env1.variables_initialized_at.iter() {
            match env2.variables_initialized_at.get(defined_at) {
                Some(env2_initialized_at) => {
                    let env1_type = env1.variable_types.get(defined_at).unwrap_or(&Type::Unknown).clone();
                    let env2_type = env2.variable_types.get(defined_at).unwrap_or(&Type::Unknown).clone();

                    let type_ = if self.is_assignable(&env2_type, &env1_type) {
                        env1_type
                    } else {
                        self.errors.push(Error::conditionally_initialized_as_different_type(env2_initialized_at.clone(), env1_type, env2_type));
                        Type::Unknown
                    };

                    result.variables_initialized_at.insert(defined_at.clone(), env1_initialized_at.clone());
                    result.variable_types.insert(defined_at.clone(), type_);
                }
                None => {
                    self.errors.push(Error::conditionally_initialized_variable(env1_initialized_at.clone()));

                    let type_ = env1.variable_types.get(defined_at).unwrap_or(&Type::Unknown).clone();
                    result.variables_initialized_at.insert(defined_at.clone(), env1_initialized_at.clone());
                    result.variable_types.insert(defined_at.clone(), type_);
                }
            }
        }
        for (defined_at, env2_initialized_at) in env2.variables_initialized_at.iter() {
            if !env1.variables_initialized_at.contains_key(defined_at) {
                self.errors.push(Error::conditionally_initialized_variable(env2_initialized_at.clone()));

                let type_ = env2.variable_types.get(defined_at).unwrap_or(&Type::Unknown).clone();
                result.variables_initialized_at.insert(defined_at.clone(), env2_initialized_at.clone());
                result.variable_types.insert(defined_at.clone(), type_);
            }
        }
        env1.variables_initialized_at.clear();
        env1.variable_types.clear();
        env2.variables_initialized_at.clear();
        env2.variable_types.clear();

        if env1.exit_reason != env2.exit_reason {
            env1.exit_reason = ExitReason::Normal;
            env2.exit_reason = ExitReason::Normal;
        }

        result.merge(env1);
        result.merge(env2);
        result
    }

    // Variable

    fn define_variable(&mut self, defined_at: Range<Position>, name: String) {
        self.current_env_mut().variables_defined_at.push(LocalVariable { name, defined_at });
    }

    fn get_variable_definition_position(&self, name: &str) -> Option<&Range<Position>> {
        for env in self.envs.iter().rev() {
            for local_variable in env.variables_defined_at.iter().rev() {
                if local_variable.name == name {
                    return Some(&local_variable.defined_at);
                }
            }
        }
        None
    }

    fn is_variable_defined(&self, name: &str) -> bool {
        self.get_variable_definition_position(name).is_some()
    }

    fn set_variable_type(&mut self, defined_at: Range<Position>, type_: Type) {
        self.current_env_mut().variable_types.insert(defined_at, type_);
    }

    fn get_variable_type(&self, defined_at: &Range<Position>) -> Type {
        for env in self.envs.iter().rev() {
            if let Some(type_) = env.variable_types.get(defined_at) {
                return type_.clone();
            }
        }
        Type::Unknown
    }

    fn set_variable_initialized(&mut self, defined_at: Range<Position>, initialized_at: Range<Position>) {
        self.current_env_mut().variables_initialized_at.insert(defined_at, initialized_at);
    }

    fn is_variable_initialized(&self, defined_at: Range<Position>) -> bool {
        for env in self.envs.iter().rev() {
            if env.variables_initialized_at.contains_key(&defined_at) {
                return true;
            }

            // If the variable is defined in the current scope, but not initialized,
            if env.variables_defined.contains(&defined_at) {
                return false;
            }
        }
        false
    }

    // Expression

    /// Set the type of the expression.
    fn set_expression_type(&mut self, range: Range<Position>, type_: Type) {
        self.current_env_mut().expressions.insert(
            range.clone(),
            ExpressionInfo::new(
                range,
                type_,
            ),
        );
    }

    /// Get the type of the expression.
    fn get_expression_type(&self, range: &Range<Position>) -> Option<Type> {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.expressions.get(range) {
                return Some(info.type_.clone());
            }
            if let Some(info) = env.identifiers.get(range) {
                return match info.defined_at {
                    Some(ref defined_at) => Some(self.get_variable_type(defined_at)),
                    None => None,
                };
            }
        }
        None
    }

    fn get_expression_type_or_void(&self, range: &Range<Position>) -> Type {
        self.get_expression_type(range).unwrap_or(Type::Void)
    }

    fn get_expression_type_or_unknown(&self, range: &Range<Position>) -> Type {
        self.get_expression_type(range).unwrap_or(Type::Unknown)
    }

    // Type Expression

    /// Set the type of the type expression.
    fn set_type_expression_type(&mut self, range: Range<Position>, type_: Type) {
        self.current_env_mut().type_expressions.insert(
            range.clone(),
            TypeExpressionInfo::new(
                range,
                type_,
            ),
        );
    }

    /// Get the type of the type expression.
    fn get_type_expression_type(&self, range: &Range<Position>) -> Type {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.type_expressions.get(range) {
                return info.type_.clone();
            }
        }
        Type::Unknown
    }

    fn is_assignable(&self, actual: &Type, expected: &Type) -> bool {
        match (actual, expected) {
            // Never values cannot be assigned to any type
            // since they shouldn't be used as values.
            (Type::Never, _) => false,
            (_, Type::Never) => false,

            // Unknown type is collection of all possible types.
            // It can be assigned only to itself, but can accept
            // any type.
            (Type::Unknown, Type::Unknown) => true,
            (Type::Unknown, _) => false,
            (_, Type::Unknown) => true,

            // Normal type checking
            (Type::Number, Type::Number) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return false;
                }
                for (param1, param2) in params1.iter().zip(params2.iter()) {
                    if !self.is_assignable(param1, param2) {
                        return false;
                    }
                }
                self.is_assignable(ret1, ret2)
            }
            _ => false,
        }
    }

    fn report_if_not_assignable(&mut self, actual: &Type, expected: &Type, range: &Range<Position>) -> bool {
        if !self.is_assignable(actual, expected) {
            self.errors.push(Error::unexpected_type(range.clone(), expected, actual));
            false
        } else {
            true
        }
    }
}

pub fn analyze_program(program: &Program) -> Analysis {
    let mut analyzer = Analyzer::new();
    analyzer.analyze_program(program);

    analyzer.into_analysis()
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
        fn conditional_initialization_true_branch() {
            test(r#"let x
            if (true) { x = 1 }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_variable(pos(1, 24)..pos(1, 29)),
            ])
        }

        #[test]
        fn conditional_initialization_false_branch() {
            test(r#"let x
            if (true) {} else { x = 1 }
            x"#).assert_errors(vec![
                Error::conditionally_initialized_variable(pos(1, 32)..pos(1, 37)),
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
                Error::conditionally_initialized_variable(pos(4, 28)..pos(4, 33)),
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
                Error::conditionally_initialized_as_different_type(pos(1, 39)..pos(1, 47), Type::Number, Type::Bool)
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
                Error::conditionally_initialized_as_different_type(pos(2, 46)..pos(2, 51), Type::Bool, Type::Number),
                Error::conditionally_initialized_as_different_type(pos(4, 43)..pos(4, 51), Type::Number, Type::Bool)
            ])
        }

        #[test]
        fn declare_same_name_variable_in_inner_scope() {
            test(r#"
                let x
                {
                    let x = 100
                }
                x
            "#).assert_errors(vec![
                Error::uninitialized_variable(pos(5, 16)..pos(5, 17), "x")
            ])
        }
    }

    mod function {
        use crate::analysis::type_::Type;
        use crate::analyzer::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn reassign_function_object() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(p1: number, p2: bool):number { return 2 }
            "#).assert_errors(vec![])
        }

        #[test]
        fn function_with_additional_parameter_is_different_type() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(p1: number, p2: bool, p3: bool):number { return 1 }
            "#).assert_errors(vec![
                Error::unexpected_type(
                    pos(2, 16)..pos(2, 70),
                    &Type::Function(vec![Type::Number, Type::Bool], Box::new(Type::Number)),
                    &Type::Function(vec![Type::Number, Type::Bool, Type::Bool], Box::new(Type::Number))
                )
            ])
        }

        #[test]
        fn function_with_few_parameter_is_different_type() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(p1: number):number { return 1 }
            "#).assert_errors(vec![
                Error::unexpected_type(
                    pos(2, 16)..pos(2, 50),
                    &Type::Function(vec![Type::Number, Type::Bool], Box::new(Type::Number)),
                    &Type::Function(vec![Type::Number], Box::new(Type::Number))
                )
            ])
        }

        #[test]
        fn function_with_different_parameter_type_is_different_type() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(p1: number, p2: number):number { return 1 }
            "#).assert_errors(vec![
                Error::unexpected_type(
                    pos(2, 16)..pos(2, 62),
                    &Type::Function(vec![Type::Number, Type::Bool], Box::new(Type::Number)),
                    &Type::Function(vec![Type::Number, Type::Number], Box::new(Type::Number))
                )
            ])
        }

        #[test]
        fn function_with_different_return_type_is_different_type() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(p1: number, p2: bool):bool { return true }
            "#).assert_errors(vec![
                Error::unexpected_type(
                    pos(2, 16)..pos(2, 61),
                    &Type::Function(vec![Type::Number, Type::Bool], Box::new(Type::Number)),
                    &Type::Function(vec![Type::Number, Type::Bool], Box::new(Type::Bool))
                )
            ])
        }

        #[test]
        fn function_with_different_parameter_name_is_same_type() {
            test(r#"
            let x = fn(p1: number, p2: bool):number { return 1 }
            x = fn(q1: number, q2: bool):number { return 2 }
            "#).assert_errors(vec![])
        }

        #[test]
        fn check_actual_return_type() {
            test(r#"
            let x = fn():number { true }
            "#).assert_errors(vec![
                Error::unexpected_type(
                    pos(1, 34)..pos(1, 38),
                    &Type::Number,
                    &Type::Bool
                )
            ])
        }

        #[test]
        fn return_statement_is_not_in_function() {
            test(r#"
            return 1
            "#).assert_errors(vec![
                Error::invalid_syntax(pos(1, 12)..pos(1, 20), "Return statement is not inside a function")
            ])
        }

        #[test]
        fn return_value_is_incorrect_with_definition() {
            test(r#"
            let x = fn():number { return true }
            "#).assert_errors(vec![
                Error::invalid_syntax(pos(1, 34)..pos(1, 45), "Expected type is number, but actual type is bool")
            ])
        }

        #[test]
        fn analyze_actual_return_type_from_function_body_with_if_statement() {
            test(r#"
            let x = fn(): number {
                if (true) {
                    return true
                }

                false
            }
            "#).assert_errors(vec![
                Error::unexpected_type(pos(3, 20)..pos(3, 31), &Type::Number, &Type::Bool),
                Error::unexpected_type(pos(6, 16)..pos(6, 21), &Type::Number, &Type::Bool)
            ])
        }

        #[test]
        fn analyze_branched_return_types() {
            test(r#"
            let x = fn(): number {
                if (true) {
                    if (true) {
                        return true
                    } else {
                        1
                    }
                    return false
                } else {
                    if (true) {
                        1
                    } else {
                        return false
                    }
                }

                false
            }
            "#).assert_errors(vec![
                Error::unexpected_type(pos(4, 24)..pos(4, 35), &Type::Number, &Type::Bool),
                Error::unexpected_type(pos(8, 20)..pos(8, 32), &Type::Number, &Type::Bool),
                Error::unexpected_type(pos(13, 24)..pos(13, 36), &Type::Number, &Type::Bool),
                Error::unexpected_type(pos(17, 16)..pos(17, 21), &Type::Number, &Type::Bool)
            ])
        }

        #[test]
        fn break_in_function() {
            test(r#"
            let y
            for (i in iterable) {
                let x = fn(): number {
                    break
                }
                y = 1
            }
            y
            "#).assert_errors(vec![
                Error::break_not_in_loop(pos(4, 20)..pos(4, 25)),
                Error::unexpected_type(pos(4, 20)..pos(4, 25), &Type::Number, &Type::Never)
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