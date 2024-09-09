use crate::analysis::analyzer_context::AnalyzerContext;
use crate::analysis::env::{Env, ExitReason, SymbolKind};
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::type_::Type;
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
use crate::punctuation_kind::PunctuationKind;
use std::ops::Deref;

pub fn analyze_program(program: &Program) -> Analysis {
    let mut ctx = AnalyzerContext::new();

    ctx.enter_env(Env::new(false, false));

    analyze_nodes(&mut ctx, &program.statements);

    ctx.exit_env();

    ctx.into_analysis()
}

fn analyze_node(ctx: &mut AnalyzerContext, node: &Node) {
    match node {
        Node::Program(..) => unreachable!("Program node should not be analyzed directly"),
        Node::IfStatement(if_statement) => analyze_if_statement(ctx, if_statement),
        Node::ForStatement(for_statement) => analyze_for_statement(ctx, for_statement),
        Node::VariableDeclaration(variable_declaration) => analyze_variable_declaration(ctx, variable_declaration),
        Node::FunctionDeclaration(function) => analyze_function(ctx, function),
        Node::StructDeclaration(struct_) => analyze_struct_declaration(ctx, struct_),
        Node::InterfaceDeclaration(interface) => analyze_interface(ctx, interface),
        Node::ImplStatement(impl_statement) => analyze_impl_statement(ctx, impl_statement),
        Node::Return(return_) => analyze_return_(ctx, return_),
        Node::Break(break_) => analyze_break(ctx, break_),
        Node::FunctionExpression(function) => analyze_function(ctx, function),
        Node::IfExpression(if_expression) => analyze_if_expression(ctx, if_expression),
        Node::Block(block) => analyze_block(ctx, block),
        Node::AssignmentExpression(assignment_expression) => analyze_assignment_expression(ctx, assignment_expression),
        Node::BinaryExpression(binary_expression) => analyze_binary_expression(ctx, binary_expression),
        Node::UnaryExpression(unary_expression) => analyze_unary_expression(ctx, unary_expression),
        Node::CallExpression(call_expression) => analyze_call_expression(ctx, call_expression),
        Node::MemberExpression(member_expression) => analyze_member_expression(ctx, member_expression),
        Node::Identifier(identifier) => analyze_identifier(ctx, identifier),
        Node::NumberLiteral(number_literal) => analyze_number_literal(ctx, number_literal),
        Node::BoolLiteral(bool_literal) => analyze_bool_literal(ctx, bool_literal),
        Node::StringLiteral(string_literal) => analyze_string_literal(ctx, string_literal),
        Node::TypeExpression(type_expression) => analyze_type_expression(ctx, type_expression),
    }
}

fn analyze_nodes(ctx: &mut AnalyzerContext, nodes: &[Node]) {
    for node in nodes {
        if ctx.is_exited() {
            ctx.add_error(Error::unreachable_code(node.range()));
            break;
        }
        analyze_node(ctx, node);
    }
}

fn analyze_if_statement(ctx: &mut AnalyzerContext, if_statement: &IfStatement) {
    analyze_node(ctx, &if_statement.condition);
    ctx.assert_type(&if_statement.condition.range(), &Type::Bool);

    ctx.enter_env(Env::new(false, false));
    analyze_node(ctx, &if_statement.true_branch);
    let true_env = ctx.exit_env_without_merge();

    ctx.enter_env(Env::new(false, false));
    if let Some(false_branch) = &if_statement.false_branch {
        analyze_node(ctx, false_branch.deref());
    }
    let false_env = ctx.exit_env_without_merge();

    let child_env = ctx.reconcile_branched_envs(true_env, false_env);
    ctx.merge_env(child_env);
}

fn analyze_for_statement(ctx: &mut AnalyzerContext, for_statement: &ForStatement) {
    ctx.enter_env(Env::new(true, false));

    ctx.define_symbol(for_statement.variable.range(), for_statement.variable.name.clone(), SymbolKind::Variable);
    ctx.set_variable_type(for_statement.variable.range(), Type::Number);
    ctx.set_variable_initialized(for_statement.variable.range(), for_statement.variable.range());
    ctx.add_expression(ExpressionInfo::variable(for_statement.variable.range(), Some(for_statement.variable.range())));

    // analyze_node(&ctx, for_statement.iterable); // TODO
    analyze_node(ctx, &for_statement.body);

    let exit_by_break = {
        let env = ctx.exit_env_without_merge();
        let exit_by_break = matches!(env.exit_reason, ExitReason::Break);
        ctx.merge_env(env);
        exit_by_break
    };

    // Catch exit-by-break
    if exit_by_break {
        ctx.set_exit_reason(ExitReason::Normal);
    }
}

fn analyze_variable_declaration(ctx: &mut AnalyzerContext, variable_declaration: &VariableDeclaration) {
    if let Some(initializer) = &variable_declaration.initializer {
        analyze_node(ctx, initializer);
    }

    ctx.define_symbol(variable_declaration.range(), variable_declaration.name.name.clone(), SymbolKind::Variable);
    if let Some(type_expression) = &variable_declaration.type_ {
        analyze_type_expression(ctx, type_expression);
        let type_ = ctx.get_type_expression_type(&type_expression.range());
        ctx.set_variable_type(variable_declaration.range(), type_);
    }
    if let Some(initializer) = &variable_declaration.initializer {
        let initializer_type = ctx.get_expression_type(&initializer.range()).unwrap_or(Type::Unknown);
        let expected_type = ctx.get_variable_type(&variable_declaration.range());
        ctx.assert_assignable(&initializer_type, &expected_type, &initializer.range());

        if variable_declaration.type_.is_none() {
            ctx.set_variable_type(variable_declaration.range(), initializer_type.clone());
        }

        ctx.set_variable_initialized(variable_declaration.range(), variable_declaration.range());
    }
}

fn analyze_function(ctx: &mut AnalyzerContext, function: &Function) {
    let name = function.interface.name.clone()
        .map(|id| id.name)
        .unwrap_or("anonymous".to_string())
        .clone();

    let mut parameter_types = vec![];
    for parameter in &function.interface.parameters {
        analyze_type_expression(ctx, &parameter.type_);
        let type_ = ctx.get_type_expression_type(&parameter.type_.range());
        parameter_types.push(type_);
    }

    analyze_type_expression(ctx, function.interface.return_type.deref());
    let return_type = ctx.get_type_expression_type(&function.interface.return_type.range());

    let function_type_ = Type::Function(parameter_types, Box::new(return_type.clone()));

    ctx.add_function(function.range(), name, function_type_.clone());

    // Function body
    ctx.enter_env(Env::new(false, true));
    for parameter in &function.interface.parameters {
        ctx.define_symbol(parameter.range(), parameter.name.name.clone(), SymbolKind::Variable);
        ctx.set_variable_type(parameter.range(), ctx.get_type_expression_type(&parameter.type_.range()));
        ctx.set_variable_initialized(parameter.range(), parameter.range());
    }
    analyze_nodes(ctx, &function.body);

    // Check return expression's type
    let mut errors = Vec::new();
    for return_range in ctx.returns().iter() {
        let return_range = return_range.clone();
        let return_info = ctx.get_return(&return_range).unwrap();

        if !ctx.is_assignable(&return_info.return_value_type, &return_type) {
            errors.push(Error::unexpected_type(
                return_range,
                &return_type,
                &return_info.return_value_type,
            ));
        }
    }
    for error in errors {
        ctx.add_error(error);
    }

    // Check the last expression's type
    if !matches!(ctx.exit_reason(), ExitReason::Return) {
        let (actual_return_type, return_range) = match function.body.last() {
            Some(node) => (
                ctx.get_expression_type(&node.range()).unwrap_or(Type::Void),
                node.range(),
            ),
            _ => (Type::Void, function.range()),
        };
        if !ctx.is_assignable(&actual_return_type, &return_type) {
            ctx.add_error(Error::unexpected_type(
                return_range,
                &return_type,
                &actual_return_type,
            ));
        }
    }

    let exit_reason = {
        let env = ctx.exit_env_without_merge();
        let exit_reason = env.exit_reason.clone();
        ctx.merge_env(env);
        exit_reason
    };

    // Catch exit-by-return
    match exit_reason {
        ExitReason::Break => {
            ctx.add_error(Error::break_not_in_loop(function.range()));
            ctx.set_exit_reason(ExitReason::Normal);
        }
        ExitReason::Normal | ExitReason::Return => {
            ctx.set_exit_reason(ExitReason::Normal);
        }
    }
}

fn analyze_struct_declaration(_ctx: &mut AnalyzerContext, _struct_: &StructDeclaration) {
    unimplemented!("Struct analysis is not implemented yet");
}

fn analyze_interface(_ctx: &mut AnalyzerContext, _interface: &InterfaceDeclaration) {
    unimplemented!("Interface analysis is not implemented yet");
}

fn analyze_impl_statement(_ctx: &mut AnalyzerContext, _impl_statement: &ImplStatement) {
    unimplemented!("Impl analysis is not implemented yet");
}

fn analyze_return_(ctx: &mut AnalyzerContext, return_: &Return) {
    if ctx.in_function() {
        ctx.set_exit_reason(ExitReason::Return);
    } else {
        ctx.add_error(Error::invalid_syntax(return_.range(), "Return statement is not inside a function"));
    }

    let return_type = match &return_.value {
        Some(value) => {
            analyze_node(ctx, value);
            ctx.get_expression_type(&value.range()).unwrap_or(Type::Unknown)
        }
        None => Type::Void,
    };
    ctx.add_return(return_.range(), return_type.clone());
}

fn analyze_break(ctx: &mut AnalyzerContext, break_: &Break) {
    if ctx.in_loop() {
        ctx.set_exit_reason(ExitReason::Break);
    } else {
        ctx.add_error(Error::break_not_in_loop(break_.range()));
    }

    ctx.add_break(break_.range());
}

fn analyze_if_expression(ctx: &mut AnalyzerContext, if_expression: &IfExpression) {
    analyze_node(ctx, &if_expression.condition);
    ctx.assert_type(&if_expression.condition.range(), &Type::Bool);

    ctx.enter_env(Env::new(false, false));
    analyze_node(ctx, &if_expression.true_branch);
    let true_branch_type_ = ctx.get_expression_type(&if_expression.true_branch.range()).unwrap_or(Type::Void);
    let true_env = ctx.exit_env_without_merge();

    ctx.enter_env(Env::new(false, false));
    analyze_node(ctx, &if_expression.false_branch);
    let false_branch_type_ = ctx.get_expression_type(&if_expression.false_branch.range()).unwrap_or(Type::Void);
    let false_env = ctx.exit_env_without_merge();

    let type_ = match (&true_branch_type_, &false_branch_type_) {
        (Type::Never, _) => &false_branch_type_,
        (_, Type::Never) => &true_branch_type_,
        _ => {
            ctx.assert_assignable(&true_branch_type_, &false_branch_type_, &if_expression.range());
            &true_branch_type_
        }
    };

    let child_env = ctx.reconcile_branched_envs(true_env, false_env);
    ctx.merge_env(child_env);

    ctx.add_expression(ExpressionInfo::temp_value(if_expression.range(), type_.clone()));
}

fn analyze_block(ctx: &mut AnalyzerContext, block: &Block) {
    ctx.enter_env(Env::new(false, false));
    analyze_nodes(ctx, &block.nodes);

    let exit_reason = {
        let env = ctx.exit_env_without_merge();
        let exit_reason = env.exit_reason.clone();
        ctx.merge_env(env);
        exit_reason
    };

    let block_type = if !matches!(exit_reason, ExitReason::Normal) {
        Type::Never
    } else {
        match block.nodes.last() {
            Some(node) => ctx.get_expression_type(&node.range()).unwrap_or(Type::Void),
            None => Type::Void,
        }
    };
    ctx.add_expression(ExpressionInfo::temp_value(block.range(), block_type.clone()));
}

fn analyze_assignment_expression(ctx: &mut AnalyzerContext, assignment_expression: &AssignmentExpression) {
    analyze_node(ctx, &assignment_expression.rhs);
    let rhs_type = ctx.get_expression_type(&assignment_expression.rhs.range()).unwrap_or(Type::Unknown);

    match assignment_expression.lhs.deref() {
        Node::Identifier(identifier) => {
            match ctx.get_symbol_by_name(&identifier.name) {
                Some(symbol) => {
                    match symbol.kind {
                        SymbolKind::Function => {
                            ctx.add_error(Error::assign_to_non_variable(
                                assignment_expression.lhs.range(),
                                "function",
                            ));
                        }
                        SymbolKind::Variable => {
                            let defined_at = symbol.defined_at.clone();
                            ctx.add_expression(ExpressionInfo::variable(assignment_expression.lhs.range(), Some(defined_at.clone())));

                            let lhs_type = ctx.get_variable_type(&defined_at);
                            ctx.assert_assignable(&rhs_type, &lhs_type, &assignment_expression.rhs.range());

                            if !ctx.is_variable_initialized(defined_at.clone()) {
                                ctx.set_variable_initialized(defined_at.clone(), assignment_expression.range());
                                ctx.set_variable_type(defined_at.clone(), rhs_type.clone());
                            }
                        }
                    }
                }
                None => {
                    ctx.add_error(Error::undefined_symbol(identifier.range(), identifier.name.clone()));
                    ctx.define_symbol(identifier.range(), identifier.name.clone(), SymbolKind::Variable);
                    ctx.set_variable_initialized(identifier.range(), assignment_expression.range());
                    ctx.set_variable_type(identifier.range(), rhs_type.clone());
                }
            }
        }
        _ => {
            ctx.add_error(Error::invalid_syntax(
                assignment_expression.range(),
                "Left-hand side of an assignment must be a variable",
            ));
        }
    }

    ctx.add_expression(ExpressionInfo::temp_value(assignment_expression.range(), rhs_type.clone()));
}

fn analyze_binary_expression(ctx: &mut AnalyzerContext, binary_expression: &BinaryExpression) {
    analyze_node(ctx, &binary_expression.lhs);
    analyze_node(ctx, &binary_expression.rhs);
    let lhs_type = ctx.get_expression_type(&binary_expression.lhs.range()).unwrap_or(Type::Unknown);
    let rhs_type = ctx.get_expression_type(&binary_expression.rhs.range()).unwrap_or(Type::Unknown);
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

    ctx.assert_assignable(&lhs_type, &expected_lhs_type, &binary_expression.lhs.range());
    ctx.assert_assignable(&rhs_type, &expected_rhs_type, &binary_expression.rhs.range());

    ctx.add_expression(ExpressionInfo::temp_value(binary_expression.range(), result_type));
}

fn analyze_unary_expression(ctx: &mut AnalyzerContext, unary_expression: &UnaryExpression) {
    analyze_node(ctx, &unary_expression.operand);
    let operand_type = ctx.get_expression_type(&unary_expression.operand.range()).unwrap_or(Type::Unknown);

    let (expected_operand_type, result_type) = match unary_expression.operator {
        PunctuationKind::Minus => (Type::Number, Type::Number),
        PunctuationKind::Exclamation => (Type::Bool, Type::Bool),
        _ => panic!("Unexpected unary operator {:?}", unary_expression.operator),
    };

    ctx.assert_assignable(&operand_type, &expected_operand_type, &unary_expression.operand.range());

    ctx.add_expression(ExpressionInfo::temp_value(unary_expression.range(), result_type));
}

fn analyze_call_expression(ctx: &mut AnalyzerContext, call_expression: &CallExpression) {
    analyze_node(ctx, &call_expression.callee);
    let function_type_ = match ctx.get_expression(&call_expression.callee.range()) {
        Some(ExpressionInfo::Function(ref symbol_ref)) => {
            match symbol_ref.defined_at {
                Some(ref defined_at) => {
                    let defined_at = defined_at.clone();
                    let function_info = ctx.get_function(&defined_at).unwrap();
                    function_info.type_.clone()
                }
                None => Type::Unknown,
            }
        }
        Some(ExpressionInfo::Variable(ref symbol_ref)) => {
            match symbol_ref.defined_at {
                Some(ref defined_at) => ctx.get_variable_type(defined_at),
                None => Type::Unknown,
            }
        }
        Some(ExpressionInfo::TempValue(ref info)) => {
            info.type_.clone()
        }
        None => panic!("Callee is not found")
    };

    ctx.assert_callable(&function_type_, &call_expression.callee.range());

    if let Type::Function(parameter_types, return_type) = function_type_ {
        if call_expression.parameters.len() != parameter_types.len() {
            ctx.add_error(Error::invalid_parameter_count(
                call_expression.range(),
                parameter_types.len(),
                call_expression.parameters.len(),
            ));
        }

        for parameter in &call_expression.parameters {
            analyze_node(ctx, parameter.value.deref());
        }
        for (parameter, expected_type) in call_expression.parameters.iter().zip(parameter_types.iter()) {
            let actual_type = ctx.get_expression_type(&parameter.value.range()).unwrap_or(Type::Unknown);

            ctx.assert_assignable(&actual_type, expected_type, &parameter.value.range());
        }

        ctx.add_expression(ExpressionInfo::temp_value(call_expression.range(), return_type.deref().clone()));
    };
}

fn analyze_member_expression(_ctx: &mut AnalyzerContext, _member_expression: &MemberExpression) {
    unimplemented!("Member analysis is not implemented yet");
}

fn analyze_identifier(ctx: &mut AnalyzerContext, identifier: &Identifier) {
    match ctx.get_symbol_by_name(&identifier.name) {
        Some(symbol) => {
            let defined_at = symbol.defined_at.clone();
            match symbol.kind {
                SymbolKind::Variable => {
                    ctx.mark_variable_as_used(defined_at.clone());
                    ctx.add_expression(ExpressionInfo::variable(identifier.range(), Some(defined_at.clone())));

                    if !ctx.is_variable_initialized(defined_at.clone()) {
                        ctx.add_error(Error::uninitialized_variable(identifier.range(), identifier.name.clone()));
                        ctx.set_variable_type(identifier.range(), Type::Unknown);
                        ctx.set_variable_initialized(identifier.range(), identifier.range());
                    }
                }
                SymbolKind::Function => {
                    ctx.add_expression(ExpressionInfo::function(identifier.range(), Some(defined_at.clone())));
                }
            }
        }
        None => {
            ctx.add_error(Error::undefined_symbol(identifier.range(), identifier.name.clone()));

            ctx.define_symbol(identifier.range(), identifier.name.clone(), SymbolKind::Variable);
            ctx.set_variable_type(identifier.range(), Type::Unknown);
            ctx.set_variable_initialized(identifier.range(), identifier.range());

            ctx.add_expression(ExpressionInfo::variable(identifier.range(), Some(identifier.range())));
        }
    };
}

fn analyze_number_literal(ctx: &mut AnalyzerContext, number_literal: &NumberLiteral) {
    ctx.add_expression(ExpressionInfo::temp_value(number_literal.range(), Type::Number));
}

fn analyze_bool_literal(ctx: &mut AnalyzerContext, bool_literal: &BoolLiteral) {
    ctx.add_expression(ExpressionInfo::temp_value(bool_literal.range(), Type::Bool));
}

fn analyze_string_literal(ctx: &mut AnalyzerContext, string_literal: &StringLiteral) {
    ctx.add_expression(ExpressionInfo::temp_value(string_literal.range(), Type::Unknown));
}

fn analyze_type_expression(ctx: &mut AnalyzerContext, type_expression: &TypeExpression) {
    let type_ = match type_expression {
        TypeExpression::Identifier(identifier) => {
            match identifier.name.as_str() {
                "number" => Type::Number,
                "bool" => Type::Bool,
                "any" => Type::Any,
                _ => {
                    unimplemented!("Type analysis for identifier is not implemented yet");
                }
            }
        }
        TypeExpression::Function(function) => {
            let mut parameter_types = vec![];
            for parameter in &function.parameters {
                analyze_type_expression(ctx, parameter);
                parameter_types.push(ctx.get_type_expression_type(&parameter.range()));
            }
            analyze_type_expression(ctx, function.return_.deref());
            let return_type = ctx.get_type_expression_type(&function.return_.range());

            Type::Function(parameter_types, Box::new(return_type))
        }
    };

    ctx.add_type_expression(type_expression.range(), type_);
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
    use crate::analyze::{analyze_program, Analysis};
    use crate::error::Error;
    use crate::parser::parse;

    mod use_undefined_variable {
        use crate::analyze::test::{test, AssertMethods};
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
        use crate::analyze::test::{test, AssertMethods};
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
        use crate::analyze::test::{test, AssertMethods};
        use crate::analyze::Type;
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
        use crate::analyze::test::{test, AssertMethods};
        use crate::analyze::Type;
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
        use crate::analyze::test::{test, AssertMethods};
        use crate::analyze::Type;
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
        use crate::analyze::test::{test, AssertMethods};
        use crate::analyze::Type;
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
        use crate::analyze::test::{test, AssertMethods};
        use crate::analyze::Type;
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
        use crate::analyze::test::{test, AssertMethods};
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
        use crate::analyze::test::{test, AssertMethods};
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
                // Error for the first inner if
                Error::conditionally_initialized_as_different_type(pos(2, 46)..pos(2, 51), Type::Bool, Type::Number),

                // Error for the second inner if
                Error::conditionally_initialized_as_different_type(pos(4, 43)..pos(4, 51), Type::Number, Type::Bool),

                // Error for outer if
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
        use crate::analyze::test::{test, AssertMethods};
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
                Error::unexpected_type(pos(4, 20)..pos(4, 25), &Type::Number, &Type::Void)
            ])
        }
    }

    mod assignment_expression {
        use crate::analyze::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn assign_to_variable() {
            test(r#"
                let x = 1;
                 x = 2
            "#).assert_errors(vec![])
        }

        #[test]
        fn assign_to_function() {
            test(r#"
                fn f(): number { return 1 }
                f = 2
            "#).assert_errors(vec![
                Error::assign_to_non_variable(pos(2, 16)..pos(2, 17), "function")
            ])
        }
    }

    mod call_expression {
        use crate::analysis::type_::Type;
        use crate::analyze::test::{test, AssertMethods};
        use crate::error::Error;
        use crate::position::pos;

        #[test]
        fn call_undefined_function() {
            test(r#"
            f(1, 2, 3)
            "#).assert_errors(vec![
                Error::undefined_symbol(pos(1, 12)..pos(1, 13), "f"),
                Error::call_non_callable_value(pos(1, 12)..pos(1, 13))
            ])
        }

        #[test]
        fn call_static_function() {
            test(r#"
            fn f(p1: number, p2: number): number { return 1 }
            f(1, 2)
            "#).assert_errors(vec![])
        }

        #[test]
        fn call_function_object() {
            test(r#"
            let f = fn (p1: number, p2: number): number { return 1 }
            f(1, 2)
            "#).assert_errors(vec![])
        }

        #[test]
        fn call_non_callable_object() {
            test(r#"
            let f = 1
            f(1, 2)
            "#).assert_errors(vec![
                Error::call_non_callable_value(pos(2, 12)..pos(2, 13))
            ])
        }

        #[test]
        fn call_function_object_immediately() {
            test(r#"
            (fn (p1: number, p2: number): number { return 1 })(1, 2)
            "#).assert_errors(vec![])
        }

        #[test]
        fn call_non_callable_value() {
            test(r#"
            (1)(1, 2)
            "#).assert_errors(vec![
                Error::call_non_callable_value(pos(1, 13)..pos(1, 14))
            ])
        }

        #[test]
        fn too_many_parameters() {
            test(r#"
            fn f(p1: number, p2: number): number { return 1 }
            f(1, 2, 3, 4, 5)
            "#).assert_errors(vec![
                Error::invalid_parameter_count(pos(2, 12)..pos(2, 28), 2, 5)
            ])
        }

        #[test]
        fn too_few_parameters() {
            test(r#"
            fn f(p1: number, p2: number): number { return 1 }
            f(1)
            "#).assert_errors(vec![
                Error::invalid_parameter_count(pos(2, 12)..pos(2, 16), 2, 1)
            ])
        }

        #[test]
        fn analyze_parameter_type() {
            test(r#"
            fn f(p1: number, p2: number): number { return 1 }
            f(1, true)
            "#).assert_errors(vec![
                Error::unexpected_type(pos(2, 17)..pos(2, 21), &Type::Number, &Type::Bool)
            ])
        }

        #[test]
        fn analyze_return_type() {
            test(r#"
            let x = 1
            fn f(): bool { return true }

            x = f()
            "#).assert_errors(vec![
                Error::unexpected_type(pos(4, 16)..pos(4, 19), &Type::Number, &Type::Bool)
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