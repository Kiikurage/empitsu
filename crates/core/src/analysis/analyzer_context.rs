use crate::analysis::env::{Env, ExitReason, Symbol, SymbolKind};
use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::return_info::ReturnInfo;
use crate::analysis::type_::Type;
use crate::analysis::Analysis;
use crate::error::Error;
use crate::position::Position;
use std::ops::Range;
use crate::analysis::function_info::FunctionInfo;
use crate::analysis::struct_info::StructInfo;

pub struct AnalyzerContext {
    envs: Vec<Env>,
    errors: Vec<Error>,
}

impl AnalyzerContext {
    pub fn new() -> AnalyzerContext {
        AnalyzerContext {
            envs: vec![Env::new(false, false)],
            errors: vec![],
        }
    }

    pub fn into_analysis(mut self) -> Analysis {
        while self.envs.len() > 1 {
            self.exit_env();
        }
        let mut env = self.envs.pop().unwrap();
        env.encapsulate();

        Analysis::new(
            env.variables,
            env.breaks,
            env.expressions,
            env.type_expressions,
            env.functions,
            env.returns,
            env.structs,
            self.errors,
        )
    }

    pub fn is_exited(&self) -> bool {
        !matches!(self.exit_reason(), ExitReason::Normal)
    }

    /// Get the position of the env that can be exited by break expression.
    pub fn in_loop(&self) -> bool {
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
    pub fn in_function(&self) -> bool {
        for env in self.envs.iter().rev() {
            if env.is_function {
                return true;
            }
        }
        false
    }

    pub fn set_exit_reason(&mut self, exit_reason: ExitReason) {
        self.current_env_mut().exit_reason = exit_reason;
    }

    pub fn exit_reason(&self) -> &ExitReason {
        &self.current_env().exit_reason
    }

    pub fn add_break(&mut self, range: Range<Position>) {
        self.current_env_mut().add_break(range);
    }

    pub fn add_return(&mut self, range: Range<Position>, return_value_type: Type) {
        self.current_env_mut().add_return(range, return_value_type);
    }

    pub fn add_struct(&mut self, info: StructInfo) {
        self.current_env_mut().add_struct(info);
    }

    pub fn get_struct(&self, range: &Range<Position>) -> Option<&StructInfo> {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.get_struct(range) {
                return Some(info);
            }
        }
        None
    }

    pub fn returns(&self) -> &Vec<Range<Position>> {
        &self.current_env().pending_returns
    }

    pub fn get_return(&self, range: &Range<Position>) -> Option<&ReturnInfo> {
        self.current_env().get_return(range)
    }

    /// Enter a new env.
    pub fn enter_env(&mut self, env: Env) {
        self.envs.push(env);
    }

    /// Exit current env and merge the result to the parent env.
    pub fn exit_env(&mut self) {
        let env = self.exit_env_without_merge();
        self.merge_env(env);
    }

    /// Exit current env, but do not merge the result to the parent env.
    /// This is useful when you want to emulate branched env like if-statement.
    pub fn exit_env_without_merge(&mut self) -> Env {
        self.envs.pop().unwrap()
    }

    /// Merge the given env to the parent env.
    pub fn merge_env(&mut self, env: Env) {
        self.current_env_mut().merge(env)
    }

    /// Reconcile two branched envs.
    pub fn reconcile_branched_envs(&mut self, mut env1: Env, mut env2: Env) -> Env {
        let mut result = Env::new(false, false);

        // Data for local symbols are not needed to be reconciled
        env1.encapsulate();
        env2.encapsulate();

        // Variables defined in the outer scope must be initialized to the same type in both branches
        for (defined_at, env1_initialized_at) in env1.initialized_variables() {
            match env2.is_variable_initialized(defined_at) {
                Some(env2_initialized_at) => {
                    let env1_type = env1.get_variable_type(defined_at).unwrap_or(&Type::Unknown).clone();
                    let env2_type = env2.get_variable_type(defined_at).unwrap_or(&Type::Unknown).clone();

                    let type_ = if self.is_assignable(&env2_type, &env1_type) {
                        env1_type
                    } else {
                        self.errors.push(Error::conditionally_initialized_as_different_type(env2_initialized_at.clone(), env1_type, env2_type));
                        Type::Unknown
                    };

                    result.set_variable_initialized(defined_at.clone(), env1_initialized_at.clone());
                    result.set_variable_type(defined_at.clone(), type_);
                }
                None => {
                    self.errors.push(Error::conditionally_initialized_variable(env1_initialized_at.clone()));

                    let type_ = env1.get_variable_type(defined_at).unwrap_or(&Type::Unknown).clone();
                    result.set_variable_initialized(defined_at.clone(), env1_initialized_at.clone());
                    result.set_variable_type(defined_at.clone(), type_);
                }
            }
        }
        for (defined_at, env2_initialized_at) in env2.initialized_variables() {
            if env1.is_variable_initialized(defined_at).is_none() {
                self.errors.push(Error::conditionally_initialized_variable(env2_initialized_at.clone()));

                let type_ = env2.get_variable_type(defined_at).unwrap_or(&Type::Unknown).clone();
                result.set_variable_initialized(defined_at.clone(), env2_initialized_at.clone());
                result.set_variable_type(defined_at.clone(), type_);
            }
        }

        if env1.exit_reason != env2.exit_reason {
            env1.exit_reason = ExitReason::Normal;
            env2.exit_reason = ExitReason::Normal;
        }

        result.merge(env1);
        result.merge(env2);
        result
    }

    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    // Symbol

    pub fn define_symbol(&mut self, defined_at: Range<Position>, name: impl Into<String>, kind: SymbolKind) {
        self.current_env_mut().define_symbol(defined_at, name, kind);
    }

    pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
        for env in self.envs.iter().rev() {
            if let Some(symbol) = env.get_symbol_by_name(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn set_variable_type(&mut self, defined_at: Range<Position>, type_: Type) {
        self.current_env_mut().set_variable_type(defined_at, type_);
    }

    pub fn get_variable_type(&self, defined_at: &Range<Position>) -> Type {
        for env in self.envs.iter().rev() {
            if let Some(type_) = env.get_variable_type(defined_at) {
                return type_.clone();
            }
        }
        Type::Unknown
    }

    pub fn set_variable_initialized(&mut self, defined_at: Range<Position>, initialized_at: Range<Position>) {
        self.current_env_mut().set_variable_initialized(defined_at, initialized_at);
    }

    pub fn is_variable_initialized(&self, defined_at: Range<Position>) -> bool {
        for env in self.envs.iter().rev() {
            if env.is_variable_initialized(&defined_at).is_some() {
                return true;
            }

            if env.is_symbol_defined(&defined_at) {
                return false;
            }
        }
        false
    }

    pub fn mark_variable_as_used(&mut self, defined_at: Range<Position>) {
        self.current_env_mut().mark_variable_as_used(defined_at);
    }

    // Static information

    pub fn add_expression(&mut self, info: ExpressionInfo) {
        self.current_env_mut().add_expression(info);
    }

    pub fn get_expression(&self, range: &Range<Position>) -> Option<&ExpressionInfo> {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.get_expression(range) {
                return Some(info);
            }
        }
        None
    }

    pub fn get_expression_type(&self, range: &Range<Position>) -> Option<Type> {
        for env in self.envs.iter().rev() {
            match env.get_expression(range) {
                Some(ExpressionInfo::TempValue(temp_value)) => return Some(temp_value.type_.clone()),
                Some(ExpressionInfo::Variable(symbol_ref)) => {
                    if let Some(ref defined_at) = symbol_ref.defined_at {
                        return Some(self.get_variable_type(defined_at));
                    }
                    return None;
                }
                Some(ExpressionInfo::Function(symbol_ref)) => {
                    if let Some(ref defined_at) = symbol_ref.defined_at {
                        if let Some(function) = env.get_function(defined_at) {
                            return Some(function.type_.clone());
                        }
                    }
                    return None;
                }
                _ => {}
            }
        }
        None
    }

    pub fn add_function(&mut self, range: Range<Position>, name: impl Into<String>, type_: Type) {
        self.current_env_mut().add_function(range, name, type_);
    }

    pub fn get_function(&mut self, range: &Range<Position>) -> Option<&FunctionInfo> {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.get_function(range) {
                return Some(info);
            }
        }

        None
    }

    pub fn add_type_expression(&mut self, range: Range<Position>, type_: Type) {
        self.current_env_mut().add_type_expression(range, type_);
    }

    pub fn get_type_expression_type(&self, range: &Range<Position>) -> Type {
        for env in self.envs.iter().rev() {
            if let Some(info) = env.get_type_expression(range) {
                return info.type_.clone();
            }
        }
        Type::Unknown
    }

    pub fn is_assignable(&self, actual: &Type, expected: &Type) -> bool {
        match (actual, expected) {
            // Any can assign and accept anything
            (Type::Any, _) => true,
            (_, Type::Any) => true,

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

    fn current_env(&self) -> &Env {
        self.envs.last().unwrap()
    }

    /// Get the current env as mutable
    fn current_env_mut(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap()
    }

    pub fn assert_type(&mut self, node_range: &Range<Position>, expected_type: &Type) {
        let actual_type = self.get_expression_type(node_range).unwrap_or(Type::Unknown).clone();
        self.assert_assignable(&actual_type, expected_type, node_range);
    }

    pub fn assert_assignable(&mut self, actual: &Type, expected: &Type, range: &Range<Position>) {
        if !self.is_assignable(actual, expected) {
            self.add_error(Error::unexpected_type(range.clone(), expected, actual));
        }
    }

    pub fn assert_callable(&mut self, actual: &Type, range: &Range<Position>) {
        if !matches!(actual, Type::Function(..) | Type::Any) {
            self.add_error(Error::call_non_callable_value(range.clone()));
        }
    }
}
