use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::ops::{ControlFlow, Deref};
use std::rc::Rc;

use crate::node::{FunctionParameterDeclaration, Node, StructPropertyDeclaration, StructPropertyInitializer, TypeExpression};
use crate::parser::parse;
use crate::punctuator_kind::PunctuatorKind;
use crate::type_::{FunctionParameterDefinition, StructPropertyDefinition, StructType, Type};
use crate::value::{FunctionValue, NativeFunction, NativeFunctionValue, StructValue, Value};

#[derive(Clone, PartialEq, Debug)]
struct Variable {
    type_: Type,
    value: Value,
}

#[derive(Default, Debug, PartialEq)]
pub struct Environment {
    variables: HashMap<String, Variable>,
    types: HashMap<String, Type>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
            types: HashMap::new(),
            parent: None,
        }
    }

    fn declare_variable(&mut self, name: &str, type_: &Type, value: &Value) {
        self.variables.insert(name.to_string(), Variable { type_: type_.clone(), value: value.clone() });
    }

    fn get_variable(&self, name: &str) -> ControlFlow<BreakResult, Variable> {
        match self.variables.get(name) {
            Some(variable) => ControlFlow::Continue(variable.clone()),
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow().get_variable(name),
                    None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined variable: {}", name)))),
                }
            }
        }
    }

    fn set_variable(&mut self, name: &str, value: &Value) -> ControlFlow<BreakResult, ()> {
        match self.variables.get_mut(name) {
            Some(variable) => {
                variable.value = value.clone();
                ControlFlow::Continue(())
            }
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow_mut().set_variable(name, value),
                    None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined variable: {}", name)))),
                }
            }
        }
    }

    fn declare_type(&mut self, name: &str, type_: &Type) {
        self.types.insert(name.to_string(), type_.clone());
    }

    fn eval_type_expression(&self, type_: &TypeExpression) -> ControlFlow<BreakResult, Type> {
        match type_ {
            TypeExpression::Identifier(name) => {
                if let Some(type_) = self.types.get(name) {
                    ControlFlow::Continue(type_.clone())
                } else {
                    match &self.parent {
                        Some(parent) => parent.borrow().eval_type_expression(type_),
                        None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined type: {:?}", type_)))),
                    }
                }
            }
            TypeExpression::Optional(type_) => {
                let type_ = self.eval_type_expression(type_)?;

                ControlFlow::Continue(Type::Union(vec![Type::Null, type_]))
            }
            TypeExpression::Union(type_expressions) => {
                let mut types = vec![];
                for type_expression in type_expressions {
                    types.push(self.eval_type_expression(type_expression)?);
                }

                ControlFlow::Continue(Type::Union(types))
            }
        }
    }
}

pub enum BreakResult {
    Return(Value),
    Break(Value),
    Error(Value),
}

pub struct VM {
    environments: Vec<Rc<RefCell<Environment>>>,
    pub structs: HashMap<usize, RefCell<StructValue>>,
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = VM {
            environments: vec![Rc::new(RefCell::new(Environment::new()))],
            structs: HashMap::new(),
        };

        vm.install_native_function("number", &[TypeExpression::Identifier("unchecked".to_string())], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            match value {
                Value::Number(value) => ControlFlow::Continue(Value::Number(*value)),
                Value::Bool(value) => ControlFlow::Continue(Value::Number(if *value { 1.0 } else { 0.0 })),
                Value::String(value) => {
                    match value.parse::<f64>() {
                        Ok(value) => ControlFlow::Continue(Value::Number(if value.is_nan() { 0.0 } else { value })),
                        Err(_) => ControlFlow::Continue(Value::Number(0.0)),
                    }
                }
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Number", value.get_type()))))
            }
        });
        vm.install_native_function("bool", &[TypeExpression::Identifier("unchecked".to_string())], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            match value {
                Value::Number(value) => ControlFlow::Continue(Value::Bool(*value != 0.0)),
                Value::Bool(value) => ControlFlow::Continue(Value::Bool(*value)),
                Value::String(value) => ControlFlow::Continue(Value::Bool(value != "false")),
                Value::Function { .. } => ControlFlow::Continue(Value::Bool(true)),
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Bool", value.get_type())))),
            }
        });
        vm.install_native_function("string", &[TypeExpression::Identifier("unchecked".to_string())], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            match value {
                Value::Number(value) => ControlFlow::Continue(Value::String(value.to_string())),
                Value::Bool(value) => ControlFlow::Continue(Value::String(value.to_string())),
                Value::String(value) => ControlFlow::Continue(Value::String(value.clone())),
                Value::Function { .. } => {
                    ControlFlow::Continue(Value::String(value.clone().into_string().into_control_flow()?))
                }
                Value::Ref(address) => ControlFlow::Continue(Value::String(address.to_string())),
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Bool", value.get_type())))),
            }
        });
        vm.install_native_function("print", &[TypeExpression::Identifier("unchecked".to_string())], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            println!("{}", value.clone().into_string().into_control_flow()?);
            ControlFlow::Continue(Value::Number(0.0))
        });
        vm.install_native_function("debug", &[TypeExpression::Identifier("unchecked".to_string())], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            match value {
                Value::Number(value) => println!("{}", value),
                Value::Bool(value) => println!("{}", value),
                Value::String(value) => println!("{}", value),
                Value::Function { .. } => println!("{}", value.clone().into_string().into_control_flow()?),
                Value::NativeFunction { .. } => println!("{}", value.clone().into_string().into_control_flow()?),
                Value::Ref(value) => println!("ref {}", value),
            }
            ControlFlow::Continue(Value::Number(0.0))
        });

        vm.declare_type("number", &Type::Number);
        vm.declare_type("string", &Type::String);
        vm.declare_type("bool", &Type::Bool);
        vm.declare_type("unchecked", &Type::Unchecked);

        vm
    }
}

impl VM {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn eval(&mut self, input: &str) -> Value {
        match parse(input) {
            Ok(ast) => match self.eval_node(&ast) {
                ControlFlow::Continue(value) => value,
                ControlFlow::Break(BreakResult::Return(value)) => value,
                ControlFlow::Break(BreakResult::Break(value)) => value,
                ControlFlow::Break(BreakResult::Error(value)) => value,
            }
            Err(message) => Value::String(message),
        }
    }

    pub fn gc(&mut self) {
        let mut retained_objects = HashSet::new();

        for env in self.environments.iter() {
            for variable in env.borrow().variables.values() {
                if let Value::Ref(address) = variable.value {
                    retained_objects.insert(address);
                }
            }
        }

        let mut queue = retained_objects.iter().copied().collect::<VecDeque<_>>();
        while let Some(address) = queue.pop_front() {
            let struct_value = match self.structs.get(&address) {
                Some(struct_value) => struct_value.borrow(),
                None => continue,
            };

            for value in struct_value.properties.values() {
                if let Value::Ref(address) = value {
                    if retained_objects.insert(*address) {
                        queue.push_back(*address);
                    }
                }
            }
        }

        self.structs.retain(|address, _| retained_objects.contains(address));
    }

    fn eval_node(&mut self, node: &Node) -> ControlFlow<BreakResult, Value> {
        match node {
            Node::Program(nodes) => {
                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = match self.eval_node(node) {
                        ControlFlow::Continue(value) => value,
                        ControlFlow::Break(BreakResult::Return(_)) => {
                            ret = Value::String("return is used outside of function".to_string());
                            break;
                        }
                        ControlFlow::Break(BreakResult::Break(_)) => {
                            ret = Value::String("break is used outside of loop".to_string());
                            break;
                        }
                        ControlFlow::Break(BreakResult::Error(value)) => {
                            ret = value;
                            break;
                        }
                    };
                }
                ControlFlow::Continue(ret)
            }

            // Statement
            Node::EmptyStatement => ControlFlow::Continue(Value::Number(0.0)),
            Node::IfStatement(condition, true_branch, false_branch) => {
                let condition = self.eval_node(condition)?.as_bool().into_control_flow()?;
                if condition {
                    self.eval_node(true_branch)
                } else {
                    match false_branch {
                        Some(false_branch) => self.eval_node(false_branch),
                        None => ControlFlow::Continue(Value::Number(0.0)),
                    }
                }
            }
            Node::VariableDeclaration(name, variable_type, value) => {
                let value = match value {
                    Some(value) => self.eval_node(value)?,
                    None => Value::Number(0.0),
                };
                let value_type = self.get_type(&value)?;
                let variable_type = match variable_type {
                    Some(type_) => self.eval_type_expression(type_)?,
                    None => value_type.clone(),
                };
                if !value_type.is_assignable(&variable_type) {
                    return ControlFlow::Break(BreakResult::Error(Value::String(
                        format!("TypeError: Type {:?} is not assignable into {:?}", value_type, variable_type)
                    )));
                }
                self.declare_variable(name, &value_type, &value);
                ControlFlow::Continue(Value::Number(0.0))
            }
            Node::ForStatement(variable, iterator, body) => {
                self.enter_new_environment();

                let (start, end) = match iterator.deref() {
                    Node::RangeIterator(start, end) => {
                        (self.eval_node(start)?, self.eval_node(end)?)
                    }
                    _ => return ControlFlow::Break(BreakResult::Error(Value::String("Unsupported iterator type".to_string())))
                };
                let mut i = start.as_number().into_control_flow()?;
                let end = end.as_number().into_control_flow()?;
                self.declare_variable(variable, &Type::Number, &start); // TODO
                while i < end {
                    self.set_variable(variable, &Value::Number(i))?;
                    match self.eval_node(body) {
                        ControlFlow::Break(BreakResult::Break(_)) => { break }
                        other => { other?; }
                    }
                    i += 1f64;
                }

                self.exit_environment();
                ControlFlow::Continue(Value::Number(0.0))
            }
            Node::FunctionDeclaration(name, parameters, body) => {
                let mut parameter_definitions = vec![];
                for FunctionParameterDeclaration { name, type_ } in parameters {
                    parameter_definitions.push(FunctionParameterDefinition {
                        name: name.clone(),
                        type_: type_.clone(),
                    });
                }

                let function = Value::Function(FunctionValue {
                    name: name.clone(),
                    parameters: parameter_definitions,
                    body: body.clone(),
                    closure: self.environments.last().unwrap().clone(),
                });
                self.declare_variable(name, &function.get_type(), &function);

                ControlFlow::Continue(function)
            }
            Node::StructDeclaration(name, properties) => {
                let mut property_definitions = vec![];
                for StructPropertyDeclaration { name, type_ } in properties {
                    property_definitions.push(StructPropertyDefinition {
                        name: name.clone(),
                        type_: type_.clone(),
                    });
                }

                self.declare_type(name, &Type::Struct(StructType {
                    name: name.clone(),
                    properties: property_definitions,
                }));
                ControlFlow::Continue(Value::Number(0.0))
            }

            // Expression
            Node::FunctionExpression(name, parameters, body) => {
                let mut parameter_definitions = vec![];
                for FunctionParameterDeclaration { name, type_ } in parameters {
                    parameter_definitions.push(FunctionParameterDefinition {
                        name: name.clone(),
                        type_: type_.clone(),
                    });
                }

                let function = Value::Function(FunctionValue {
                    name: name.clone().unwrap_or("(anonymous)".to_string()),
                    parameters: parameter_definitions,
                    body: body.clone(),
                    closure: self.environments.last().unwrap().clone(),
                });
                ControlFlow::Continue(function)
            }
            Node::IfExpression(condition, true_branch, false_branch) => {
                let condition = match self.eval_node(condition)?.as_bool() {
                    Ok(condition) => condition,
                    Err(message) => return ControlFlow::Break(BreakResult::Error(Value::String(message))),
                };

                if condition { self.eval_node(true_branch) } else { self.eval_node(false_branch) }
            }
            Node::BlockExpression(nodes) => {
                self.enter_new_environment();

                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = self.eval_node(node)?;
                }

                self.exit_environment();
                ControlFlow::Continue(ret)
            }
            Node::AssignmentExpression(lhs, value) => {
                let value = self.eval_node(value)?;
                match lhs.as_ref() {
                    Node::Identifier(name) => {
                        let variable_type = self.get_variable(name)?.type_;
                        let value_type = self.get_type(&value)?;

                        if !value_type.is_assignable(&variable_type) {
                            return ControlFlow::Break(BreakResult::Error(Value::String(
                                format!("TypeError: Type {:?} is not assignable into type {:?}", value_type, variable_type)
                            )));
                        }

                        self.set_variable(name, &value)?
                    }
                    Node::MemberExpression(object, property) => {
                        let property = match property.deref() {
                            Node::Identifier(name) => name,
                            _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected identifier".to_string()))),
                        };

                        let mut struct_value = match self.eval_node(object)? {
                            Value::Ref(address) => {
                                match self.structs.get(&address) {
                                    Some(object) => object.borrow_mut(),
                                    None => return ControlFlow::Break(BreakResult::Error(Value::String("Undefined object".to_string()))),
                                }
                            }
                            _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected reference".to_string()))),
                        };
                        let struct_type = &struct_value.type_;

                        let left_type_expression = match struct_type.get_property_type(property) {
                            Some(type_) => type_,
                            None => return ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined property: {}", property)))),
                        };
                        let left_type = self.eval_type_expression(left_type_expression)?;
                        let right_type = self.get_type(&value)?;

                        if !right_type.is_assignable(&left_type) {
                            return ControlFlow::Break(
                                BreakResult::Error(
                                    Value::String(
                                        format!("TypeError: Type {:?} is not assignable into type {:?}", right_type, left_type)
                                    )
                                )
                            );
                        }

                        struct_value.properties.insert(property.clone(), value.clone());
                    }
                    _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected identifier".to_string()))),
                }
                ControlFlow::Continue(value)
            }
            Node::BinaryExpression(left, operator, right) => {
                let left = self.eval_node(left)?;
                let right = self.eval_node(right)?;

                match operator {
                    PunctuatorKind::Plus => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? + right.as_number().into_control_flow()?)),
                    PunctuatorKind::Minus => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? - right.as_number().into_control_flow()?)),
                    PunctuatorKind::Multiply => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? * right.as_number().into_control_flow()?)),
                    PunctuatorKind::Divide => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? / right.as_number().into_control_flow()?)),
                    PunctuatorKind::LogicalOr => ControlFlow::Continue(Value::Bool(left.as_bool().into_control_flow()? || right.as_bool().into_control_flow()?)),
                    PunctuatorKind::LogicalAnd => ControlFlow::Continue(Value::Bool(left.as_bool().into_control_flow()? && right.as_bool().into_control_flow()?)),
                    PunctuatorKind::Equal => ControlFlow::Continue(Value::Bool(left == right)),  // TODO: 演算子オーバーロード
                    PunctuatorKind::NotEqual => ControlFlow::Continue(Value::Bool(left != right)),  // TODO: 演算子オーバーロード
                    PunctuatorKind::LessThan => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? < right.as_number().into_control_flow()?)),
                    PunctuatorKind::LessThanOrEqual => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? <= right.as_number().into_control_flow()?)),
                    PunctuatorKind::GreaterThan => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? > right.as_number().into_control_flow()?)),
                    PunctuatorKind::GreaterThanOrEqual => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? >= right.as_number().into_control_flow()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval_node(operand)?;

                match operator {
                    PunctuatorKind::Plus => ControlFlow::Continue(Value::Number(operand.as_number().into_control_flow()?)),
                    PunctuatorKind::Minus => ControlFlow::Continue(Value::Number(-operand.as_number().into_control_flow()?)),
                    PunctuatorKind::LogicalNot => ControlFlow::Continue(Value::Bool(!operand.as_bool().into_control_flow()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::CallExpression(function, arguments) => {
                let function = self.eval_node(function)?;
                match function {
                    Value::Function(function) => {
                        let mut evaluated_arguments = vec![];
                        for argument in arguments {
                            evaluated_arguments.push(self.eval_node(argument)?);
                        }

                        let mut environment = Environment::new();
                        environment.parent = Some(function.closure);
                        self.environments.push(Rc::new(RefCell::new(environment)));

                        for (argument, FunctionParameterDefinition { name, type_ }) in evaluated_arguments.iter().zip(function.parameters.iter()) {
                            let parameter_type = self.eval_type_expression(type_)?;
                            let value_type = self.get_type(argument)?;
                            if value_type != parameter_type {
                                return ControlFlow::Break(BreakResult::Error(Value::String(
                                    format!("TypeError: Expected type {:?}, actual type {:?}", parameter_type, value_type)
                                )));
                            }
                            self.declare_variable(name, &parameter_type, argument);
                        }
                        let ret = match self.eval_node(function.body.deref()) {
                            ControlFlow::Continue(value) => value,
                            ControlFlow::Break(BreakResult::Return(value)) => value,
                            others => return others,
                        };

                        self.environments.pop();
                        ControlFlow::Continue(ret)
                    }
                    Value::NativeFunction(function) => {
                        let mut evaluated_arguments = vec![];
                        for argument in arguments {
                            evaluated_arguments.push(self.eval_node(argument)?);
                        }

                        let mut environment = Environment::new();
                        environment.parent = self.environments.first().map(Rc::clone);
                        self.environments.push(Rc::new(RefCell::new(environment)));

                        let ret = (function.body)(&evaluated_arguments, self);

                        self.environments.pop();
                        ret
                    }
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("{:?} is not a function", function)))),
                }
            }
            Node::Number(value) => ControlFlow::Continue(Value::Number(*value)),
            Node::Bool(value) => ControlFlow::Continue(Value::Bool(*value)),
            Node::String(value) => ControlFlow::Continue(Value::String(value.clone())),
            Node::Identifier(name) => {
                ControlFlow::Continue(self.get_variable(name)?.value)
            }
            Node::ReturnExpression(value) => {
                ControlFlow::Break(BreakResult::Return(
                    match value {
                        Some(value) => self.eval_node(value)?,
                        None => Value::Number(0.0),
                    }
                ))
            }
            Node::BreakExpression => ControlFlow::Break(BreakResult::Break(Value::Number(0.0))),
            Node::MemberExpression(object, property) => {
                let property = match property.deref() {
                    Node::Identifier(name) => name,
                    _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected identifier".to_string()))),
                };

                let struct_value = match self.eval_node(object)? {
                    Value::Ref(address) => {
                        match self.structs.get(&address) {
                            Some(object) => object.borrow_mut(),
                            None => return ControlFlow::Break(BreakResult::Error(Value::String("Undefined object".to_string()))),
                        }
                    }
                    _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected reference".to_string()))),
                };

                match struct_value.properties.get(property) {
                    Some(value) => ControlFlow::Continue(value.clone()),
                    None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined property: {}", property))))
                }
            }
            Node::Struct(type_, property_initializers) => {
                let struct_type = match self.eval_type_expression(type_)? {
                    Type::Struct(struct_type) => struct_type,
                    _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected struct type".to_string()))),
                };

                let mut properties = HashMap::new();
                for StructPropertyInitializer { name, value } in property_initializers.iter() {
                    let property_type_expression = match struct_type.get_property_type(name) {
                        Some(type_) => type_,
                        None => return ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined property: {}", name)))),
                    };
                    let property_type = self.eval_type_expression(property_type_expression)?;

                    let value = self.eval_node(value)?;
                    let value_type = self.get_type(&value)?;

                    if !value_type.is_assignable(&property_type) {
                        return ControlFlow::Break(
                            BreakResult::Error(
                                Value::String(
                                    format!("TypeError: Type {:?} is not assignable into type {:?}", value_type, property_type)
                                )
                            )
                        );
                    }

                    properties.insert(name.clone(), value);
                }

                let address = self.allocate_object();
                self.structs.insert(address, RefCell::new(StructValue {
                    type_: struct_type,
                    properties,
                }));

                ControlFlow::Continue(Value::Ref(address))
            }

            // tmp
            Node::RangeIterator(_start, _end) => {
                ControlFlow::Break(BreakResult::Break(Value::String("Unsupported".to_string())))
            }
        }
    }

    fn enter_new_environment(&mut self) {
        let mut environment = Environment::new();
        environment.parent = self.environments.last().map(Rc::clone);
        self.environments.push(Rc::new(RefCell::new(environment)));
    }

    fn exit_environment(&mut self) {
        self.environments.pop();
    }

    fn declare_variable(&mut self, name: &str, type_: &Type, value: &Value) {
        match self.environments.last() {
            Some(environment) => environment.borrow_mut().declare_variable(name, type_, value),
            None => panic!("No environment"),
        }
    }

    fn get_variable(&self, name: &str) -> ControlFlow<BreakResult, Variable> {
        match self.environments.last() {
            Some(environment) => environment.borrow().get_variable(name),
            None => panic!("No environment"),
        }
    }

    fn set_variable(&mut self, name: &str, value: &Value) -> ControlFlow<BreakResult, ()> {
        match self.environments.last() {
            Some(environment) => environment.borrow_mut().set_variable(name, value),
            None => panic!("No environment"),
        }
    }

    fn install_native_function(
        &mut self,
        name: &str,
        parameter_types: &[TypeExpression],
        body: NativeFunction,
    ) {
        let mut global = self.environments.first().unwrap().borrow_mut();

        let mut parameter_definitions = vec![];
        for (i, type_) in parameter_types.iter().enumerate() {
            parameter_definitions.push(FunctionParameterDefinition {
                name: format!("v{}", i),
                type_: type_.clone(),
            });
        }

        let native_function = Value::NativeFunction(NativeFunctionValue {
            name: name.to_string(),
            parameters: parameter_definitions,
            body,
        });
        global.variables.insert(name.to_string(), Variable { type_: native_function.get_type(), value: native_function });
    }

    fn allocate_object(&self) -> usize {
        for i in 0.. {
            if !self.structs.contains_key(&i) {
                return i;
            }
        }
        panic!("Failed to allocate object");
    }

    fn declare_type(&mut self, name: &str, type_: &Type) {
        match self.environments.last() {
            Some(environment) => environment.borrow_mut().declare_type(name, type_),
            None => panic!("No environment"),
        }
    }

    fn eval_type_expression(&self, type_: &TypeExpression) -> ControlFlow<BreakResult, Type> {
        match self.environments.last() {
            Some(environment) => environment.borrow().eval_type_expression(type_),
            None => panic!("No environment"),
        }
    }

    fn get_type(&self, value: &Value) -> ControlFlow<BreakResult, Type> {
        match value {
            Value::Ref(address) => {
                match self.structs.get(address) {
                    Some(struct_value) => ControlFlow::Continue(Type::Struct(struct_value.borrow().type_.clone())),
                    None => ControlFlow::Break(BreakResult::Error(Value::String("Undefined object".to_string()))),
                }
            }
            _ => ControlFlow::Continue(value.get_type()),
        }
    }
}

trait IntoControlFlow<T> {
    fn into_control_flow(self) -> ControlFlow<BreakResult, T>;
}

impl<T> IntoControlFlow<T> for Result<T, String> {
    fn into_control_flow(self) -> ControlFlow<BreakResult, T> {
        match self {
            Ok(value) => ControlFlow::Continue(value),
            Err(message) => ControlFlow::Break(BreakResult::Error(Value::String(message))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::{Value, VM};

    #[test]
    fn reserved_words_in_variable_declaration() {
        assert_eq!(VM::new().eval("let if"), Value::String("SyntaxError: \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let let"), Value::String("SyntaxError: \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let for"), Value::String("SyntaxError: \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let function"), Value::String("SyntaxError: \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let true"), Value::String("Expected identifier".to_string()));
        assert_eq!(VM::new().eval("let false"), Value::String("Expected identifier".to_string()));
        assert_eq!(VM::new().eval("let else"), Value::String("SyntaxError: \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let return"), Value::String("SyntaxError: \"return\" is a reserved word".to_string()));
    }

    #[test]
    fn reserved_words_in_function_name() {
        assert_eq!(VM::new().eval("function if() {}"), Value::String("SyntaxError: \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function let() {}"), Value::String("SyntaxError: \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function for() {}"), Value::String("SyntaxError: \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function function() {}"), Value::String("SyntaxError: \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function true() {}"), Value::String("Expected '('".to_string()));
        assert_eq!(VM::new().eval("function false() {}"), Value::String("Expected '('".to_string()));
        assert_eq!(VM::new().eval("function else() {}"), Value::String("SyntaxError: \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function return() {}"), Value::String("SyntaxError: \"return\" is a reserved word".to_string()));
    }

    #[test]
    fn reserved_words_in_function_parameter() {
        assert_eq!(VM::new().eval("function f(if) {}"), Value::String("SyntaxError: \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(let) {}"), Value::String("SyntaxError: \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(for) {}"), Value::String("SyntaxError: \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(function) {}"), Value::String("SyntaxError: \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(true) {}"), Value::String("Expected ')'".to_string()));
        assert_eq!(VM::new().eval("function f(false) {}"), Value::String("Expected ')'".to_string()));
        assert_eq!(VM::new().eval("function f(else) {}"), Value::String("SyntaxError: \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(return) {}"), Value::String("SyntaxError: \"return\" is a reserved word".to_string()));
    }

    #[test]
    fn test_eval() {
        assert_eq!(VM::new().eval("1+2"), Value::Number(3.0));
        assert_eq!(VM::new().eval("1+2*\n3"), Value::Number(7.0));
        assert_eq!(VM::new().eval("(1+2)*3"), Value::Number(9.0));
        assert_eq!(VM::new().eval("debug(1)"), Value::Number(0.0));
        assert_eq!(VM::new().eval("number(1)+number(true)"), Value::Number(2.0));
        assert_eq!(VM::new().eval("{1+2}"), Value::Number(3.0));
        assert_eq!(VM::new().eval("if (true) 2 else 3"), Value::Number(2.0));
        assert_eq!(VM::new().eval("if (true) 2 else 3"), Value::Number(2.0));
        assert_eq!(VM::new().eval("if (true) { debug(123); 2 } else { 3 }"), Value::Number(2.0));
        assert_eq!(VM::new().eval("if (true) 2 else 3*3"), Value::Number(2.0));
        assert_eq!(VM::new().eval("1 + if (false) 2 else 3 * 3"), Value::Number(10.0));
        assert_eq!(VM::new().eval("1 + if (false) 2 else if (false) 3 else 4"), Value::Number(5.0));
        assert_eq!(VM::new().eval("let x=false; if (x) 1 else 2"), Value::Number(2.0));
        assert_eq!(VM::new().eval("let x=true; if (x) 1 else 2"), Value::Number(1.0));
        assert_eq!(VM::new().eval("let x=true; let y=if (x) 2 else 3; let z=y*y; z"), Value::Number(4.0));
        assert_eq!(VM::new().eval("let x=1; x=2; x"), Value::Number(2.0));
        assert_eq!(VM::new().eval("let x=5; x=x*x; x"), Value::Number(25.0));
        assert_eq!(VM::new().eval("true && true"), Value::Bool(true));
        assert_eq!(VM::new().eval("true && false"), Value::Bool(false));
        assert_eq!(VM::new().eval("false && true"), Value::Bool(false));
        assert_eq!(VM::new().eval("false && false"), Value::Bool(false));
        assert_eq!(VM::new().eval("true || true"), Value::Bool(true));
        assert_eq!(VM::new().eval("true || false"), Value::Bool(true));
        assert_eq!(VM::new().eval("false || true"), Value::Bool(true));
        assert_eq!(VM::new().eval("false || false"), Value::Bool(false));
        assert_eq!(VM::new().eval("false + 1"), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
        assert_eq!(VM::new().eval("\"hello\" + 1"), Value::String("TypeError: Expected type Number, actual type String".to_string()));

        assert_eq!(VM::new().eval("
        let x = 0
        for (i in 0 to 10) {
            debug(i)
            x = x + i
        }
        x
        "), Value::Number(45.0));
    }

    #[test]
    fn if_statement() {
        assert_eq!(VM::new().eval("
            if (0) {
                print(\"true\")
            } else {
                print(\"false\")
            }
        "), Value::String("TypeError: Expected type Bool, actual type Number".to_string()));
    }

    #[test]
    fn declare_function() {
        assert_eq!(VM::new().eval("
            function test (x: number) {
                print(string(x*2))
            }

            test(1)
            test(10)
            test(100)
        "), Value::Number(0.0));
    }

    #[test]
    fn single_line_comment() {
        assert_eq!(VM::new().eval("
            let x = 1  // single line comment
            x
        "), Value::Number(1.0));
    }

    #[test]
    fn multi_line_comment() {
        assert_eq!(VM::new().eval("
            let x = 1
            /* THIS IS MULTI LINE COMMENTS

            x = 100

            */
            x
        "), Value::Number(1.0));
    }

    #[test]
    fn closure1() {
        assert_eq!(VM::new().eval("
            let x = 0

            function setX (y: number) {
                x = y
            }

            debug(x)
            setX(1)
            debug(x)
            x
        "), Value::Number(1.0));
    }

    #[test]
    fn closure2() {
        assert_eq!(VM::new().eval("
            let x = 0
            function setX (y: number) {
                x = y
            }

            function wrapper () {
                let x = 100
                setX(1)
                debug(x)  // 100
            }

            debug(x)  // 0
            wrapper()  // x=1
            debug(x)  // 1
            x
        "), Value::Number(1.0));
    }

    #[test]
    fn function_object() {
        assert_eq!(VM::new().eval("
            let double = function double_fn_expression(x: number) {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn anonymous_function_object() {
        assert_eq!(VM::new().eval("
            let double = function (x: number) {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn function_object_cannot_be_referred_by_name() {
        assert_eq!(VM::new().eval("
            function f(x: number) { x * 10 }
            let double = function f(x: number) { x * 2 }

            f(3)
        "), Value::Number(30.0));
    }

    #[test]
    fn call_function_immediately() {
        assert_eq!(VM::new().eval("
            (function (x: number) { x * 2 })(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn function_return_statement() {
        assert_eq!(VM::new().eval("
            function test() {
                return 2
                print(\"unreachable\")
                3
            }

            test()
        "), Value::Number(2.0));
    }

    #[test]
    fn function_return_expression() {
        assert_eq!(VM::new().eval("
            function test() {
                return 10 + print(\"reachable\") + (return 1) + print(\"unreachable\")
            }

            test()
        "), Value::Number(1.0));
    }

    #[test]
    fn for_loop() {
        assert_eq!(VM::new().eval("
        let i = 100
        for (i in 0 to 10) {
            debug(i)
        }
        debug(i)
        i
        "), Value::Number(100.0));
    }

    #[test]
    fn for_loop_with_break_statement() {
        assert_eq!(VM::new().eval("
        let x = 0
        for (i in -5 to +5) {
            x = x + i
            if (!bool(i)) {
                break
            }
        }
        x
        "), Value::Number(-15.0));
    }

    #[test]
    fn for_loop_with_break_expression() {
        assert_eq!(VM::new().eval("
        let x = 0
        for (i in -5 to +5) {
            x = x + i
            if (!bool(i)) {
                x = 0 + break + print(\"unreachable\")
            }
        }
        x
        "), Value::Number(-15.0));
    }

    #[test]
    fn nested_for_loop_with_break_statement() {
        assert_eq!(VM::new().eval("
        let x = 0
        for (i in 0 to 10) {
            for (j in -5 to +5) {
                x = x + j
                if (!bool(j)) {
                    break
                }
            }
        }
        x
        "), Value::Number(-150.0));
    }

    #[test]
    fn shadowing() {
        assert_eq!(VM::new().eval("
        let x = 0
        {
            let x
            x = 100
            debug(x)
        }
        debug(x)
        x
        "), Value::Number(0.0));
    }

    #[test]
    fn use_variable_in_outer_scope() {
        assert_eq!(VM::new().eval("
        let x = 0
        {
            x = 100
            debug(x)
        }
        debug(x)
        x
        "), Value::Number(100.0));
    }

    mod expression {
        use crate::value::Value;
        use crate::vm::VM;

        #[test]
        fn equal() {
            assert_eq!(VM::new().eval("1 == 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("1 == 2"), Value::Bool(false));
            assert_eq!(VM::new().eval("true == true"), Value::Bool(true));
            assert_eq!(VM::new().eval("true == false"), Value::Bool(false));
            assert_eq!(VM::new().eval("\"a\" == \"a\""), Value::Bool(true));
            assert_eq!(VM::new().eval("\"a\" == \"b\""), Value::Bool(false));
        }

        #[test]
        fn not_equal() {
            assert_eq!(VM::new().eval("1 != 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 != 2"), Value::Bool(true));
            assert_eq!(VM::new().eval("true != true"), Value::Bool(false));
            assert_eq!(VM::new().eval("true != false"), Value::Bool(true));
            assert_eq!(VM::new().eval("\"a\" != \"a\""), Value::Bool(false));
            assert_eq!(VM::new().eval("\"a\" != \"b\""), Value::Bool(true));
        }

        #[test]
        fn less_than() {
            assert_eq!(VM::new().eval("1 < 0"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 < 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 < 2"), Value::Bool(true));
            assert_eq!(VM::new().eval("true < false"), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" < \"a\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" < \"1.0\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(VM::new().eval("1 <= 0"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 <= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("1 <= 2"), Value::Bool(true));
            assert_eq!(VM::new().eval("true < false"), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" < \"a\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" < \"1.0\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
        }

        #[test]
        fn greater_than() {
            assert_eq!(VM::new().eval("0 > 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 > 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("2 > 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("true > false"), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" > \"a\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" > \"1.0\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(VM::new().eval("0 >= 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 >= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("2 >= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("true >= false"), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" >= \"a\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" >= \"1.0\""), Value::String("TypeError: Expected type Number, actual type String".to_string()));
        }
    }

    mod variable_declaration {
        use crate::vm::{Type, Value, Variable, VM};
        use std::ops::ControlFlow;

        #[test]
        fn declare_variable_with_type() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval("let x: number = 1"),
                Value::Number(0.0)
            );

            let variable = match vm.get_variable("x") {
                ControlFlow::Continue(variable) => variable,
                _ => panic!("Expected variable"),
            };
            assert_eq!(variable, Variable {
                type_: Type::Number,
                value: Value::Number(1.0),
            });
        }
    }

    mod reference {
        use crate::vm::{Value, VM};
        use std::collections::HashMap;

        #[test]
        fn set_and_get_object() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
                user
            "), Value::Ref(0));

            let mut properties = HashMap::new();
            properties.insert("id".to_string(), Value::Number(1.0));
            properties.insert("name".to_string(), Value::String("Alice".to_string()));
            assert_eq!(vm.structs.get(&0).unwrap().borrow().clone().properties, properties);
        }

        #[test]
        fn create_nested_object() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }
                struct UserRef { user: User }

                let userRef = UserRef { user: User { id: 1, name: \"Alice\" } }
                userRef
            "), Value::Ref(1));

            let mut properties_user = HashMap::new();
            properties_user.insert("id".to_string(), Value::Number(1.0));
            properties_user.insert("name".to_string(), Value::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert("user".to_string(), Value::Ref(0));

            assert_eq!(vm.structs.get(&1).unwrap().borrow().clone().properties, properties_user_ref);
        }

        #[test]
        fn read_member_of_object() {
            let mut vm = VM::new();
            vm.eval("
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
            ");
            assert_eq!(vm.eval("user.id"), Value::Number(1f64));
            assert_eq!(vm.eval("user.name"), Value::String("Alice".to_string()));
        }

        #[test]
        fn write_member_of_object() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
                user.id = 2
                user
            "), Value::Ref(0));

            let mut properties = HashMap::new();
            properties.insert("id".to_string(), Value::Number(2.0));
            properties.insert("name".to_string(), Value::String("Alice".to_string()));
            assert_eq!(vm.structs.get(&0).unwrap().borrow().clone().properties, properties);
        }

        #[test]
        fn assign_ref_into_another_object() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }
                struct UserRef { user: User }

                let user1 = User { id: 1, name: \"Alice\" }
                let userRef = UserRef { user: user1 }
                userRef
            "), Value::Ref(1));

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Value::Number(1.0));
            expected_members_user1.insert("name".to_string(), Value::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert("user".to_string(), Value::Ref(0));

            assert_eq!(vm.structs.get(&1).unwrap().borrow().clone().properties, properties_user_ref);
        }

        #[test]
        fn write_member_of_object_through_nested_ref() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }
                struct UserRef { user: User }

                let user1 = User { id: 1, name: \"Alice\" }
                let userRef = UserRef { user: user1 }
                userRef.user.id = 2
                userRef
            "), Value::Ref(1));

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Value::Number(2.0));
            expected_members_user1.insert("name".to_string(), Value::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert("user".to_string(), Value::Ref(0));

            assert_eq!(vm.structs.get(&1).unwrap().borrow().clone().properties, properties_user_ref);
        }

        #[test]
        fn access_ref_after_ref_variable_is_overwritten() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }
                struct UserRef { user: User }

                let user1 = User { id: 1, name: \"Alice\" }
                let userRef = UserRef { user: user1 }
                user1 = User {}
                userRef
            "), Value::Ref(1));

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Value::Number(1.0));
            expected_members_user1.insert("name".to_string(), Value::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert("user".to_string(), Value::Ref(0));

            assert_eq!(vm.structs.get(&1).unwrap().borrow().clone().properties, properties_user_ref);
        }

        #[test]
        fn resolve_member_of_primitive() {
            assert_eq!(VM::new().eval("
                let obj = 1
                obj.id
            "), Value::String("Expected reference".to_string()));
        }

        #[test]
        fn resolve_member_of_undefined_object() {
            assert_eq!(VM::new().eval("
                obj.id
            "), Value::String("Undefined variable: obj".to_string()));
        }
    }

    mod built_in_functions {
        use crate::vm::{Value, VM};

        #[test]
        fn number() {
            assert_eq!(VM::new().eval("number(1)"), Value::Number(1.0));
            assert_eq!(VM::new().eval("number(0)"), Value::Number(0.0));
            assert_eq!(VM::new().eval("number(true)"), Value::Number(1.0));
            assert_eq!(VM::new().eval("number(false)"), Value::Number(0.0));
            assert_eq!(VM::new().eval("number(\"123\")"), Value::Number(123.0));
            assert_eq!(VM::new().eval("number(\"NaN\")"), Value::Number(0.0));
        }

        #[test]
        fn bool() {
            assert_eq!(VM::new().eval("bool(1)"), Value::Bool(true));
            assert_eq!(VM::new().eval("bool(0)"), Value::Bool(false));
            assert_eq!(VM::new().eval("bool(100)"), Value::Bool(true));
            assert_eq!(VM::new().eval("bool(true)"), Value::Bool(true));
            assert_eq!(VM::new().eval("bool(false)"), Value::Bool(false));
            assert_eq!(VM::new().eval("bool(\"true\")"), Value::Bool(true));
            assert_eq!(VM::new().eval("bool(\"false\")"), Value::Bool(false));
            assert_eq!(VM::new().eval("bool(\"foo\")"), Value::Bool(true));
            assert_eq!(VM::new().eval("bool(\"FALSE\")"), Value::Bool(true));
        }

        #[test]
        fn string() {
            assert_eq!(VM::new().eval("string(1)"), Value::String("1".to_string()));
            assert_eq!(VM::new().eval("string(0.5)"), Value::String("0.5".to_string()));
            assert_eq!(VM::new().eval("string(true)"), Value::String("true".to_string()));
            assert_eq!(VM::new().eval("string(false)"), Value::String("false".to_string()));
            assert_eq!(VM::new().eval("string(\"ABC\")"), Value::String("ABC".to_string()));
            assert_eq!(VM::new().eval("string(\"\")"), Value::String("".to_string()));
        }

        #[test]
        fn print() {
            assert_eq!(VM::new().eval("print(1)"), Value::String("TypeError: Expected type String, actual type Number".to_string()));
            assert_eq!(VM::new().eval("print(true)"), Value::String("TypeError: Expected type String, actual type Bool".to_string()));
            assert_eq!(VM::new().eval("print(\"ABC\")"), Value::Number(0f64));
        }
    }

    mod gc {
        use crate::value::Value;
        use crate::vm::VM;

        #[test]
        fn clean_up_all_unused_refs() {
            let mut vm = VM::new();
            vm.eval("
                struct User { name: string }
                let alice = User { name: \"Alice\" }
            ");
            assert_eq!(vm.structs.len(), 1);

            vm.gc();
            assert_eq!(vm.structs.len(), 1);

            vm.eval("alice = User {};");
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        fn clean_up_objects_referred_by_another_object() {
            let mut vm = VM::new();
            vm.eval("
                struct User { name: string }
                struct Users { alice: User }
                let users = Users { alice: User { name: \"Alice\" } }
            ");
            assert_eq!(vm.structs.len(), 2);

            vm.gc();
            assert_eq!(vm.structs.len(), 2);

            vm.eval("users = Users {}");
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        fn circular_reference() {
            let mut vm = VM::new();
            vm.eval("
                struct User { name: string, friend: User? }
                let alice = User { name: \"Alice\" }
                let bob = User { name: \"Bob\" }
                alice.friend = bob
                bob.friend = alice
            ");

            assert_eq!(vm.structs.len(), 2);

            vm.gc();
            assert_eq!(vm.structs.len(), 2);

            println!("{:?}", vm.eval("
                alice = User { name: \"Alice\" }
                bob = alice
            "));
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New object assigned into alice and bob
        }

        #[test]
        fn object_allocated_in_function() {
            let mut vm = VM::new();
            vm.eval("
                struct User { name: string }

                function createUser() {
                    let user = User { name: \"Alice\" }
                }
            ");
            assert_eq!(vm.structs.len(), 0);

            vm.eval("createUser()");
            assert_eq!(vm.structs.len(), 1);

            vm.gc();
            assert_eq!(vm.structs.len(), 0);
        }

        #[test]
        fn object_returned_from_function() {
            let mut vm = VM::new();
            vm.eval("
                struct User { name: string? }

                function createUser() {
                    return User { name: \"Alice\" }
                }
            ");
            assert_eq!(vm.structs.len(), 0);

            vm.eval("let user = createUser()");
            assert_eq!(vm.structs.len(), 1);

            vm.gc();
            assert_eq!(vm.structs.len(), 1);

            vm.eval("user = {}");
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        fn reuse_released_address() {
            let mut vm = VM::new();
            vm.eval("
                struct Obj { i: number }
                let a = Obj {i:0}
                let b = Obj {i:1}
             ");
            assert_eq!(vm.structs.len(), 2);

            vm.eval("a = Obj {i:2}; b = a");
            assert_eq!(vm.structs.len(), 3);

            vm.gc();    // address 0 and 1 are released
            assert_eq!(vm.structs.len(), 1);

            vm.eval("b = Obj {i:3}");  // address 1 is reused
            vm.eval("b = Obj {i:4}");  // address 2 is reused
            assert_eq!(vm.eval("a"), Value::Ref(2));
            assert_eq!(*vm.structs.get(&2).unwrap().borrow().properties.get("i").unwrap(), Value::Number(2.0));
        }
    }

    mod struct_declaration {
        use crate::value::Value;
        use crate::vm::VM;

        #[test]
        fn declare_struct() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
            "), Value::Number(0.0));
        }
    }

    mod type_check {
        use crate::vm::{Value, VM};

        #[test]
        fn assign_variable_declared_with_type_annotation() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                let x: number = true
            "), Value::String("TypeError: Type Bool is not assignable into Number".to_string()));
        }

        #[test]
        fn assign_variable_declared_without_type_annotation() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                let x = 0
                x = true
            "), Value::String("TypeError: Type Bool is not assignable into type Number".to_string()));
        }

        #[test]
        fn function_parameter() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f(x: number) { x }
                f(true)
            "), Value::String("TypeError: Expected type Number, actual type Bool".to_string()));
        }

        #[test]
        fn return_value() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f() { return \"test\" }
                let x: number = f()
            "), Value::String("TypeError: Type String is not assignable into Number".to_string()));
        }

        #[test]
        fn return_value_with_inferred_type() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f() { return \"test\" }
                let x = 0
                x = f()
            "), Value::String("TypeError: Type String is not assignable into type Number".to_string()));
        }

        #[test]
        fn assign_into_union_type_variable() {
            assert_eq!(VM::new().eval("
                let x: number | string = 0
            "), Value::Number(0f64));
        }

        #[test]
        fn assign_into_struct_property() {
            assert_eq!(VM::new().eval("
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
                user.id = \"userId\"
            "), Value::String("TypeError: Type String is not assignable into type Number".to_string()));
        }

        #[test]
        fn struct_initialization() {
            assert_eq!(VM::new().eval("
                struct User { id: number, name: string }

                let user = User { id: \"userId\", name: \"Alice\" }
            "), Value::String("TypeError: Type String is not assignable into type Number".to_string()));
        }
    }
}

