use crate::node::{Node, TypeExpression};
use crate::parser::parse;
use crate::punctuator_kind::PunctuatorKind;
use crate::type_::{FunctionParameterDefinition, FunctionType, StructPropertyDefinition, StructType, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

pub struct TypeEnvironment {
    symbol_type: HashMap<String, Type>,
    parent: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl TypeEnvironment {
    fn new() -> Self {
        Self {
            symbol_type: HashMap::new(),
            parent: None,
        }
    }

    fn declare_symbol(&mut self, name: &str, type_: Type) {
        self.symbol_type.insert(name.to_string(), type_);
    }

    fn get_symbol_type(&self, name: &str) -> Option<Type> {
        self.symbol_type.get(name).cloned()
            .or_else(|| {
                match &self.parent {
                    Some(parent) => parent.borrow().get_symbol_type(name),
                    None => None,
                }
            })
    }
}

pub struct TypeChecker {
    environments: Vec<Rc<RefCell<TypeEnvironment>>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            environments: vec![Rc::new(RefCell::new(TypeEnvironment::new()))],
        };

        checker.declare_symbol("number", Type::Function(FunctionType {
            parameters: vec![FunctionParameterDefinition {
                name: "value".to_string(),
                type_: Type::Unchecked,
            }],
            return_type: Box::new(Type::Number),
        }));
        checker.declare_symbol("bool", Type::Function(FunctionType {
            parameters: vec![FunctionParameterDefinition {
                name: "value".to_string(),
                type_: Type::Unchecked,
            }],
            return_type: Box::new(Type::Bool),
        }));
        checker.declare_symbol("string", Type::Function(FunctionType {
            parameters: vec![FunctionParameterDefinition {
                name: "value".to_string(),
                type_: Type::Unchecked,
            }],
            return_type: Box::new(Type::String),
        }));
        checker.declare_symbol("print", Type::Function(FunctionType {
            parameters: vec![FunctionParameterDefinition {
                name: "value".to_string(),
                type_: Type::String,
            }],
            return_type: Box::new(Type::Null),
        }));
        checker.declare_symbol("debug", Type::Function(FunctionType {
            parameters: vec![FunctionParameterDefinition {
                name: "value".to_string(),
                type_: Type::Unchecked,
            }],
            return_type: Box::new(Type::Null),
        }));

        checker
    }

    fn eval_node_type(&self, node: &Node) -> Result<Type, String> {
        match node {
            Node::Program(..) => Err(format!("{:?} is not an expression", node)),

            // Statement
            Node::EmptyStatement => Err(format!("Cannot evaluate type of {:?}", node)),
            Node::IfStatement(..) => Err(format!("Cannot evaluate type of {:?}", node)),
            Node::ForStatement(..) => Err(format!("Cannot evaluate type of {:?}", node)),
            Node::VariableDeclaration(_name, declared_type, value) => {
                let value_type = match value {
                    Some(initial_value) => self.eval_node_type(initial_value)?,
                    None => Type::Unchecked,
                };

                let declared_type = match declared_type {
                    Some(declared_type) => &self.eval_type_expression(declared_type),
                    None => &value_type,
                };

                if !value_type.is_assignable(declared_type) {
                    panic!("TypeError: Type {:?} is not assignable into {:?}", value_type, declared_type);
                }

                Ok(declared_type.clone())
            }
            Node::FunctionDeclaration(function_node) => {
                let mut parameter_definitions = vec![];
                for declaration in function_node.parameters.iter() {
                    parameter_definitions.push(FunctionParameterDefinition {
                        name: declaration.name.clone(),
                        type_: self.eval_type_expression(&declaration.type_),
                    });
                }

                let return_type = self.eval_type_expression(&function_node.return_type);

                Ok(Type::Function(FunctionType {
                    parameters: parameter_definitions,
                    return_type: Box::new(return_type),
                }))
            }
            Node::StructDeclaration(name, parameter_declarations) => {
                let mut property_definitions = vec![];

                for property_declarations in parameter_declarations {
                    property_definitions.push(StructPropertyDefinition {
                        name: property_declarations.name.clone(),
                        type_: self.eval_type_expression(&property_declarations.type_),
                    });
                }

                Ok(Type::Struct(StructType {
                    name: name.clone(),
                    properties: property_definitions,
                }))
            }

            // Expression
            Node::ReturnExpression(value) => {
                match value {
                    Some(value) => Ok(self.eval_node_type(value)?),
                    None => Ok(Type::Null),
                }
            }
            Node::BreakExpression => Err(format!("Cannot evaluate type of {:?}", node)),
            Node::FunctionExpression(function_node) => {
                let mut parameter_definitions = vec![];
                for declaration in function_node.parameters.iter() {
                    parameter_definitions.push(FunctionParameterDefinition {
                        name: declaration.name.clone(),
                        type_: self.eval_type_expression(&declaration.type_),
                    });
                }

                let return_type = self.eval_type_expression(&function_node.return_type);

                Ok(Type::Function(FunctionType {
                    parameters: parameter_definitions,
                    return_type: Box::new(return_type),
                }))
            }
            Node::IfExpression(_condition, true_branch, false_brunch) => {
                Ok(Type::union_of(&[
                    self.eval_node_type(true_branch)?,
                    self.eval_node_type(false_brunch)?,
                ]))
            }
            Node::BlockExpression(statements) => {
                let mut ret = Type::Unchecked;
                for statement in statements {
                    ret = self.eval_node_type(statement)?;
                }
                Ok(ret)
            }
            Node::AssignmentExpression(lhs, _rhs) => {
                Ok(self.eval_node_type(lhs)?)
            }
            Node::BinaryExpression(_lhs, operator, _rhs) => {
                match operator {
                    PunctuatorKind::Plus => Ok(Type::Number),
                    PunctuatorKind::Minus => Ok(Type::Number),
                    PunctuatorKind::Multiply => Ok(Type::Number),
                    PunctuatorKind::Divide => Ok(Type::Number),
                    PunctuatorKind::LogicalAnd => Ok(Type::Bool),
                    PunctuatorKind::LogicalOr => Ok(Type::Bool),
                    PunctuatorKind::Equal => Ok(Type::Bool),
                    PunctuatorKind::NotEqual => Ok(Type::Bool),
                    PunctuatorKind::LessThan => Ok(Type::Bool),
                    PunctuatorKind::LessThanOrEqual => Ok(Type::Bool),
                    PunctuatorKind::GreaterThan => Ok(Type::Bool),
                    PunctuatorKind::GreaterThanOrEqual => Ok(Type::Bool),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::UnaryExpression(operator, _operand) => {
                match operator {
                    PunctuatorKind::Plus => Ok(Type::Number),
                    PunctuatorKind::Minus => Ok(Type::Number),
                    PunctuatorKind::LogicalNot => Ok(Type::Bool),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::CallExpression(function, _argument) => {
                let function_type = match self.eval_node_type(function)? {
                    Type::Function(function_type) => function_type,
                    _ => return Err(format!("{:?} is not a function", function)),
                };

                Ok(function_type.return_type.deref().clone())
            }
            Node::MemberExpression(struct_, property) => {
                let struct_type = self.eval_node_type(struct_)?;
                let property_name = match property.deref() {
                    Node::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, but got {:?}", property)),
                };
                match struct_type {
                    Type::Struct(struct_type) => {
                        match struct_type.get_property_type(property_name) {
                            Some(property_type) => Ok(property_type.clone()),
                            None => Err(format!("Undefined property: {}", property_name)),
                        }
                    }
                    _ => Err(format!("Expected struct type, but got {:?}", struct_type)),
                }
            }
            Node::Number(_value) => Ok(Type::Number),
            Node::Bool(_value) => Ok(Type::Bool),
            Node::String(_value) => Ok(Type::String),
            Node::Null => Ok(Type::Null),
            Node::Struct(type_, _property_initializers) => {
                let type_ = self.eval_type_expression(type_);
                Ok(type_)
            }
            Node::Identifier(name) => {
                match self.get_symbol_type(name) {
                    Some(type_) => Ok(type_.clone()),
                    None => Err(format!("Undefined symbol: {}", name)),
                }
            }

            // temp
            Node::RangeIterator(_, _) => Err(format!("Cannot evaluate type of {:?}", node)),
        }
    }

    pub fn check(&mut self, input: &str) -> Result<(), String> {
        match parse(input) {
            Ok(ast) => self.check_node(&ast),
            Err(message) => Err(message),
        }
    }

    pub fn check_node(&mut self, node: &Node) -> Result<(), String> {
        match node {
            Node::Program(statements) => {
                for statement in statements {
                    self.check_node(statement)?;
                }
                Ok(())
            }

            // Statement
            Node::EmptyStatement => {
                Ok(())
            }
            Node::IfStatement(condition, true_branch, false_branch) => {
                self.check_node(condition)?;

                let condition_type = self.eval_node_type(condition)?;
                if !condition_type.is_assignable(&Type::Bool) {
                    return Err(format!("TypeError: If-statement condition must be a Bool, but actual type is {:?}", condition_type));
                }

                self.check_node(true_branch)?;
                if let Some(false_branch) = false_branch {
                    self.check_node(false_branch)?;
                }

                Ok(())
            }
            Node::ForStatement(variable, _iterator, body) => {
                // TODO: check iterator's type
                let env = Rc::new(RefCell::new(TypeEnvironment {
                    symbol_type: HashMap::new(),
                    parent: Some(self.environments.last().unwrap().clone()),
                }));
                self.environments.push(env);
                self.declare_symbol(variable, Type::Unchecked); // TODO: Use iterator's type instead of unchecked
                self.check_node(body)?;
                self.environments.pop();

                Ok(())
            }
            Node::VariableDeclaration(name, declared_type, value) => {
                if let Some(value) = value {
                    self.check_node(value)?;
                }

                let value_type = match value {
                    Some(initial_value) => self.eval_node_type(initial_value)?,
                    None => Type::Unchecked,
                };

                let declared_type = match declared_type {
                    Some(declared_type) => &self.eval_type_expression(declared_type),
                    None => &value_type,
                };

                if !value_type.is_assignable(declared_type) {
                    return Err(format!("TypeError: Type {:?} is not assignable into {:?}", value_type, declared_type));
                }

                self.declare_symbol(name, declared_type.clone());
                Ok(())
            }
            Node::FunctionDeclaration(function_node) => {
                let env = Rc::new(RefCell::new(TypeEnvironment {
                    symbol_type: HashMap::new(),
                    parent: Some(self.environments.last().unwrap().clone()),
                }));
                self.environments.push(env);

                for declaration in function_node.parameters.iter() {
                    self.declare_symbol(&declaration.name, self.eval_type_expression(&declaration.type_));
                }
                let return_type = self.eval_type_expression(&function_node.return_type);

                self.check_node(&function_node.body)?;

                self.environments.pop();

                self.declare_symbol(&function_node.name, Type::Function(FunctionType {
                    parameters: function_node.parameters.iter().map(|declaration| {
                        FunctionParameterDefinition {
                            name: declaration.name.clone(),
                            type_: self.eval_type_expression(&declaration.type_),
                        }
                    }).collect(),
                    return_type: Box::new(return_type),
                }));

                Ok(())
            }
            Node::StructDeclaration(name, parameter_declarations) => {
                // TODO: nameという名前で宣言されているのは、構造体ではなく、構造体の型のこと
                self.declare_symbol(name, Type::Struct(StructType {
                    name: name.clone(),
                    properties: parameter_declarations.iter().map(|declaration| {
                        StructPropertyDefinition {
                            name: declaration.name.clone(),
                            type_: self.eval_type_expression(&declaration.type_),
                        }
                    }).collect(),
                }));

                Ok(())
            }

            // Expression
            Node::ReturnExpression(value) => {
                // TODO: check return type with current function scope

                if let Some(value) = value {
                    self.check_node(value)?;
                }

                Ok(())
            }
            Node::BreakExpression => {
                // TODO: check if it's in loop
                Ok(())
            }
            Node::FunctionExpression(function_node) => {
                let env = Rc::new(RefCell::new(TypeEnvironment {
                    symbol_type: HashMap::new(),
                    parent: Some(self.environments.last().unwrap().clone()),
                }));
                self.environments.push(env);

                for declaration in function_node.parameters.iter() {
                    self.declare_symbol(&declaration.name, self.eval_type_expression(&declaration.type_));
                }

                self.check_node(&function_node.body)?;

                self.environments.pop();

                Ok(())
            }
            Node::IfExpression(condition, true_branch, false_branch) => {
                self.check_node(condition)?;

                let condition_type = self.eval_node_type(condition)?;
                if !condition_type.is_assignable(&Type::Bool) {
                    return Err(format!("TypeError: If-statement condition must be a Bool, but actual type is {:?}", condition_type));
                }

                self.check_node(true_branch)?;
                self.check_node(false_branch)?;

                Ok(())
            }
            Node::BlockExpression(statements) => {
                let env = Rc::new(RefCell::new(TypeEnvironment {
                    symbol_type: HashMap::new(),
                    parent: Some(self.environments.last().unwrap().clone()),
                }));
                self.environments.push(env);
                for statement in statements {
                    self.check_node(statement)?;
                }
                self.environments.pop();
                Ok(())
            }
            Node::AssignmentExpression(lhs, rhs) => {
                self.check_node(lhs)?;
                self.check_node(rhs)?;

                let lhs_type = self.eval_node_type(lhs)?;
                let rhs_type = self.eval_node_type(rhs)?;

                if !rhs_type.is_assignable(&lhs_type) {
                    return Err(format!("TypeError: Type {:?} is not assignable into {:?}", rhs_type, lhs_type));
                }

                Ok(())
            }
            Node::BinaryExpression(lhs, operator, rhs) => {
                self.check_node(lhs)?;
                self.check_node(rhs)?;

                let lhs_type = self.eval_node_type(lhs)?;
                let rhs_type = self.eval_node_type(rhs)?;

                match operator {
                    PunctuatorKind::Equal |
                    PunctuatorKind::NotEqual => {
                        if !rhs_type.is_assignable(&lhs_type) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", lhs_type, rhs_type));
                        }
                    }
                    PunctuatorKind::Plus |
                    PunctuatorKind::Minus |
                    PunctuatorKind::Multiply |
                    PunctuatorKind::Divide |
                    PunctuatorKind::LessThan |
                    PunctuatorKind::LessThanOrEqual |
                    PunctuatorKind::GreaterThan |
                    PunctuatorKind::GreaterThanOrEqual => {
                        if !lhs_type.is_assignable(&Type::Number) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Number, lhs_type));
                        }
                        if !rhs_type.is_assignable(&Type::Number) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Number, rhs_type));
                        }
                    }
                    PunctuatorKind::LogicalAnd |
                    PunctuatorKind::LogicalOr => {
                        if !lhs_type.is_assignable(&Type::Bool) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Bool, lhs_type));
                        }
                        if !rhs_type.is_assignable(&Type::Bool) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Bool, rhs_type));
                        }
                    }
                    _ => return Err(format!("Unexpected operator: {:?}", operator)),
                }

                Ok(())
            }
            Node::UnaryExpression(operator, operand) => {
                self.check_node(operand)?;

                let operand_type = self.eval_node_type(operand)?;

                match operator {
                    PunctuatorKind::Plus |
                    PunctuatorKind::Minus => {
                        if !operand_type.is_assignable(&Type::Number) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Number, operand_type));
                        }
                    }
                    PunctuatorKind::LogicalNot => {
                        if !operand_type.is_assignable(&Type::Bool) {
                            return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", Type::Bool, operand_type));
                        }
                    }
                    _ => return Err(format!("Unexpected operator: {:?}", operator)),
                }

                Ok(())
            }
            Node::CallExpression(function, parameters) => {
                self.check_node(function)?;
                for parameter in parameters {
                    self.check_node(parameter)?;
                }

                let function_type = match self.eval_node_type(function)? {
                    Type::Function(function_type) => function_type,
                    _ => return Err(format!("{:?} is not a function", function)),
                };

                for (value, parameter) in parameters.iter().zip(function_type.parameters.iter()) {
                    let value_type = self.eval_node_type(value)?;
                    let parameter_type = &parameter.type_;

                    if !value_type.is_assignable(parameter_type) {
                        return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", parameter_type, value_type));
                    }
                }

                Ok(())
            }
            Node::MemberExpression(struct_, property) => {
                self.check_node(struct_)?;

                let property_name = match property.deref() {
                    Node::Identifier(name) => name,
                    _ => return Err(format!("Expected identifier, but got {:?}", property)),
                };

                let struct_type = match self.eval_node_type(struct_)? {
                    Type::Struct(struct_type) => struct_type,
                    other => return Err(format!("Expected struct type, but got {:?}", other)),
                };

                if struct_type.get_property_type(property_name).is_none() {
                    return Err(format!("Undefined property: {}", property_name));
                }

                Ok(())
            }
            Node::Number(_value) => Ok(()),
            Node::Bool(_value) => Ok(()),
            Node::String(_value) => Ok(()),
            Node::Null => Ok(()),
            Node::Struct(type_, property_initializers) => {
                let struct_type = match self.eval_type_expression(type_) {
                    Type::Struct(struct_type) => struct_type,
                    other => return Err(format!("Expected struct type, actual type is {:?}", other)),
                };

                for definition in struct_type.properties.iter() {
                    let initializer = match property_initializers
                        .iter().find(|initializer| initializer.name == definition.name) {
                        Some(initializer) => initializer,
                        None => return Err(format!("Property {} is not initialized", definition.name)),
                    };

                    let initializer_type = self.eval_node_type(&initializer.value)?;

                    if !initializer_type.is_assignable(&definition.type_) {
                        return Err(format!("TypeError: Expected type {:?}, but actual type is {:?}", definition.type_, initializer_type));
                    }
                }

                for initializer in property_initializers {
                    if !struct_type.properties.iter().any(|definition| definition.name == initializer.name) {
                        return Err(
                            format!("Property \"{}\" is not defined in struct \"{}\"",
                                    initializer.name, struct_type.name)
                        );
                    };
                }

                Ok(())
            }
            Node::Identifier(name) => {
                if self.get_symbol_type(name).is_none() {
                    return Err(format!("Undefined symbol: {}", name));
                }

                Ok(())
            }

            // temp
            Node::RangeIterator(..) => Ok(())
        }
    }

    fn declare_symbol(&mut self, name: &str, type_: Type) {
        match self.environments.last() {
            Some(environment) => environment.borrow_mut().declare_symbol(name, type_),
            None => panic!("No environment"),
        }
    }

    fn get_symbol_type(&self, name: &str) -> Option<Type> {
        match self.environments.last() {
            Some(environment) => environment.borrow().get_symbol_type(name),
            None => panic!("No environment"),
        }
    }

    fn eval_type_expression(&self, expression: &TypeExpression) -> Type {
        match expression {
            TypeExpression::Identifier(name) => {
                match name.as_str() {
                    "number" => Type::Number,
                    "bool" => Type::Bool,
                    "string" => Type::String,
                    "null" => Type::Null,
                    _ => match self.get_symbol_type(name) {
                        Some(type_) => type_,
                        None => panic!("Undefined symbol: {}", name),
                    }
                }
            }
            TypeExpression::Optional(inner) => {
                Type::Union(vec![self.eval_type_expression(inner), Type::Null])
            }
            TypeExpression::Union(types) => {
                Type::Union(
                    types.iter().map(|type_| self.eval_type_expression(type_)).collect()
                )
            }
        }
    }
}