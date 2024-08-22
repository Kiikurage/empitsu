use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::ops::{ControlFlow, Deref};
use std::rc::Rc;

use crate::node::Node;
use crate::parser::parse;
use crate::punctuation_kind::PunctuationKind;
use crate::type_::Type;
use crate::type_checker::TypeChecker;
use crate::value::{FunctionValue, NativeFunction, NativeFunctionValue, StructValue, Value};

#[derive(Clone, PartialEq, Debug)]
struct Variable {
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

    fn declare_variable(&mut self, name: &str, value: &Value) {
        self.variables.insert(name.to_string(), Variable { value: value.clone() });
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

        vm.install_native_function("number", &["value"], |args, _vm| {
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
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Number", value))))
            }
        });
        vm.install_native_function("bool", &["value"], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            match value {
                Value::Number(value) => ControlFlow::Continue(Value::Bool(*value != 0.0)),
                Value::Bool(value) => ControlFlow::Continue(Value::Bool(*value)),
                Value::String(value) => ControlFlow::Continue(Value::Bool(value != "false")),
                Value::Function { .. } => ControlFlow::Continue(Value::Bool(true)),
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Bool", value)))),
            }
        });
        vm.install_native_function("string", &["value"], |args, _vm| {
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
                _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to cast {:?} to Bool", value)))),
            }
        });
        vm.install_native_function("print", &["value"], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => return ControlFlow::Break(BreakResult::Error(Value::String("Expected argument".to_string()))),
            };
            println!("{}", value.clone().into_string().into_control_flow()?);
            ControlFlow::Continue(Value::Number(0.0))
        });
        vm.install_native_function("debug", &["value"], |args, _vm| {
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
                Value::Null => println!("null"),
            }
            ControlFlow::Continue(Value::Number(0.0))
        });

        vm
    }
}

impl VM {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn eval(&mut self, input: &str) -> Value {
        let result = parse(input);
        let ast = if result.errors.is_empty() {
            result.node
        } else {
            return Value::String(result.errors.first().unwrap().get_message());
        };

        if let Err(message) = TypeChecker::new().check_node(&ast) {
            return Value::String(message);
        };

        match self.eval_node(&ast) {
            ControlFlow::Continue(value) => value,
            ControlFlow::Break(BreakResult::Return(value)) => value,
            ControlFlow::Break(BreakResult::Break(value)) => value,
            ControlFlow::Break(BreakResult::Error(value)) => value,
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
            Node::VariableDeclaration(name, _variable_type, value) => {
                let value = match value {
                    Some(value) => self.eval_node(value)?,
                    None => Value::Number(0.0),
                };
                self.declare_variable(name, &value);
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
                self.declare_variable(variable, &start);
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
            Node::FunctionDeclaration(function_node) => {
                let mut parameters = vec![];
                for parameter_declaration in function_node.parameters.iter() {
                    parameters.push(parameter_declaration.name.clone());
                }

                let function = Value::Function(FunctionValue {
                    name: function_node.name.clone(),
                    parameters,
                    body: function_node.body.clone(),
                    closure: self.environments.last().unwrap().clone(),
                });
                self.declare_variable(&function_node.name, &function);

                ControlFlow::Continue(function)
            }
            Node::StructDeclaration(_name, property_declarations) => {
                let mut properties = vec![];
                for property_declaration in property_declarations {
                    properties.push(property_declaration.name.clone());
                }

                ControlFlow::Continue(Value::Number(0.0))
            }

            // Expression
            Node::FunctionExpression(function_node) => {
                let mut parameters = vec![];
                for parameter_declaration in function_node.parameters.iter() {
                    parameters.push(parameter_declaration.name.clone());
                }

                let function = Value::Function(FunctionValue {
                    name: function_node.name.clone(),
                    parameters,
                    body: function_node.body.clone(),
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
                    Node::Identifier(name) => self.set_variable(name, &value)?,
                    Node::MemberExpression(object, property) => {
                        let mut struct_value = match self.eval_node(object)? {
                            Value::Ref(address) => {
                                match self.structs.get(&address) {
                                    Some(object) => object.borrow_mut(),
                                    None => return ControlFlow::Break(BreakResult::Error(Value::String("Undefined object".to_string()))),
                                }
                            }
                            _ => return ControlFlow::Break(BreakResult::Error(Value::String("Expected reference".to_string()))),
                        };

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
                    PunctuationKind::Plus => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? + right.as_number().into_control_flow()?)),
                    PunctuationKind::Minus => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? - right.as_number().into_control_flow()?)),
                    PunctuationKind::Asterisk => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? * right.as_number().into_control_flow()?)),
                    PunctuationKind::Slash => ControlFlow::Continue(Value::Number(left.as_number().into_control_flow()? / right.as_number().into_control_flow()?)),
                    PunctuationKind::VerticalLineVerticalLine => ControlFlow::Continue(Value::Bool(left.as_bool().into_control_flow()? || right.as_bool().into_control_flow()?)),
                    PunctuationKind::AndAnd => ControlFlow::Continue(Value::Bool(left.as_bool().into_control_flow()? && right.as_bool().into_control_flow()?)),
                    PunctuationKind::EqualEqual => ControlFlow::Continue(Value::Bool(left == right)),  // TODO: 演算子オーバーロード
                    PunctuationKind::ExclamationEqual => ControlFlow::Continue(Value::Bool(left != right)),  // TODO: 演算子オーバーロード
                    PunctuationKind::LeftChevron => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? < right.as_number().into_control_flow()?)),
                    PunctuationKind::LeftChevronEqual => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? <= right.as_number().into_control_flow()?)),
                    PunctuationKind::RightChevron => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? > right.as_number().into_control_flow()?)),
                    PunctuationKind::RightChevronEqual => ControlFlow::Continue(Value::Bool(left.as_number().into_control_flow()? >= right.as_number().into_control_flow()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval_node(operand)?;

                match operator {
                    PunctuationKind::Plus => ControlFlow::Continue(Value::Number(operand.as_number().into_control_flow()?)),
                    PunctuationKind::Minus => ControlFlow::Continue(Value::Number(-operand.as_number().into_control_flow()?)),
                    PunctuationKind::Exclamation => ControlFlow::Continue(Value::Bool(!operand.as_bool().into_control_flow()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::CallExpression(function, parameters) => {
                let callee = self.eval_node(function)?;
                match callee {
                    Value::Function(function) => {
                        let mut evaluated_parameters = vec![];
                        for parameter in parameters {
                            evaluated_parameters.push(self.eval_node(parameter.value.deref())?);
                        }

                        let mut environment = Environment::new();
                        environment.parent = Some(function.closure);
                        self.environments.push(Rc::new(RefCell::new(environment)));

                        for (argument, parameter_name) in evaluated_parameters.iter().zip(function.parameters.iter()) {
                            self.declare_variable(parameter_name, argument);
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
                        let mut evaluated_parameters = vec![];
                        for parameter in parameters {
                            evaluated_parameters.push(self.eval_node(parameter.value.deref())?);
                        }

                        let mut environment = Environment::new();
                        environment.parent = self.environments.first().map(Rc::clone);
                        self.environments.push(Rc::new(RefCell::new(environment)));

                        let ret = (function.body)(&evaluated_parameters, self);

                        self.environments.pop();
                        ret
                    }
                    // TODO: struct instantiation
                    // Node::Struct(_type, property_initializers) => {
                    //     let mut properties = HashMap::new();
                    //     for StructPropertyInitializer { name, value } in property_initializers.iter() {
                    //         properties.insert(name.clone(), self.eval_node(value)?);
                    //     }
                    //
                    //     let address = self.allocate_object();
                    //     self.structs.insert(address, RefCell::new(StructValue { properties }));
                    //
                    //     ControlFlow::Continue(Value::Ref(address))
                    // }
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

    fn declare_variable(&mut self, name: &str, value: &Value) {
        match self.environments.last() {
            Some(environment) => environment.borrow_mut().declare_variable(name, value),
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
        parameters: &[&str],
        body: NativeFunction,
    ) {
        let mut global = self.environments.first().unwrap().borrow_mut();

        let parameters = parameters.iter().cloned()
            .map(str::to_string)
            .collect();

        let native_function = Value::NativeFunction(NativeFunctionValue {
            name: name.to_string(),
            parameters,
            body,
        });
        global.variables.insert(name.to_string(), Variable { value: native_function });
    }

    fn allocate_object(&self) -> usize {
        for i in 0.. {
            if !self.structs.contains_key(&i) {
                return i;
            }
        }
        panic!("Failed to allocate object");
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
        assert_eq!(VM::new().eval("let if"), Value::String("Syntax error: (1:5) \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let let"), Value::String("Syntax error: (1:5) \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let for"), Value::String("Syntax error: (1:5) \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let function"), Value::String("Syntax error: (1:5) \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let true"), Value::String("Syntax error: (1:5) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("let false"), Value::String("Syntax error: (1:5) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("let else"), Value::String("Syntax error: (1:5) \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let return"), Value::String("Syntax error: (1:5) \"return\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("let null"), Value::String("Syntax error: (1:5) \"null\" is a reserved word".to_string()));
    }

    #[test]
    fn reserved_words_in_function_name() {
        assert_eq!(VM::new().eval("function if() {}"), Value::String("Syntax error: (1:10) \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function let() {}"), Value::String("Syntax error: (1:10) \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function for() {}"), Value::String("Syntax error: (1:10) \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function function() {}"), Value::String("Syntax error: (1:10) \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function true() {}"), Value::String("Syntax error: (1:10) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("function false() {}"), Value::String("Syntax error: (1:10) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("function else() {}"), Value::String("Syntax error: (1:10) \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function return() {}"), Value::String("Syntax error: (1:10) \"return\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function null() {}"), Value::String("Syntax error: (1:10) \"null\" is a reserved word".to_string()));
    }

    #[test]
    fn reserved_words_in_function_parameter() {
        assert_eq!(VM::new().eval("function f(if): null {}"), Value::String("Syntax error: (1:12) \"if\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(let): null {}"), Value::String("Syntax error: (1:12) \"let\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(for): null {}"), Value::String("Syntax error: (1:12) \"for\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(function): null {}"), Value::String("Syntax error: (1:12) \"function\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(true): null {}"), Value::String("Syntax error: (1:12) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("function f(false): null {}"), Value::String("Syntax error: (1:12) Expected identifier".to_string()));
        assert_eq!(VM::new().eval("function f(else): null {}"), Value::String("Syntax error: (1:12) \"else\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(return): null {}"), Value::String("Syntax error: (1:12) \"return\" is a reserved word".to_string()));
        assert_eq!(VM::new().eval("function f(null): null {}"), Value::String("Syntax error: (1:12) \"null\" is a reserved word".to_string()));
    }

    #[test]
    fn test_eval() {
        // assert_eq!(VM::new().eval("1+2"), Value::Number(3.0));
        // assert_eq!(VM::new().eval("1+2*\n3"), Value::Number(7.0));
        // assert_eq!(VM::new().eval("(1+2)*3"), Value::Number(9.0));
        // assert_eq!(VM::new().eval("debug(1)"), Value::Number(0.0));
        // assert_eq!(VM::new().eval("number(1)+number(true)"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("{1+2}"), Value::Number(3.0));
        assert_eq!(VM::new().eval("if (true) 2 else 3"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("if (true) 2 else 3"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("if (true) { debug(123); 2 } else { 3 }"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("if (true) 2 else 3*3"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("1 + if (false) 2 else 3 * 3"), Value::Number(10.0));
        // assert_eq!(VM::new().eval("1 + if (false) 2 else if (false) 3 else 4"), Value::Number(5.0));
        // assert_eq!(VM::new().eval("let x=false; if (x) 1 else 2"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("let x=true; if (x) 1 else 2"), Value::Number(1.0));
        // assert_eq!(VM::new().eval("let x=true; let y=if (x) 2 else 3; let z=y*y; z"), Value::Number(4.0));
        // assert_eq!(VM::new().eval("let x=1; x=2; x"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("let x=5; x=x*x; x"), Value::Number(25.0));
        // assert_eq!(VM::new().eval("true && true"), Value::Bool(true));
        // assert_eq!(VM::new().eval("true && false"), Value::Bool(false));
        // assert_eq!(VM::new().eval("false && true"), Value::Bool(false));
        // assert_eq!(VM::new().eval("false && false"), Value::Bool(false));
        // assert_eq!(VM::new().eval("true || true"), Value::Bool(true));
        // assert_eq!(VM::new().eval("true || false"), Value::Bool(true));
        // assert_eq!(VM::new().eval("false || true"), Value::Bool(true));
        // assert_eq!(VM::new().eval("false || false"), Value::Bool(false));
        // assert_eq!(VM::new().eval("false + 1"), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
        // assert_eq!(VM::new().eval("\"hello\" + 1"), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        //
        // assert_eq!(VM::new().eval("
        // let x = 0
        // for (i in 0 to 10) {
        //     debug(i)
        //     x = x + i
        // }
        // x
        // "), Value::Number(45.0));
    }

    #[test]
    fn if_statement() {
        assert_eq!(VM::new().eval("
            if (0) {
                print(\"true\")
            } else {
                print(\"false\")
            }
        "), Value::String("TypeError: If-statement condition must be a Bool, but actual type is Number".to_string()));
    }

    #[test]
    fn declare_function() {
        assert_eq!(VM::new().eval("
            function test (x: number):null {
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

            function setX (y: number): null {
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
            function setX (y: number):null {
                x = y
            }

            function wrapper ():null {
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
            let double = function (x: number):number {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn anonymous_function_object() {
        assert_eq!(VM::new().eval("
            let double = function (x: number): number {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn call_function_immediately() {
        assert_eq!(VM::new().eval("
            (function (x: number): number { x * 2 })(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn function_return_statement() {
        assert_eq!(VM::new().eval("
            function test():number {
                return 2
                print(\"unreachable\")
                3
            }

            test()
        "), Value::Number(2.0));
    }

    #[test]
    #[ignore] // 文式の型評価
    fn function_return_expression() {
        assert_eq!(VM::new().eval("
            function test():number {
                return true && (return 1)
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
    #[ignore] // 文式の型評価
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
            assert_eq!(VM::new().eval("true < false"), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" < \"a\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" < \"1.0\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(VM::new().eval("1 <= 0"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 <= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("1 <= 2"), Value::Bool(true));
            assert_eq!(VM::new().eval("true < false"), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" < \"a\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" < \"1.0\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        }

        #[test]
        fn greater_than() {
            assert_eq!(VM::new().eval("0 > 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 > 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("2 > 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("true > false"), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" > \"a\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" > \"1.0\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(VM::new().eval("0 >= 1"), Value::Bool(false));
            assert_eq!(VM::new().eval("1 >= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("2 >= 1"), Value::Bool(true));
            assert_eq!(VM::new().eval("true >= false"), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
            assert_eq!(VM::new().eval("\"a\" >= \"a\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
            assert_eq!(VM::new().eval("\"0.0\" >= \"1.0\""), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        }
    }

    mod variable_declaration {
        use crate::vm::{Value, Variable, VM};
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
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
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
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let userRef = UserRef(user=User(id=1, name=\"Alice\"))
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
        #[ignore] // 型検査器の情報引き継ぎ
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
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
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
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
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
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
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
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
                user1 = User(id=2, name=\"Bob\")
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
            "), Value::String("Expected struct type, but got Number".to_string()));
        }

        #[test]
        fn resolve_member_of_undefined_object() {
            assert_eq!(VM::new().eval("
                obj.id
            "), Value::String("Undefined symbol: obj".to_string()));
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
            assert_eq!(VM::new().eval("print(1)"), Value::String("TypeError: Expected type String, but actual type is Number".to_string()));
            assert_eq!(VM::new().eval("print(true)"), Value::String("TypeError: Expected type String, but actual type is Bool".to_string()));
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
                struct User(name: string)
                let alice = User(name=\"Alice\")
            ");
            assert_eq!(vm.structs.len(), 1);

            vm.gc();
            assert_eq!(vm.structs.len(), 1);

            vm.eval("alice = User {};");
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
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

            vm.eval("users = Users { alice: User { name: \"Alice2\" } }");
            vm.gc();
            assert_eq!(vm.structs.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        #[ignore] // 循環参照
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
        #[ignore] // 型検査器の情報引き継ぎ
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
        #[ignore] // 型検査器の情報引き継ぎ
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
        #[ignore] // 型検査器の情報引き継ぎ
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
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
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
            "), Value::String("TypeError: Type Bool is not assignable into Number".to_string()));
        }

        #[test]
        fn function_parameter() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f(x: number):number { x }
                f(true)
            "), Value::String("TypeError: Expected type Number, but actual type is Bool".to_string()));
        }

        #[test]
        fn return_value() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f(): string { return \"test\" }
                let x: number = f()
            "), Value::String("TypeError: Type String is not assignable into Number".to_string()));
        }

        #[test]
        fn return_value_with_inferred_type() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("
                function f(): string { return \"test\" }
                let x = 0
                x = f()
            "), Value::String("TypeError: Type String is not assignable into Number".to_string()));
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
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
                user.id = \"userId\"
            "), Value::String("TypeError: Type String is not assignable into Number".to_string()));
        }

        #[test]
        fn struct_initialization() {
            assert_eq!(VM::new().eval("
                struct User(
                    id: number,
                    name: string
                )

                let user = User(
                    id: \"userId\",
                    name: \"Alice\"
                )
            "), Value::String("TypeError: Expected type Number, but actual type is String".to_string()));
        }
    }
}

