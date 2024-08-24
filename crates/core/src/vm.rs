use crate::node::Node;
use crate::parser::parse;
use crate::punctuation_kind::PunctuationKind;
use crate::type_checker::TypeChecker;
use crate::value::{FunctionValue, NativeFunction, NativeFunctionValue, Object, Primitive, StructDefinitionValue, StructValue};
use std::collections::{hash_map, BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::ops::{ControlFlow, Deref};
use std::rc::Rc;

#[derive(Default, Debug, PartialEq)]
pub struct StackFrame {
    offset: usize,
    variable_offset: HashMap<String, usize>,

    // VM内での親Frameのindex
    parent: Option<usize>,
}

impl StackFrame {
    fn new(offset: usize, parent: Option<usize>) -> Self {
        StackFrame { offset, variable_offset: HashMap::new(), parent }
    }
}

struct Stack {
    stack: Vec<Primitive>,
    frames: Vec<StackFrame>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            stack: Vec::new(),
            frames: vec![StackFrame::new(0, None)],
        }
    }

    fn enter_frame(&mut self) {
        self.frames.push(
            StackFrame::new(self.stack.len(), Some(self.frames.len() - 1))
        );
    }

    fn enter_frame_with_closure(&mut self, closure: usize) {
        self.frames.push(
            StackFrame::new(self.stack.len(), Some(closure))
        );
    }

    fn exit_frame(&mut self) {
        self.frames.pop();
    }

    fn declare_variable(&mut self, name: String, value: Primitive) {
        let frame = self.frames.last_mut().unwrap();

        let offset = frame.variable_offset.len();
        frame.variable_offset.insert(name, offset);
        self.stack.insert(frame.offset + offset, value);
    }

    fn get_variable(&self, name: &str) -> Option<&Primitive> {
        let mut frame = self.frames.last()?;

        loop {
            match frame.variable_offset.get(name) {
                Some(offset) => return self.stack.get(frame.offset + offset),
                None => match frame.parent {
                    Some(parent) => frame = self.frames.get(parent)?,
                    None => return None
                },
            }
        }
    }

    fn set_variable(&mut self, name: String, value: Primitive) -> Result<(), String> {
        let mut frame = self.frames.last_mut().unwrap();

        loop {
            match frame.variable_offset.get(&name) {
                Some(offset) => {
                    self.stack[frame.offset + offset] = value;
                    return Ok(());
                }
                None => match frame.parent {
                    Some(parent) => frame = self.frames.get_mut(parent).unwrap(),
                    None => return Err(format!("Undefined variable: {}", name)),
                },
            }
        }
    }
}

struct Heap {
    pub heap: HashMap<usize, Object>,
}

impl Heap {
    fn new() -> Self {
        Heap {
            heap: HashMap::new(),
        }
    }

    fn allocate(&mut self, object: Object) -> Primitive {
        for address in 0.. {
            if let hash_map::Entry::Vacant(e) = self.heap.entry(address) {
                e.insert(object);
                return Primitive::Ref(address);
            }
        }
        panic!("Failed to allocate object");
    }

    fn dereference(&self, ref_: &Primitive) -> Result<&Object, String> {
        match ref_ {
            Primitive::Ref(address) => match self.heap.get(address) {
                Some(object) => Ok(object),
                None => Err("Undefined object".to_string()),
            },
            _ => Err("Expected reference".to_string()),
        }
    }

    fn dereference_mut(&mut self, ref_: &Primitive) -> Result<&mut Object, String> {
        match ref_ {
            Primitive::Ref(address) => match self.heap.get_mut(address) {
                Some(object) => Ok(object),
                None => Err("Undefined object".to_string()),
            },
            _ => Err("Expected reference".to_string()),
        }
    }
}

pub enum BreakResult {
    Return(Primitive),
    Break(Primitive),
    Error(Primitive),
}

pub struct VM {
    stack: Stack,
    heap: Heap,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM { stack: Stack::new(), heap: Heap::new() };

        vm.install_native_function("number", vec!["value".to_string()], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => {
                    return ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Expected parameter".to_string(),
                    )))
                }
            };
            match value {
                Primitive::Number(value) => ControlFlow::Continue(Primitive::Number(*value)),
                Primitive::Bool(value) => {
                    ControlFlow::Continue(Primitive::Number(if *value { 1.0 } else { 0.0 }))
                }
                Primitive::String(value) => match value.parse::<f64>() {
                    Ok(value) => ControlFlow::Continue(Primitive::Number(if value.is_nan() {
                        0.0
                    } else {
                        value
                    })),
                    Err(_) => ControlFlow::Continue(Primitive::Number(0.0)),
                },
                _ => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                    "Failed to cast {:?} to Number",
                    value
                )))),
            }
        });
        vm.install_native_function("bool", vec!["value".to_string()], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => {
                    return ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Expected parameter".to_string(),
                    )))
                }
            };
            match value {
                Primitive::Number(value) => ControlFlow::Continue(Primitive::Bool(*value != 0.0)),
                Primitive::Bool(value) => ControlFlow::Continue(Primitive::Bool(*value)),
                Primitive::String(value) => {
                    ControlFlow::Continue(Primitive::Bool(value != "false"))
                }
                _ => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                    "Failed to cast {:?} to Bool",
                    value
                )))),
            }
        });
        vm.install_native_function("string", vec!["value".to_string()], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => {
                    return ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Expected parameter".to_string(),
                    )))
                }
            };
            match value {
                Primitive::Number(value) => {
                    ControlFlow::Continue(Primitive::String(value.to_string()))
                }
                Primitive::Bool(value) => {
                    ControlFlow::Continue(Primitive::String(value.to_string()))
                }
                Primitive::String(value) => ControlFlow::Continue(Primitive::String(value.clone())),
                Primitive::Ref(address) => {
                    ControlFlow::Continue(Primitive::String(address.to_string()))
                }
                _ => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                    "Failed to cast {:?} to Bool",
                    value
                )))),
            }
        });
        vm.install_native_function("print", vec!["value".to_string()], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => {
                    return ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Expected parameter".to_string(),
                    )))
                }
            };
            println!("{}", value.clone().into_string().into_control_flow()?);
            ControlFlow::Continue(Primitive::Number(0.0))
        });
        vm.install_native_function("debug", vec!["value".to_string()], |args, _vm| {
            let value = match args.first() {
                Some(value) => value,
                None => {
                    return ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Expected parameter".to_string(),
                    )))
                }
            };
            match value {
                Primitive::Number(value) => println!("{}", value),
                Primitive::Bool(value) => println!("{}", value),
                Primitive::String(value) => println!("{}", value),
                Primitive::Ref(address) => {
                    // TODO: derefしたうえで、内容を表示する
                    println!("ref {}", address)
                }
                Primitive::Null => println!("null"),
            }
            ControlFlow::Continue(Primitive::Number(0.0))
        });

        vm
    }

    pub fn eval(&mut self, input: &str) -> Primitive {
        let result = parse(input);
        let ast = if result.errors.is_empty() {
            result.node
        } else {
            return Primitive::String(result.errors.first().unwrap().get_message());
        };

        if let Err(message) = TypeChecker::new().check_node(&ast) {
            return Primitive::String(message);
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

        for value in self.stack.stack.iter() {
            if let Primitive::Ref(address) = value {
                retained_objects.insert(*address);
            }
        }

        let mut queue = retained_objects.iter().copied().collect::<VecDeque<_>>();
        while let Some(address) = queue.pop_front() {
            let heap_object = match self.heap.heap.get(&address) {
                Some(heap_object) => heap_object,
                None => continue,
            };

            match heap_object {
                Object::StructInstance(struct_value) => {
                    for value in struct_value.properties.values() {
                        if let Primitive::Ref(address) = value {
                            if retained_objects.insert(*address) {
                                queue.push_back(*address);
                            }
                        }
                    }
                    // TODO: 自身の型をretainする
                    // 以下の例の場合、userはUserをretainする必要がある
                    // struct User(name: String)
                    // let user = User(name="Alice")
                }
                Object::StructDefinition(..) => {
                    // TODO: プロパティの型として参照している別の構造体をretainする
                    // 以下の例の場合、UsersはUserを参照しているため、Userもretainする必要がある
                    // struct Users(user1: User)
                }
                Object::Function(..) | Object::NativeFunction(..) => {
                    // TODO: クロージャを通して参照しているオブジェクトをretainする
                    // いまはすべてのframeを見ているため問題ないが、そちらでは
                    // 現在の実行スコープから遡って参照可能なframeのみをチェックすべき
                }
            }
        }

        self.heap
            .heap
            .retain(|address, _| retained_objects.contains(address));
    }

    pub fn get_builtin_object_count() -> usize {
        VM::new().heap.heap.len()
    }

    fn eval_node(&mut self, node: &Node) -> ControlFlow<BreakResult, Primitive> {
        match node {
            Node::Program(nodes) => {
                let mut ret = Primitive::Number(0.0);
                for node in nodes {
                    ret = match self.eval_node(node) {
                        ControlFlow::Continue(value) => value,
                        ControlFlow::Break(BreakResult::Return(_)) => {
                            ret = Primitive::String("return is used outside of function".to_string());
                            break;
                        }
                        ControlFlow::Break(BreakResult::Break(_)) => {
                            ret = Primitive::String("break is used outside of loop".to_string());
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
            Node::EmptyStatement => ControlFlow::Continue(Primitive::Number(0.0)),
            Node::IfStatement(condition, true_branch, false_branch) => {
                let condition = self.eval_node(condition)?.as_bool().into_control_flow()?;
                if condition {
                    self.eval_node(true_branch)?;
                } else if let Some(false_branch) = false_branch {
                    self.eval_node(false_branch)?;
                }
                ControlFlow::Continue(Primitive::Null)
            }
            Node::VariableDeclaration(name, _variable_type, value) => {
                let value = match value {
                    Some(value) => self.eval_node(value)?,
                    None => Primitive::Number(0.0),
                };
                self.stack.declare_variable(name.clone(), value);
                ControlFlow::Continue(Primitive::Null)
            }
            Node::ForStatement(var_name, iterator, body) => {

                let (start, end) = match iterator.deref() {
                    Node::RangeIterator(start, end) => {
                        (self.eval_node(start)?, self.eval_node(end)?)
                    }
                    _ => {
                        return ControlFlow::Break(BreakResult::Error(Primitive::String(
                            "Unsupported iterator type".to_string(),
                        )))
                    }
                };
                let mut i = start.as_number().into_control_flow()?;
                let end = end.as_number().into_control_flow()?;
                while i < end {
                    self.stack.enter_frame();
                    self.stack.declare_variable(var_name.clone(), Primitive::Number(i));
                    let ret = self.eval_node(body);
                    self.stack.exit_frame();

                    match ret {
                        ControlFlow::Break(BreakResult::Break(_)) => break,
                        other => other?,
                    };
                    i += 1f64;
                }

                ControlFlow::Continue(Primitive::Null)
            }
            Node::FunctionDeclaration(function_node) => {
                let mut parameters = vec![];
                for parameter_declaration in function_node.parameters.iter() {
                    parameters.push(parameter_declaration.name.clone());
                }

                let function_ref = self.heap.allocate(Object::Function(FunctionValue {
                    name: function_node.name.clone(),
                    parameters,
                    body: Rc::new(function_node.body.clone()),
                    closure: self.stack.frames.len() - 1,
                }));
                self.stack.declare_variable(function_node.name.clone(), function_ref);

                ControlFlow::Continue(Primitive::Null)
            }
            Node::StructDeclaration(name, property_declarations) => {
                let mut properties = Vec::new();
                for property_declaration in property_declarations {
                    properties.push(property_declaration.name.clone());
                }

                let struct_ref = self.heap.allocate(Object::StructDefinition(StructDefinitionValue {
                    name: name.clone(),
                    properties,
                }));
                self.stack.declare_variable(name.clone(), struct_ref);

                ControlFlow::Continue(Primitive::Null)
            }

            // Expression
            Node::FunctionExpression(function_node) => {
                let mut parameters = vec![];
                for parameter_declaration in function_node.parameters.iter() {
                    parameters.push(parameter_declaration.name.clone());
                }

                let function_ref = self.heap.allocate(Object::Function(FunctionValue {
                    name: function_node.name.clone(),
                    parameters,
                    body: Rc::new(function_node.body.clone()),
                    closure: self.stack.frames.len() - 1,
                }));
                ControlFlow::Continue(function_ref)
            }
            Node::IfExpression(condition, true_branch, false_branch) => {
                let condition = match self.eval_node(condition)?.as_bool() {
                    Ok(condition) => condition,
                    Err(message) => {
                        return ControlFlow::Break(BreakResult::Error(Primitive::String(message)))
                    }
                };

                if condition {
                    self.eval_node(true_branch)
                } else {
                    self.eval_node(false_branch)
                }
            }
            Node::BlockExpression(nodes) => {
                self.stack.enter_frame();

                let mut ret = Primitive::Number(0.0);
                for node in nodes {
                    ret = self.eval_node(node)?;
                }

                self.stack.exit_frame();
                ControlFlow::Continue(ret)
            }
            Node::AssignmentExpression(lhs, value) => {
                let value = self.eval_node(value)?;
                match lhs.as_ref() {
                    Node::Identifier(name) => {
                        return match self.stack.set_variable(name.clone(), value.clone()) {
                            Ok(()) => ControlFlow::Continue(value),
                            Err(message) => ControlFlow::Break(BreakResult::Error(Primitive::String(message))),
                        }
                    }
                    Node::MemberExpression(object_ref, property_name) => {
                        let object_ref = self.eval_node(object_ref)?;
                        let object = match self.heap.dereference_mut(&object_ref) {
                            Ok(object) => object,
                            Err(message) => return ControlFlow::Break(BreakResult::Error(Primitive::String(message))),
                        };

                        match object {
                            Object::StructInstance(ref mut struct_) => {
                                struct_.properties.insert(property_name.clone(), value.clone());
                            }
                            _ => {
                                return ControlFlow::Break(BreakResult::Error(Primitive::String(
                                    "Unsupported Operation: set property of non-struct object"
                                        .to_string(),
                                )))
                            }
                        };
                    }
                    _ => {
                        return ControlFlow::Break(BreakResult::Error(Primitive::String(
                            "Expected identifier".to_string(),
                        )))
                    }
                }
                ControlFlow::Continue(value)
            }
            Node::BinaryExpression(left, operator, right) => {
                let left = self.eval_node(left)?;
                let right = self.eval_node(right)?;

                match operator {
                    PunctuationKind::Plus => ControlFlow::Continue(Primitive::Number(
                        left.as_number().into_control_flow()?
                            + right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::Minus => ControlFlow::Continue(Primitive::Number(
                        left.as_number().into_control_flow()?
                            - right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::Asterisk => ControlFlow::Continue(Primitive::Number(
                        left.as_number().into_control_flow()?
                            * right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::Slash => ControlFlow::Continue(Primitive::Number(
                        left.as_number().into_control_flow()?
                            / right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::VerticalLineVerticalLine => {
                        ControlFlow::Continue(Primitive::Bool(
                            left.as_bool().into_control_flow()?
                                || right.as_bool().into_control_flow()?,
                        ))
                    }
                    PunctuationKind::AndAnd => ControlFlow::Continue(Primitive::Bool(
                        left.as_bool().into_control_flow()?
                            && right.as_bool().into_control_flow()?,
                    )),
                    PunctuationKind::EqualEqual => {
                        ControlFlow::Continue(Primitive::Bool(left == right))
                    } // TODO: 演算子オーバーロード
                    PunctuationKind::ExclamationEqual => {
                        ControlFlow::Continue(Primitive::Bool(left != right))
                    } // TODO: 演算子オーバーロード
                    PunctuationKind::LeftChevron => ControlFlow::Continue(Primitive::Bool(
                        left.as_number().into_control_flow()?
                            < right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::LeftChevronEqual => ControlFlow::Continue(Primitive::Bool(
                        left.as_number().into_control_flow()?
                            <= right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::RightChevron => ControlFlow::Continue(Primitive::Bool(
                        left.as_number().into_control_flow()?
                            > right.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::RightChevronEqual => ControlFlow::Continue(Primitive::Bool(
                        left.as_number().into_control_flow()?
                            >= right.as_number().into_control_flow()?,
                    )),
                    _ => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                        "Unexpected operator: {:?}",
                        operator
                    )))),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval_node(operand)?;

                match operator {
                    PunctuationKind::Plus => ControlFlow::Continue(Primitive::Number(
                        operand.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::Minus => ControlFlow::Continue(Primitive::Number(
                        -operand.as_number().into_control_flow()?,
                    )),
                    PunctuationKind::Exclamation => ControlFlow::Continue(Primitive::Bool(
                        !operand.as_bool().into_control_flow()?,
                    )),
                    _ => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                        "Unexpected operator: {:?}",
                        operator
                    )))),
                }
            }
            Node::CallExpression(callee, parameters) => {
                let mut evaluated_parameters = vec![];
                for parameter in parameters {
                    evaluated_parameters.push(EvaluatedParameter {
                        name: parameter.name.clone(),
                        value: self.eval_node(&parameter.value)?,
                    });
                }

                let callee_ref = self.eval_node(callee)?;
                let callee = match self.heap.dereference(&callee_ref) {
                    Ok(callee) => callee,
                    Err(message) => return ControlFlow::Break(BreakResult::Error(Primitive::String(message))),
                };

                match callee {
                    Object::Function(function) => {
                        let parameters = match parse_parameters(evaluated_parameters, &function.parameters) {
                            Ok(parameters) => parameters,
                            Err(message) => {
                                return ControlFlow::Break(BreakResult::Error(
                                    Primitive::String(message),
                                ))
                            }
                        };

                        self.stack.enter_frame_with_closure(function.closure);

                        for (parameter, parameter_name) in parameters.into_iter().zip(function.parameters.iter()) {
                            self.stack.declare_variable(parameter_name.clone(), parameter);
                        }
                        let ret = match self.eval_node(&function.body.clone()) {
                            ControlFlow::Continue(value) => value,
                            ControlFlow::Break(BreakResult::Return(value)) => value,
                            others => return others,
                        };

                        self.stack.frames.pop();
                        ControlFlow::Continue(ret)
                    }
                    Object::NativeFunction(function) => {
                        let parameters = match parse_parameters(evaluated_parameters, &function.parameters) {
                            Ok(parameters) => parameters,
                            Err(message) => {
                                return ControlFlow::Break(BreakResult::Error(
                                    Primitive::String(message),
                                ))
                            }
                        };

                        self.stack.enter_frame_with_closure(0);

                        let ret = (function.body)(&parameters, self);

                        self.stack.frames.pop();
                        ret
                    }
                    Object::StructDefinition(struct_) => {
                        let mut properties = HashMap::new();
                        let parameters = match parse_parameters(evaluated_parameters, &struct_.properties) {
                            Ok(parameters) => parameters,
                            Err(message) => {
                                return ControlFlow::Break(BreakResult::Error(
                                    Primitive::String(message),
                                ))
                            }
                        };

                        for (parameter, name) in
                            parameters.into_iter().zip(struct_.properties.iter())
                        {
                            properties.insert(name.clone(), parameter);
                        }

                        let ref_ = self.heap.allocate(Object::StructInstance(StructValue {
                            name: struct_.name.clone(),
                            properties,
                        }));

                        ControlFlow::Continue(ref_)
                    }
                    Object::StructInstance(..) => {
                        ControlFlow::Break(BreakResult::Error(Primitive::String(
                            "Unsupported Operation: Use call expression with struct instance"
                                .to_string(),
                        )))
                    }
                }
            }
            Node::Number(value) => ControlFlow::Continue(Primitive::Number(*value)),
            Node::Bool(value) => ControlFlow::Continue(Primitive::Bool(*value)),
            Node::String(value) => ControlFlow::Continue(Primitive::String(value.clone())),
            Node::Identifier(name) => {
                match self.stack.get_variable(name) {
                    Some(value) => ControlFlow::Continue(value.clone()),
                    None => ControlFlow::Break(BreakResult::Error(Primitive::String(format!(
                        "Undefined variable: {}",
                        name
                    )))),
                }
            }
            Node::ReturnExpression(value) => ControlFlow::Break(BreakResult::Return(match value {
                Some(value) => self.eval_node(value)?,
                None => Primitive::Number(0.0),
            })),
            Node::BreakExpression => ControlFlow::Break(BreakResult::Break(Primitive::Number(0.0))),
            Node::MemberExpression(object_ref, property) => {
                let object_ref = self.eval_node(object_ref)?;
                let object = match self.heap.dereference(&object_ref) {
                    Ok(object) => object,
                    Err(message) => return ControlFlow::Break(BreakResult::Error(Primitive::String(message))),
                };

                match object {
                    Object::StructInstance(struct_value) => {
                        match struct_value.properties.get(property) {
                            Some(value) => ControlFlow::Continue(value.clone()),
                            None => ControlFlow::Break(BreakResult::Error(Primitive::String(
                                format!("Undefined property: {}", property),
                            ))),
                        }
                    }
                    _ => ControlFlow::Break(BreakResult::Error(Primitive::String(
                        "Unsupported Operation: get property of non-struct object".to_string(),
                    ))),
                }
            }

            // tmp
            Node::RangeIterator(_start, _end) => ControlFlow::Break(BreakResult::Break(
                Primitive::String("Unsupported".to_string()),
            )),
        }
    }

    fn install_native_function(&mut self, name: &str, parameters: Vec<String>, body: NativeFunction) {
        let ref_ = self.heap.allocate(Object::NativeFunction(NativeFunctionValue {
            name: name.to_string(),
            parameters,
            body,
        }));

        self.stack.declare_variable(name.to_string(), ref_);
    }
}

struct EvaluatedParameter {
    name: Option<String>,
    value: Primitive,
}

fn parse_parameters(
    parameters: Vec<EvaluatedParameter>,
    names: &[String],
) -> Result<Vec<Primitive>, String> {
    let mut map = HashMap::new();

    let mut non_specified_names = names.iter().cloned().collect::<BTreeSet<_>>();
    for parameter in parameters.iter() {
        if let Some(ref name) = parameter.name {
            if !non_specified_names.remove(name) {
                return Err(format!("Unknown parameter: {}", name));
            }
        };
    }

    for parameter in parameters {
        let name = match parameter.name {
            Some(ref name) => name,
            None => match non_specified_names.iter().next() {
                Some(name) => name,
                None => return Err("Too many parameters".to_string()),
            },
        };

        map.insert(name.clone(), parameter.value);
        non_specified_names.remove(&name.clone());
    }

    if !non_specified_names.is_empty() {
        return Err(format!(
            "Too few parameters. Follow parameter(s) is not specified: {}",
            non_specified_names
                .into_iter()
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    Ok(names
        .iter()
        .map(|name| map.remove(name).unwrap())
        .collect::<Vec<_>>())
}

trait IntoControlFlow<T> {
    fn into_control_flow(self) -> ControlFlow<BreakResult, T>;
}

impl<T> IntoControlFlow<T> for Result<T, String> {
    fn into_control_flow(self) -> ControlFlow<BreakResult, T> {
        match self {
            Ok(value) => ControlFlow::Continue(value),
            Err(message) => ControlFlow::Break(BreakResult::Error(Primitive::String(message))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::{Primitive, VM};

    #[test]
    fn reserved_words_in_variable_declaration() {
        assert_eq!(
            VM::new().eval("let if"),
            Primitive::String("Syntax error: (1:5) \"if\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let let"),
            Primitive::String("Syntax error: (1:5) \"let\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let for"),
            Primitive::String("Syntax error: (1:5) \"for\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let function"),
            Primitive::String("Syntax error: (1:5) \"function\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let true"),
            Primitive::String("Syntax error: (1:5) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("let false"),
            Primitive::String("Syntax error: (1:5) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("let else"),
            Primitive::String("Syntax error: (1:5) \"else\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let return"),
            Primitive::String("Syntax error: (1:5) \"return\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("let null"),
            Primitive::String("Syntax error: (1:5) \"null\" is a reserved word".to_string())
        );
    }

    #[test]
    fn reserved_words_in_function_name() {
        assert_eq!(
            VM::new().eval("function if() {}"),
            Primitive::String("Syntax error: (1:10) \"if\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function let() {}"),
            Primitive::String("Syntax error: (1:10) \"let\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function for() {}"),
            Primitive::String("Syntax error: (1:10) \"for\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function function() {}"),
            Primitive::String("Syntax error: (1:10) \"function\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function true() {}"),
            Primitive::String("Syntax error: (1:10) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("function false() {}"),
            Primitive::String("Syntax error: (1:10) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("function else() {}"),
            Primitive::String("Syntax error: (1:10) \"else\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function return() {}"),
            Primitive::String("Syntax error: (1:10) \"return\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function null() {}"),
            Primitive::String("Syntax error: (1:10) \"null\" is a reserved word".to_string())
        );
    }

    #[test]
    fn reserved_words_in_function_parameter() {
        assert_eq!(
            VM::new().eval("function f(if): null {}"),
            Primitive::String("Syntax error: (1:12) \"if\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(let): null {}"),
            Primitive::String("Syntax error: (1:12) \"let\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(for): null {}"),
            Primitive::String("Syntax error: (1:12) \"for\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(function): null {}"),
            Primitive::String("Syntax error: (1:12) \"function\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(true): null {}"),
            Primitive::String("Syntax error: (1:12) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(false): null {}"),
            Primitive::String("Syntax error: (1:12) Expected identifier".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(else): null {}"),
            Primitive::String("Syntax error: (1:12) \"else\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(return): null {}"),
            Primitive::String("Syntax error: (1:12) \"return\" is a reserved word".to_string())
        );
        assert_eq!(
            VM::new().eval("function f(null): null {}"),
            Primitive::String("Syntax error: (1:12) \"null\" is a reserved word".to_string())
        );
    }

    #[test]
    fn test_eval() {
        // assert_eq!(VM::new().eval("1+2"), Value::Number(3.0));
        // assert_eq!(VM::new().eval("1+2*\n3"), Value::Number(7.0));
        // assert_eq!(VM::new().eval("(1+2)*3"), Value::Number(9.0));
        // assert_eq!(VM::new().eval("debug(1)"), Value::Number(0.0));
        // assert_eq!(VM::new().eval("number(1)+number(true)"), Value::Number(2.0));
        // assert_eq!(VM::new().eval("{1+2}"), Value::Number(3.0));
        assert_eq!(VM::new().eval("if (true) 2 else 3"), Primitive::Number(2.0));
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
        assert_eq!(
            VM::new().eval(
                "
            if (0) {
                print(\"true\")
            } else {
                print(\"false\")
            }
        "
            ),
            Primitive::String(
                "TypeError: If-statement condition must be a Bool, but actual type is Number"
                    .to_string()
            )
        );
    }

    #[test]
    fn declare_function() {
        assert_eq!(
            VM::new().eval(
                "
            function test (x: number):null {
                print(string(x*2))
            }

            test(1)
            test(10)
            test(100)
        "
            ),
            Primitive::Number(0.0)
        );
    }

    #[test]
    fn single_line_comment() {
        assert_eq!(
            VM::new().eval(
                "
            let x = 1  // single line comment
            x
        "
            ),
            Primitive::Number(1.0)
        );
    }

    #[test]
    fn multi_line_comment() {
        assert_eq!(
            VM::new().eval(
                "
            let x = 1
            /* THIS IS MULTI LINE COMMENTS

            x = 100

            */
            x
        "
            ),
            Primitive::Number(1.0)
        );
    }

    #[test]
    fn closure1() {
        assert_eq!(
            VM::new().eval(
                "
            let x = 0

            function setX (y: number): null {
                x = y
            }

            debug(x)
            setX(1)
            debug(x)
            x
        "
            ),
            Primitive::Number(1.0)
        );
    }

    #[test]
    fn closure2() {
        assert_eq!(
            VM::new().eval(
                "
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
        "
            ),
            Primitive::Number(1.0)
        );
    }

    #[test]
    fn function_object() {
        assert_eq!(
            VM::new().eval(
                "
            let double = function (x: number):number {
                x * 2
            }

            double(3)
        "
            ),
            Primitive::Number(6.0)
        );
    }

    #[test]
    fn anonymous_function_object() {
        assert_eq!(
            VM::new().eval(
                "
            let double = function (x: number): number {
                x * 2
            }

            double(3)
        "
            ),
            Primitive::Number(6.0)
        );
    }

    #[test]
    fn call_function_immediately() {
        assert_eq!(
            VM::new().eval(
                "
            (function (x: number): number { x * 2 })(3)
        "
            ),
            Primitive::Number(6.0)
        );
    }

    #[test]
    fn function_return_statement() {
        assert_eq!(
            VM::new().eval(
                "
            function test():number {
                return 2
                print(\"unreachable\")
                3
            }

            test()
        "
            ),
            Primitive::Number(2.0)
        );
    }

    #[test]
    #[ignore] // 文式の型評価
    fn function_return_expression() {
        assert_eq!(
            VM::new().eval(
                "
            function test():number {
                return true && (return 1)
            }

            test()
        "
            ),
            Primitive::Number(1.0)
        );
    }

    #[test]
    fn for_loop() {
        assert_eq!(
            VM::new().eval(
                "
        let i = 100
        for (i in 0 to 10) {
            debug(i)
        }
        debug(i)
        i
        "
            ),
            Primitive::Number(100.0)
        );
    }

    #[test]
    fn for_loop_with_break_statement() {
        assert_eq!(
            VM::new().eval(
                "
        let x = 0
        for (i in -5 to +5) {
            x = x + i
            if (!bool(i)) {
                break
            }
        }
        x
        "
            ),
            Primitive::Number(-15.0)
        );
    }

    #[test]
    #[ignore] // 文式の型評価
    fn for_loop_with_break_expression() {
        assert_eq!(
            VM::new().eval(
                "
        let x = 0
        for (i in -5 to +5) {
            x = x + i
            if (!bool(i)) {
                x = 0 + break + print(\"unreachable\")
            }
        }
        x
        "
            ),
            Primitive::Number(-15.0)
        );
    }

    #[test]
    fn nested_for_loop_with_break_statement() {
        assert_eq!(
            VM::new().eval(
                "
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
        "
            ),
            Primitive::Number(-150.0)
        );
    }

    #[test]
    fn shadowing() {
        assert_eq!(
            VM::new().eval(
                "
        let x = 0
        {
            let x
            x = 100
            debug(x)
        }
        debug(x)
        x
        "
            ),
            Primitive::Number(0.0)
        );
    }

    #[test]
    fn use_variable_in_outer_scope() {
        assert_eq!(
            VM::new().eval(
                "
        let x = 0
        {
            x = 100
            debug(x)
        }
        debug(x)
        x
        "
            ),
            Primitive::Number(100.0)
        );
    }

    mod expression {
        use crate::value::Primitive;
        use crate::vm::VM;

        #[test]
        fn equal() {
            assert_eq!(VM::new().eval("1 == 1"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("1 == 2"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("true == true"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("true == false"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("\"a\" == \"a\""), Primitive::Bool(true));
            assert_eq!(VM::new().eval("\"a\" == \"b\""), Primitive::Bool(false));
        }

        #[test]
        fn not_equal() {
            assert_eq!(VM::new().eval("1 != 1"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 != 2"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("true != true"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("true != false"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("\"a\" != \"a\""), Primitive::Bool(false));
            assert_eq!(VM::new().eval("\"a\" != \"b\""), Primitive::Bool(true));
        }

        #[test]
        fn less_than() {
            assert_eq!(VM::new().eval("1 < 0"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 < 1"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 < 2"), Primitive::Bool(true));
            assert_eq!(
                VM::new().eval("true < false"),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is Bool".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"a\" < \"a\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"0.0\" < \"1.0\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(VM::new().eval("1 <= 0"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 <= 1"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("1 <= 2"), Primitive::Bool(true));
            assert_eq!(
                VM::new().eval("true < false"),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is Bool".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"a\" < \"a\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"0.0\" < \"1.0\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
        }

        #[test]
        fn greater_than() {
            assert_eq!(VM::new().eval("0 > 1"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 > 1"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("2 > 1"), Primitive::Bool(true));
            assert_eq!(
                VM::new().eval("true > false"),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is Bool".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"a\" > \"a\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"0.0\" > \"1.0\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(VM::new().eval("0 >= 1"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("1 >= 1"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("2 >= 1"), Primitive::Bool(true));
            assert_eq!(
                VM::new().eval("true >= false"),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is Bool".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"a\" >= \"a\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("\"0.0\" >= \"1.0\""),
                Primitive::String(
                    "TypeError: Expected type Number, but actual type is String".to_string()
                )
            );
        }
    }

    mod variable_declaration {
        use crate::vm::{Primitive, VM};

        #[test]
        fn declare_variable_with_type() {
            let mut vm = VM::new();
            assert_eq!(vm.eval("let x: number = 1"), Primitive::Null);

            let value = match vm.stack.get_variable("x") {
                Some(value) => value,
                _ => panic!("Expected variable"),
            };
            assert_eq!(value, &Primitive::Number(1.0));
        }
    }

    mod reference {
        use crate::value::Object;
        use crate::vm::{Primitive, VM};
        use std::collections::HashMap;

        #[test]
        fn set_and_get_object() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
                user
            "
                ),
                Primitive::Ref(1 + VM::get_builtin_object_count())
            );

            let mut properties = HashMap::new();
            properties.insert("id".to_string(), Primitive::Number(1.0));
            properties.insert("name".to_string(), Primitive::String("Alice".to_string()));

            match vm.heap.heap.get(&(1 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        fn create_nested_object() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let userRef = UserRef(user=User(id=1, name=\"Alice\"))
                userRef
            "
                ),
                Primitive::Ref(3 + VM::get_builtin_object_count())
            );

            let mut properties_user = HashMap::new();
            properties_user.insert("id".to_string(), Primitive::Number(1.0));
            properties_user.insert("name".to_string(), Primitive::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert(
                "user".to_string(),
                Primitive::Ref(2 + VM::get_builtin_object_count()),
            );

            match vm.heap.heap.get(&(3 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties_user_ref);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
        fn read_member_of_object() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct User { id: number, name: string }

                let user = User { id: 1, name: \"Alice\" }
            ",
            );
            assert_eq!(vm.eval("user.id"), Primitive::Number(1f64));
            assert_eq!(vm.eval("user.name"), Primitive::String("Alice".to_string()));
        }

        #[test]
        fn write_member_of_object() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
                user.id = 2
                user
            "
                ),
                Primitive::Ref(1 + VM::get_builtin_object_count())
            );

            let mut properties = HashMap::new();
            properties.insert("id".to_string(), Primitive::Number(2.0));
            properties.insert("name".to_string(), Primitive::String("Alice".to_string()));

            match vm.heap.heap.get(&(1 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        fn assign_ref_into_another_object() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
                userRef
            "
                ),
                Primitive::Ref(3 + VM::get_builtin_object_count())
            );

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Primitive::Number(1.0));
            expected_members_user1
                .insert("name".to_string(), Primitive::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert(
                "user".to_string(),
                Primitive::Ref(2 + VM::get_builtin_object_count()),
            );

            match vm.heap.heap.get(&(3 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties_user_ref);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        fn write_member_of_object_through_nested_ref() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
                userRef.user.id = 2
                userRef
            "
                ),
                Primitive::Ref(3 + VM::get_builtin_object_count())
            );

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Primitive::Number(2.0));
            expected_members_user1
                .insert("name".to_string(), Primitive::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert(
                "user".to_string(),
                Primitive::Ref(2 + VM::get_builtin_object_count()),
            );

            match vm.heap.heap.get(&(3 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties_user_ref);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        fn access_ref_after_ref_variable_is_overwritten() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)
                struct UserRef(user: User)

                let user1 = User(id=1, name=\"Alice\")
                let userRef = UserRef(user=user1)
                user1 = User(id=2, name=\"Bob\")
                userRef
            "
                ),
                Primitive::Ref(3 + VM::get_builtin_object_count())
            );

            let mut expected_members_user1 = HashMap::new();
            expected_members_user1.insert("id".to_string(), Primitive::Number(1.0));
            expected_members_user1
                .insert("name".to_string(), Primitive::String("Alice".to_string()));

            let mut properties_user_ref = HashMap::new();
            properties_user_ref.insert(
                "user".to_string(),
                Primitive::Ref(2 + VM::get_builtin_object_count()),
            );

            match vm.heap.heap.get(&(3 + VM::get_builtin_object_count())).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(struct_value.properties, properties_user_ref);
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        fn resolve_member_of_primitive() {
            assert_eq!(
                VM::new().eval(
                    "
                let obj = 1
                obj.id
            "
                ),
                Primitive::String("Expected struct type, but got Number".to_string())
            );
        }

        #[test]
        fn resolve_member_of_undefined_object() {
            assert_eq!(
                VM::new().eval(
                    "
                obj.id
            "
                ),
                Primitive::String("Undefined symbol: obj".to_string())
            );
        }
    }

    mod built_in_functions {
        use crate::vm::{Primitive, VM};

        #[test]
        fn number() {
            assert_eq!(VM::new().eval("number(1)"), Primitive::Number(1.0));
            assert_eq!(VM::new().eval("number(0)"), Primitive::Number(0.0));
            assert_eq!(VM::new().eval("number(true)"), Primitive::Number(1.0));
            assert_eq!(VM::new().eval("number(false)"), Primitive::Number(0.0));
            assert_eq!(VM::new().eval("number(\"123\")"), Primitive::Number(123.0));
            assert_eq!(VM::new().eval("number(\"NaN\")"), Primitive::Number(0.0));
        }

        #[test]
        fn bool() {
            assert_eq!(VM::new().eval("bool(1)"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("bool(0)"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("bool(100)"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("bool(true)"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("bool(false)"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("bool(\"true\")"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("bool(\"false\")"), Primitive::Bool(false));
            assert_eq!(VM::new().eval("bool(\"foo\")"), Primitive::Bool(true));
            assert_eq!(VM::new().eval("bool(\"FALSE\")"), Primitive::Bool(true));
        }

        #[test]
        fn string() {
            assert_eq!(
                VM::new().eval("string(1)"),
                Primitive::String("1".to_string())
            );
            assert_eq!(
                VM::new().eval("string(0.5)"),
                Primitive::String("0.5".to_string())
            );
            assert_eq!(
                VM::new().eval("string(true)"),
                Primitive::String("true".to_string())
            );
            assert_eq!(
                VM::new().eval("string(false)"),
                Primitive::String("false".to_string())
            );
            assert_eq!(
                VM::new().eval("string(\"ABC\")"),
                Primitive::String("ABC".to_string())
            );
            assert_eq!(
                VM::new().eval("string(\"\")"),
                Primitive::String("".to_string())
            );
        }

        #[test]
        fn print() {
            assert_eq!(
                VM::new().eval("print(1)"),
                Primitive::String(
                    "TypeError: Expected type String, but actual type is Number".to_string()
                )
            );
            assert_eq!(
                VM::new().eval("print(true)"),
                Primitive::String(
                    "TypeError: Expected type String, but actual type is Bool".to_string()
                )
            );
            assert_eq!(VM::new().eval("print(\"ABC\")"), Primitive::Number(0f64));
        }
    }

    mod gc {
        use crate::value::{Object, Primitive};
        use crate::vm::VM;

        #[test]
        fn clean_up_all_unused_refs() {
            let mut vm = VM::new();
            println!(
                "{:?}",
                vm.eval(
                    "
                struct User(name: string)
                let alice = User(name=\"Alice\")
            "
                )
            );
            assert_eq!(vm.heap.heap.len(), 2 + VM::get_builtin_object_count());

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 2 + VM::get_builtin_object_count());

            vm.eval("alice = User(name=\"Bob\");");
            vm.gc();
            assert_eq!(vm.heap.heap.len(), 2 + VM::get_builtin_object_count());
            // Alice should be released
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
        fn clean_up_objects_referred_by_another_object() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct User { name: string }
                struct Users { alice: User }
                let users = Users { alice: User { name: \"Alice\" } }
            ",
            );
            assert_eq!(vm.heap.heap.len(), 2);

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 2);

            vm.eval("users = Users { alice: User { name: \"Alice2\" } }");
            vm.gc();
            assert_eq!(vm.heap.heap.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        #[ignore] // 循環参照
        fn circular_reference() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct User { name: string, friend: User? }
                let alice = User { name: \"Alice\" }
                let bob = User { name: \"Bob\" }
                alice.friend = bob
                bob.friend = alice
            ",
            );

            assert_eq!(vm.heap.heap.len(), 2);

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 2);

            println!(
                "{:?}",
                vm.eval(
                    "
                alice = User { name: \"Alice\" }
                bob = alice
            "
                )
            );
            vm.gc();
            assert_eq!(vm.heap.heap.len(), 1); // New object assigned into alice and bob
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
        fn object_allocated_in_function() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct User { name: string }

                function createUser() {
                    let user = User { name: \"Alice\" }
                }
            ",
            );
            assert_eq!(vm.heap.heap.len(), 0);

            vm.eval("createUser()");
            assert_eq!(vm.heap.heap.len(), 1);

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 0);
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
        fn object_returned_from_function() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct User { name: string? }

                function createUser() {
                    return User { name: \"Alice\" }
                }
            ",
            );
            assert_eq!(vm.heap.heap.len(), 0);

            vm.eval("let user = createUser()");
            assert_eq!(vm.heap.heap.len(), 1);

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 1);

            vm.eval("user = {}");
            vm.gc();
            assert_eq!(vm.heap.heap.len(), 1); // New empty object assigned into alice and bob
        }

        #[test]
        #[ignore] // 型検査器の情報引き継ぎ
        fn reuse_released_address() {
            let mut vm = VM::new();
            vm.eval(
                "
                struct Obj { i: number }
                let a = Obj {i:0}
                let b = Obj {i:1}
             ",
            );
            assert_eq!(vm.heap.heap.len(), 2);

            vm.eval("a = Obj {i:2}; b = a");
            assert_eq!(vm.heap.heap.len(), 3);

            vm.gc(); // address 0 and 1 are released
            assert_eq!(vm.heap.heap.len(), 1);

            vm.eval("b = Obj {i:3}"); // address 1 is reused
            vm.eval("b = Obj {i:4}"); // address 2 is reused
            assert_eq!(vm.eval("a"), Primitive::Ref(2));

            match vm.heap.heap.get(&2).unwrap() {
                Object::StructInstance(struct_value) => {
                    assert_eq!(
                        struct_value.properties.get("i").unwrap(),
                        &Primitive::Number(2.0)
                    );
                }
                _ => panic!("Expected StructInstance"),
            }
        }

        #[test]
        #[ignore] // 不要な型オブジェクト・関数の解放
        fn gc_struct_declaration() {
            let mut vm = VM::new();
            vm.eval(
                "
                let a = {
                    struct Obj(i: number)
                    Obj(0)
                }
            ",
            );

            assert_eq!(vm.heap.heap.len(), 2 + VM::get_builtin_object_count());

            vm.gc();
            assert_eq!(vm.heap.heap.len(), 2 + VM::get_builtin_object_count());
        }
    }

    mod struct_declaration {
        use crate::value::Primitive;
        use crate::vm::VM;

        #[test]
        fn declare_struct() {
            let mut vm = VM::new();
            assert_eq!(
                vm.eval(
                    "
                struct User(id: number, name: string)

                let user = User(id=1, name=\"Alice\")
            "
                ),
                Primitive::Null
            );
        }
    }

    mod named_parameter {
        use crate::value::Primitive;
        use crate::vm::VM;

        #[test]
        fn function_call_with_named_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                function sub(x: number, y: number): number {
                    x - y
                }

                sub(y=1, x=2)
            "
                ),
                Primitive::Number(1.0)
            );
        }

        #[test]
        fn function_call_with_partially_named_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                function sub(x: number, y: number, z:number): number {
                    (x - y) / z
                }

                sub(y=1, 7, 2)
            "
                ),
                Primitive::Number(3.0)
            );
        }

        #[test]
        fn named_parameters_after_unnamed_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                function sub(x: number, y: number): number {
                    x - y
                }

                sub(1, x=2)
            "
                ),
                Primitive::Number(1.0)
            );
        }

        #[test]
        fn struct_initialization_with_named_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                struct User(id: number, age: number)

                let user1 = User(id=1, age=30)
                let user2 = User(age=40, id=2)

                user1.id - user2.id
            "
                ),
                Primitive::Number(-1.0)
            );
        }

        #[test]
        fn struct_initialization_with_partially_named_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                struct User(id: number, age: number)

                let user1 = User(1, age=30)
                let user2 = User(40, id=2)

                user1.id - user2.id
            "
                ),
                Primitive::Number(-1.0)
            );
        }

        #[test]
        fn too_many_parameters() {
            assert_eq!(
                VM::new().eval(
                    "
                function test(x: number): number { x + 1 }

                test(x=1, 1)
            "
                ),
                Primitive::String("Too many parameters".to_string())
            );
        }

        #[test]
        fn too_few_parameters1() {
            assert_eq!(
                VM::new().eval(
                    "
                function test(x: number, y: number): number { x + y }

                test(1)
            "
                ),
                Primitive::String(
                    "Too few parameters. Follow parameter(s) is not specified: y".to_string()
                )
            );
        }

        #[test]
        fn too_few_parameters2() {
            assert_eq!(
                VM::new().eval(
                    "
                function test(x: number, y: number): number { x + y }

                test(y=1)
            "
                ),
                Primitive::String(
                    "Too few parameters. Follow parameter(s) is not specified: x".to_string()
                )
            );
        }

        #[test]
        fn unknown_parameter() {
            assert_eq!(
                VM::new().eval(
                    "
                function test(x: number, y: number): number { x + y }

                test(z=1, 2, 3)
            "
                ),
                Primitive::String("Unknown parameter: z".to_string())
            );
        }
    }
}
