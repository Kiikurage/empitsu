use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::ops::{ControlFlow, Deref};
use std::rc::Rc;

use crate::node::Node;
use crate::parser::parse;
use crate::punctuator_kind::PunctuatorKind;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    Function {
        name: String,
        parameters: Vec<String>,
        body: Box<Node>,
        closure: Rc<RefCell<Environment>>,
    },
}

impl Value {
    fn get_type(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Function { .. } => "function",
        }
    }

    fn as_number(&self) -> ControlFlow<BreakResult, f64> {
        match self {
            Value::Number(value) => ControlFlow::Continue(*value),
            _ => ControlFlow::Break(BreakResult::Error(Value::String(
                format!("TypeError: Expected type number, actual type {:?}", self.get_type())
            ))),
        }
    }

    fn as_bool(&self) -> ControlFlow<BreakResult, bool> {
        match self {
            Value::Bool(value) => ControlFlow::Continue(*value),
            _ => ControlFlow::Break(BreakResult::Error(Value::String(
                format!("TypeError: Expected type bool, actual type {:?}", self.get_type())
            ))),
        }
    }

    fn as_string(&self) -> ControlFlow<BreakResult, &String> {
        match self {
            Value::String(value) => ControlFlow::Continue(value),
            _ => ControlFlow::Break(BreakResult::Error(Value::String(
                format!("TypeError: Expected type string, actual type {:?}", self.get_type())
            ))),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Number(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Function { name, parameters, .. } => write!(f, "function {}({})", name, parameters.join(", ")),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Environment {
    variables: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn declare_variable(&mut self, name: &str, value: &Value) {
        self.variables.insert(name.to_string(), value.clone());
    }

    fn get_variable(&self, name: &str) -> ControlFlow<BreakResult, Value> {
        match self.variables.get(name) {
            Some(value) => ControlFlow::Continue(value.clone()),
            None => {
                match &self.parent {
                    Some(parent) => parent.borrow().get_variable(name),
                    None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined variable: {}", name)))),
                }
            }
        }
    }

    fn set_variable(&mut self, name: &str, value: &Value) -> ControlFlow<BreakResult, ()> {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), value.clone());
            ControlFlow::Continue(())
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().set_variable(name, value),
                None => ControlFlow::Break(BreakResult::Error(Value::String(format!("Undefined variable: {}", name)))),
            }
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

pub struct VM {
    environments: Vec<Rc<RefCell<Environment>>>,
}

enum BreakResult {
    Return(Value),
    Break(Value),
    Error(Value),
}

impl VM {
    pub fn new() -> Self {
        VM { environments: vec![Rc::new(RefCell::new(Environment::new()))] }
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
                let condition = self.eval_node(condition)?.as_bool()?;
                if condition {
                    self.eval_node(true_branch)
                } else {
                    match false_branch {
                        Some(false_branch) => self.eval_node(false_branch),
                        None => ControlFlow::Continue(Value::Number(0.0)),
                    }
                }
            }
            Node::BlockStatement(nodes) => {
                self.enter_new_environment();

                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = self.eval_node(node)?;
                }

                self.exit_environment();
                ControlFlow::Continue(ret)
            }
            Node::VariableDeclaration(name, value) => {
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
                let mut i = start.as_number()?;
                self.declare_variable(variable, &start);
                while i < end.as_number()? {
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
                let function = Value::Function {
                    name: name.clone(),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.environments.last().unwrap().clone(),
                };
                self.declare_variable(name, &function);

                ControlFlow::Continue(function)
            }
            Node::ReturnStatement(value) => {
                ControlFlow::Break(BreakResult::Return(
                    match value {
                        Some(value) => self.eval_node(value)?,
                        None => Value::Number(0.0),
                    }
                ))
            }
            Node::BreakStatement => ControlFlow::Break(BreakResult::Break(Value::Number(0.0))),

            // Expression
            Node::FunctionExpression(name, parameters, body) => {
                let function = Value::Function {
                    name: name.clone().unwrap_or("(anonymous)".to_string()),
                    parameters: parameters.clone(),
                    body: body.clone(),
                    closure: self.environments.last().unwrap().clone(),
                };
                ControlFlow::Continue(function)
            }
            Node::IfExpression(condition, true_branch, false_branch) => {
                let condition = self.eval_node(condition)?.as_bool()?;

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
            Node::AssignmentExpression(name, value) => {
                let value = self.eval_node(value)?;
                self.set_variable(name, &value)?;
                ControlFlow::Continue(value)
            }
            Node::BinaryExpression(left, operator, right) => {
                let left = self.eval_node(left)?;
                let right = self.eval_node(right)?;

                match operator {
                    PunctuatorKind::Plus => ControlFlow::Continue(Value::Number(left.as_number()? + right.as_number()?)),
                    PunctuatorKind::Minus => ControlFlow::Continue(Value::Number(left.as_number()? - right.as_number()?)),
                    PunctuatorKind::Multiply => ControlFlow::Continue(Value::Number(left.as_number()? * right.as_number()?)),
                    PunctuatorKind::Divide => ControlFlow::Continue(Value::Number(left.as_number()? / right.as_number()?)),
                    PunctuatorKind::LogicalOr => ControlFlow::Continue(Value::Bool(left.as_bool()? || right.as_bool()?)),
                    PunctuatorKind::LogicalAnd => ControlFlow::Continue(Value::Bool(left.as_bool()? && right.as_bool()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval_node(operand)?;

                match operator {
                    PunctuatorKind::Plus => ControlFlow::Continue(Value::Number(operand.as_number()?)),
                    PunctuatorKind::Minus => ControlFlow::Continue(Value::Number(-operand.as_number()?)),
                    PunctuatorKind::LogicalNot => ControlFlow::Continue(Value::Bool(!operand.as_bool()?)),
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Unexpected operator: {:?}", operator)))),
                }
            }
            Node::CallExpression(callee, arguments) => {
                match callee.deref() {
                    Node::Identifier(name) => {
                        match name.as_str() {
                            "number" => {
                                let val = self.eval_node(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => ControlFlow::Continue(Value::Number(value)),
                                    Value::Bool(value) => ControlFlow::Continue(Value::Number(if value { 1.0 } else { 0.0 })),
                                    Value::String(value) => {
                                        match value.parse::<f64>() {
                                            Ok(value) => ControlFlow::Continue(Value::Number(if value.is_nan() { 0.0 } else { value })),
                                            Err(_) => ControlFlow::Continue(Value::Number(0.0)),
                                        }
                                    }
                                    Value::Function { .. } => ControlFlow::Continue(Value::Number(0.0)),
                                }
                            }
                            "bool" => {
                                let val = self.eval_node(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => ControlFlow::Continue(Value::Bool(value != 0.0)),
                                    Value::Bool(value) => ControlFlow::Continue(Value::Bool(value)),
                                    Value::String(value) => ControlFlow::Continue(Value::Bool(value != "false")),
                                    Value::Function { .. } => ControlFlow::Continue(Value::Bool(true)),
                                }
                            }
                            "string" => {
                                let val = self.eval_node(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => ControlFlow::Continue(Value::String(value.to_string())),
                                    Value::Bool(value) => ControlFlow::Continue(Value::String(value.to_string())),
                                    Value::String(value) => ControlFlow::Continue(Value::String(value.clone())),
                                    Value::Function { name, parameters, .. } => ControlFlow::Continue(Value::String(format!("function {}({})", name, parameters.join(", ")))),
                                }
                            }
                            "print" => {
                                for argument in arguments {
                                    println!("{}", self.eval_node(argument)?.as_string()?);
                                }
                                ControlFlow::Continue(Value::Number(0.0))
                            }
                            "debug" => {
                                for argument in arguments {
                                    let value = self.eval_node(argument)?;
                                    match value {
                                        Value::Number(value) => println!("{}", value),
                                        Value::Bool(value) => println!("{}", value),
                                        Value::String(value) => println!("{}", value),
                                        Value::Function { name, parameters, .. } => println!("function {}({})", name, parameters.join(", ")),
                                    }
                                }
                                ControlFlow::Continue(Value::Number(0.0))
                            }
                            _ => {
                                match self.get_variable(name)? {
                                    Value::Function { parameters, body, closure, .. } => {
                                        let mut evaluated_arguments = vec![];
                                        for argument in arguments {
                                            evaluated_arguments.push(self.eval_node(argument)?);
                                        }

                                        let environment = Rc::new(RefCell::new(Environment {
                                            variables: HashMap::new(),
                                            parent: Some(closure),
                                        }));
                                        self.environments.push(environment);

                                        for (argument, parameter) in evaluated_arguments.iter().zip(parameters.iter()) {
                                            self.declare_variable(parameter, argument);
                                        }
                                        let ret = match self.eval_node(body.deref()) {
                                            ControlFlow::Continue(value) => value,
                                            ControlFlow::Break(BreakResult::Return(value)) => value,
                                            others => return others,
                                        };

                                        self.environments.pop();
                                        ControlFlow::Continue(ret)
                                    }
                                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("{} is not a function", name)))),
                                }
                            }
                        }
                    }
                    _ => ControlFlow::Break(BreakResult::Error(Value::String(format!("Failed to call {:?}", callee))))
                }
            }
            Node::Number(value) => ControlFlow::Continue(Value::Number(*value)),
            Node::Bool(value) => ControlFlow::Continue(Value::Bool(*value)),
            Node::String(value) => ControlFlow::Continue(Value::String(value.clone())),
            Node::Identifier(name) => self.get_variable(name),
            Node::ReturnExpression(value) => {
                ControlFlow::Break(BreakResult::Return(
                    match value {
                        Some(value) => self.eval_node(value)?,
                        None => Value::Number(0.0),
                    }
                ))
            }
            Node::BreakExpression => ControlFlow::Break(BreakResult::Break(Value::Number(0.0))),

            // tmp
            Node::RangeIterator(_start, _end) => {
                ControlFlow::Break(BreakResult::Break(Value::String("Unsupported".to_string())))
            }
        }
    }

    fn enter_new_environment(&mut self) {
        self.environments.push(Rc::new(RefCell::new(Environment {
            variables: HashMap::new(),
            parent: self.environments.last().map(Rc::clone),
        })));
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

    fn get_variable(&self, name: &str) -> ControlFlow<BreakResult, Value> {
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
        assert_eq!(VM::new().eval("false + 1"), Value::String("TypeError: Expected type number, actual type \"bool\"".to_string()));
        assert_eq!(VM::new().eval("\"hello\" + 1"), Value::String("TypeError: Expected type number, actual type \"string\"".to_string()));

        assert_eq!(VM::new().eval("\
        let x = 0;
        for (i in 0 to 10) {\
            debug(i);\
            x = x + i;\
        }\
        x\
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
        "), Value::String("TypeError: Expected type bool, actual type \"number\"".to_string()));
    }

    #[test]
    fn declare_function() {
        assert_eq!(VM::new().eval("
            function test (x) {
                print(string(x*2))
            }

            test(1)
            test(10)
            test(100)
        "), Value::Number(0.0));
    }

    #[test]
    fn closure1() {
        assert_eq!(VM::new().eval("
            let x = 0

            function setX (y) {
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
            function setX (y) {
                x = y
            }

            function wrapper () {
                let x = 100;
                setX(1)
                debug(x)
            }

            debug(x)
            wrapper()
            debug(x)
            x
        "), Value::Number(1.0));
    }

    #[test]
    fn function_object() {
        assert_eq!(VM::new().eval("
            let double = function double_fn_expression(x) {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn anonymous_function_object() {
        assert_eq!(VM::new().eval("
            let double = function (x) {
                x * 2
            }

            double(3)
        "), Value::Number(6.0));
    }

    #[test]
    fn function_object_cannot_be_referred_by_name() {
        assert_eq!(VM::new().eval("
            function f(x) { x * 10 }
            let double = function f(x) { x * 2 }

            f(3)
        "), Value::Number(30.0));
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
        let x = 0;
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
            assert_eq!(VM::new().eval("bool(\"Hoge\")"), Value::Bool(true));
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
            assert_eq!(VM::new().eval("print(1)"), Value::String("TypeError: Expected type string, actual type \"number\"".to_string()));
            assert_eq!(VM::new().eval("print(true)"), Value::String("TypeError: Expected type string, actual type \"bool\"".to_string()));
            assert_eq!(VM::new().eval("print(\"ABC\")"), Value::Number(0f64));
        }
    }
}