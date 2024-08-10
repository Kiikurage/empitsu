use std::collections::HashMap;
use std::ops::Deref;

use crate::node::Node;
use crate::parser::parse;
use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Number(f64),
    Bool(bool),
    String(String),
}

impl Value {
    fn get_type(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
        }
    }

    fn as_number(&self) -> Result<f64, String> {
        match self {
            Value::Number(value) => Ok(*value),
            _ => Err(format!("TypeError: Expected type number, actual type {:?}", self.get_type())),
        }
    }

    fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(value) => Ok(*value),
            _ => Err(format!("TypeError: Expected type bool, actual type {:?}", self.get_type())),
        }
    }

    fn as_string(&self) -> Result<&String, String> {
        match self {
            Value::String(value) => Ok(value),
            _ => Err(format!("TypeError: Expected type string, actual type {:?}", self.get_type())),
        }
    }
}

struct Environment {
    variables: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, name: &str, value: &Value) {
        self.variables.insert(name.to_string(), value.clone());
    }

    fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn set_variable(&mut self, name: &str, value: &Value) -> Result<(), String> {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), value.clone());
            Ok(())
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }
}

pub struct VM {
    environments: Vec<Environment>,
}

impl VM {
    fn new() -> Self {
        VM { environments: vec![Environment::new()] }
    }

    fn eval(&mut self, node: &Node) -> Result<Value, String> {
        match node {
            Node::Program(nodes) => {
                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = self.eval(node)?;
                }
                Ok(ret)
            }

            // Statement
            Node::EmptyStatement => Ok(Value::Number(0.0)),
            Node::IfStatement(condition, true_branch, false_branch) => {
                let condition = self.eval(condition)?.as_bool()?;
                if condition {
                    self.eval(true_branch)
                } else {
                    match false_branch {
                        Some(false_branch) => self.eval(false_branch),
                        None => Ok(Value::Number(0.0)),
                    }
                }
            }
            Node::BlockStatement(nodes) => {
                self.enter_new_environment();

                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = self.eval(node)?;
                }

                self.exit_environment();
                Ok(ret)
            }
            Node::VariableDeclaration(name, value) => {
                let value = match value {
                    Some(value) => self.eval(value)?,
                    None => Value::Number(0.0),
                };
                self.declare_variable(name, &value);
                Ok(Value::Number(0.0))
            }
            Node::ForStatement(variable, iterator, body) => {
                self.enter_new_environment();

                let (start, end) = match iterator.deref() {
                    Node::RangeIterator(start, end) => {
                        (self.eval(start)?, self.eval(end)?)
                    }
                    _ => return Err("Unsupported iterator type".to_string())
                };
                let mut i = start.as_number()?;
                self.declare_variable(variable, &start);
                while i < end.as_number()? {
                    self.set_variable(variable, &Value::Number(i))?;
                    self.eval(body)?;
                    i += 1f64;
                }

                self.exit_environment();
                Ok(Value::Number(0.0))
            }

            // Expression
            Node::IfExpression(condition, true_branch, false_branch) => {
                let condition = self.eval(condition)?.as_bool()?;

                if condition { self.eval(true_branch) } else { self.eval(false_branch) }
            }
            Node::BlockExpression(nodes) => {
                self.enter_new_environment();

                let mut ret = Value::Number(0.0);
                for node in nodes {
                    ret = self.eval(node)?;
                }

                self.exit_environment();
                Ok(ret)
            }
            Node::AssignmentExpression(name, value) => {
                let value = self.eval(value)?;
                self.set_variable(name, &value)?;
                Ok(value)
            }
            Node::BinaryExpression(left, operator, right) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;

                match operator {
                    PunctuatorKind::Plus => Ok(Value::Number(left.as_number()? + right.as_number()?)),
                    PunctuatorKind::Minus => Ok(Value::Number(left.as_number()? - right.as_number()?)),
                    PunctuatorKind::Multiply => Ok(Value::Number(left.as_number()? * right.as_number()?)),
                    PunctuatorKind::Divide => Ok(Value::Number(left.as_number()? / right.as_number()?)),
                    PunctuatorKind::LogicalOr => Ok(Value::Bool(left.as_bool()? || right.as_bool()?)),
                    PunctuatorKind::LogicalAnd => Ok(Value::Bool(left.as_bool()? && right.as_bool()?)),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval(operand)?;

                match operator {
                    PunctuatorKind::Plus => Ok(Value::Number(operand.as_number()?)),
                    PunctuatorKind::Minus => Ok(Value::Number(-operand.as_number()?)),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::CallExpression(callee, arguments) => {
                match callee.deref() {
                    Node::Identifier(name) => {
                        match name.as_str() {
                            "number" => {
                                let val = self.eval(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => Ok(Value::Number(value)),
                                    Value::Bool(value) => Ok(Value::Number(if value { 1.0 } else { 0.0 })),
                                    Value::String(value) => {
                                        match value.parse::<f64>() {
                                            Ok(value) => Ok(Value::Number(if value.is_nan() { 0.0 } else { value })),
                                            Err(_) => Ok(Value::Number(0.0)),
                                        }
                                    }
                                }
                            }
                            "bool" => {
                                let val = self.eval(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => Ok(Value::Bool(value != 0.0)),
                                    Value::Bool(value) => Ok(Value::Bool(value)),
                                    Value::String(value) => Ok(Value::Bool(value != "false")),
                                }
                            }
                            "string" => {
                                let val = self.eval(arguments.first().unwrap())?;
                                match val {
                                    Value::Number(value) => Ok(Value::String(value.to_string())),
                                    Value::Bool(value) => Ok(Value::String(value.to_string())),
                                    Value::String(value) => Ok(Value::String(value.clone())),
                                }
                            }
                            "print" => {
                                for argument in arguments {
                                    println!("{}", self.eval(argument)?.as_string()?);
                                }
                                Ok(Value::Number(0.0))
                            }
                            "debug" => {
                                for argument in arguments {
                                    let value = self.eval(argument)?;
                                    match value {
                                        Value::Number(value) => println!("{}", value),
                                        Value::Bool(value) => println!("{}", value),
                                        Value::String(value) => println!("{}", value),
                                    }
                                }
                                Ok(Value::Number(0.0))
                            }
                            _ => Err(format!("Unknown function: {}", name))
                        }
                    }
                    _ => Err(format!("Failed to call {:?}", callee))
                }
            }
            Node::Number(value) => Ok(Value::Number(*value)),
            Node::Bool(value) => Ok(Value::Bool(*value)),
            Node::String(value) => Ok(Value::String(value.clone())),
            Node::Identifier(name) => self.get_variable(name),

            // tmp
            Node::RangeIterator(_start, _end) => {
                Err("Unsupported".to_string())
            }
        }
    }

    fn enter_new_environment(&mut self) {
        self.environments.push(Environment::new());
    }

    fn exit_environment(&mut self) {
        self.environments.pop();
    }

    fn declare_variable(&mut self, name: &str, value: &Value) {
        match self.environments.last_mut() {
            Some(environment) => environment.declare_variable(name, value),
            None => panic!("No environment"),
        }
    }

    fn get_variable(&self, name: &String) -> Result<Value, String> {
        for environment in self.environments.iter().rev() {
            if let Some(value) = environment.get_variable(name) {
                return Ok(value.clone());
            }
        }

        Err(format!("Undefined variable: {}", name))
    }

    fn set_variable(&mut self, name: &String, value: &Value) -> Result<(), String> {
        for environment in self.environments.iter_mut().rev() {
            if environment.set_variable(name, value).is_ok() {
                return Ok(());
            }
        }

        Err(format!("Undefined variable: {}", name))
    }
}

pub fn eval(input: &str) -> Result<Value, String> {
    VM::new().eval(&parse(input)?)
}

#[cfg(test)]
mod tests {
    use crate::eval::{eval, Value};

    #[test]
    fn test_eval() {
        assert_eq!(eval("1+2").unwrap(), Value::Number(3.0));
        assert_eq!(eval("1+2*\n3").unwrap(), Value::Number(7.0));
        assert_eq!(eval("(1+2)*3").unwrap(), Value::Number(9.0));
        assert_eq!(eval("debug(1)").unwrap(), Value::Number(0.0));
        assert_eq!(eval("number(1)+number(true)").unwrap(), Value::Number(2.0));
        assert_eq!(eval("{1+2}").unwrap(), Value::Number(3.0));
        assert_eq!(eval("if (true) 2 else 3").unwrap(), Value::Number(2.0));
        assert_eq!(eval("if (true) 2 else 3").unwrap(), Value::Number(2.0));
        assert_eq!(eval("if (true) { debug(123); 2 } else { 3 }").unwrap(), Value::Number(2.0));
        assert_eq!(eval("if (true) 2 else 3*3").unwrap(), Value::Number(2.0));
        assert_eq!(eval("1 + if (false) 2 else 3 * 3").unwrap(), Value::Number(10.0));
        assert_eq!(eval("1 + if (false) 2 else if (false) 3 else 4").unwrap(), Value::Number(5.0));
        assert_eq!(eval("let x=false; if (x) 1 else 2").unwrap(), Value::Number(2.0));
        assert_eq!(eval("let x=true; if (x) 1 else 2").unwrap(), Value::Number(1.0));
        assert_eq!(eval("let x=true; let y=if (x) 2 else 3; let z=y*y; z").unwrap(), Value::Number(4.0));
        assert_eq!(eval("let x=1; x=2; x").unwrap(), Value::Number(2.0));
        assert_eq!(eval("let x=5; x=x*x; x").unwrap(), Value::Number(25.0));
        assert_eq!(eval("true && true").unwrap(), Value::Bool(true));
        assert_eq!(eval("true && false").unwrap(), Value::Bool(false));
        assert_eq!(eval("false && true").unwrap(), Value::Bool(false));
        assert_eq!(eval("false && false").unwrap(), Value::Bool(false));
        assert_eq!(eval("true || true").unwrap(), Value::Bool(true));
        assert_eq!(eval("true || false").unwrap(), Value::Bool(true));
        assert_eq!(eval("false || true").unwrap(), Value::Bool(true));
        assert_eq!(eval("false || false").unwrap(), Value::Bool(false));
        assert_eq!(eval("false + 1"), Err("TypeError: Expected type number, actual type \"bool\"".to_string()));
        assert_eq!(eval("\"hello\" + 1"), Err("TypeError: Expected type number, actual type \"string\"".to_string()));

        assert_eq!(eval("\
        let x = 0;
        for (i in 0 to 10) {\
            debug(i);\
            x = x + i;\
        }\
        x\
        ").unwrap(), Value::Number(45.0));
    }

    #[test]
    fn if_statement() {
        assert_eq!(eval("
            if (0) {
                print(\"true\")
            } else {
                print(\"false\")
            }
        "), Err("TypeError: Expected type bool, actual type \"number\"".to_string()));
    }

    #[test]
    fn for_loop() {
        assert_eq!(eval("
        let i = 100
        for (i in 0 to 10) {
            debug(i)
        }
        debug(i)
        i
        ").unwrap(), Value::Number(100.0));
    }

    #[test]
    fn shadowing() {
        assert_eq!(eval("
        let x = 0;
        {
            let x
            x = 100
            debug(x)
        }
        debug(x)
        x
        ").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn use_variable_in_outer_scope() {
        assert_eq!(eval("
        let x = 0
        {
            x = 100
            debug(x)
        }
        debug(x)
        x
        ").unwrap(), Value::Number(100.0));
    }

    mod built_in_functions {
        use crate::eval::{eval, Value};

        #[test]
        fn number() {
            assert_eq!(eval("number(1)").unwrap(), Value::Number(1.0));
            assert_eq!(eval("number(0)").unwrap(), Value::Number(0.0));
            assert_eq!(eval("number(true)").unwrap(), Value::Number(1.0));
            assert_eq!(eval("number(false)").unwrap(), Value::Number(0.0));
            assert_eq!(eval("number(\"123\")").unwrap(), Value::Number(123.0));
            assert_eq!(eval("number(\"NaN\")").unwrap(), Value::Number(0.0));
        }

        #[test]
        fn bool() {
            assert_eq!(eval("bool(1)").unwrap(), Value::Bool(true));
            assert_eq!(eval("bool(0)").unwrap(), Value::Bool(false));
            assert_eq!(eval("bool(100)").unwrap(), Value::Bool(true));
            assert_eq!(eval("bool(true)").unwrap(), Value::Bool(true));
            assert_eq!(eval("bool(false)").unwrap(), Value::Bool(false));
            assert_eq!(eval("bool(\"true\")").unwrap(), Value::Bool(true));
            assert_eq!(eval("bool(\"false\")").unwrap(), Value::Bool(false));
            assert_eq!(eval("bool(\"Hoge\")").unwrap(), Value::Bool(true));
            assert_eq!(eval("bool(\"FALSE\")").unwrap(), Value::Bool(true));
        }

        #[test]
        fn string() {
            assert_eq!(eval("string(1)").unwrap(), Value::String("1".to_string()));
            assert_eq!(eval("string(0.5)").unwrap(), Value::String("0.5".to_string()));
            assert_eq!(eval("string(true)").unwrap(), Value::String("true".to_string()));
            assert_eq!(eval("string(false)").unwrap(), Value::String("false".to_string()));
            assert_eq!(eval("string(\"ABC\")").unwrap(), Value::String("ABC".to_string()));
            assert_eq!(eval("string(\"\")").unwrap(), Value::String("".to_string()));
        }

        #[test]
        fn print() {
            assert_eq!(eval("print(1)"), Err("TypeError: Expected type string, actual type \"number\"".to_string()));
            assert_eq!(eval("print(true)"), Err("TypeError: Expected type string, actual type \"bool\"".to_string()));
            assert_eq!(eval("print(\"ABC\")").unwrap(), Value::Number(0f64));
        }
    }
}