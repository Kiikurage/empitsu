use std::collections::HashMap;
use std::ops::Deref;

use crate::node::Node;
use crate::parser::parse;
use crate::punctuator_kind::PunctuatorKind;

struct Environment {
    variables: HashMap<String, f64>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, name: &str, value: f64) {
        self.variables.insert(name.to_string(), value);
    }

    fn get_variable(&self, name: &str) -> Option<&f64> {
        self.variables.get(name)
    }

    fn set_variable(&mut self, name: &str, value: f64) -> Result<(), String> {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), value);
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

    fn eval(&mut self, node: &Node) -> Result<f64, String> {
        match node {
            Node::Program(nodes) => {
                let mut ret = 0.0;
                for node in nodes {
                    ret = self.eval(node)?;
                }
                Ok(ret)
            }

            // Statement
            Node::EmptyStatement => Ok(0.0),
            Node::IfStatement(condition, true_branch, false_branch) => {
                let condition = self.eval(condition)?;
                if condition != 0.0 {
                    self.eval(true_branch)
                } else {
                    match false_branch {
                        Some(false_branch) => self.eval(false_branch),
                        None => Ok(0.0),
                    }
                }
            }
            Node::BlockStatement(nodes) => {
                self.enter_new_environment();

                let mut ret = 0.0;
                for node in nodes {
                    ret = self.eval(node)?;
                }

                self.exit_environment();
                Ok(ret)
            }
            Node::VariableDeclaration(name, value) => {
                let value = match value {
                    Some(value) => self.eval(value)?,
                    None => 0.0,
                };
                self.declare_variable(name, value);
                Ok(0.0)
            }
            Node::ForStatement(variable, iterator, body) => {
                self.enter_new_environment();

                let (start, end) = match iterator.deref() {
                    Node::RangeIterator(start, end) => {
                        (self.eval(start)?, self.eval(end)?)
                    }
                    _ => return Err("Unsupported iterator type".to_string())
                };
                let mut i = start;
                self.declare_variable(variable, start);
                while i < end {
                    self.set_variable(variable, i)?;
                    self.eval(body)?;
                    i += 1f64;
                }

                self.exit_environment();
                Ok(0.0)
            }

            // Expression
            Node::IfExpression(condition, true_branch, false_branch) => {
                let condition = self.eval(condition)?;

                if condition != 0.0 { self.eval(true_branch) } else { self.eval(false_branch) }
            }
            Node::BlockExpression(nodes) => {
                self.enter_new_environment();

                let mut ret = 0.0;
                for node in nodes {
                    ret = self.eval(node)?;
                }

                self.exit_environment();
                Ok(ret)
            }
            Node::AssignmentExpression(name, value) => {
                let value = self.eval(value)?;
                self.set_variable(name, value)?;
                Ok(value)
            }
            Node::AdditiveExpression(left, operator, right) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;

                match operator {
                    PunctuatorKind::Plus => Ok(left + right),
                    PunctuatorKind::Minus => Ok(left - right),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::MultiplicativeExpression(left, operator, right) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;

                match operator {
                    PunctuatorKind::Multiply => Ok(left * right),
                    PunctuatorKind::Divide => Ok(left / right),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                let operand = self.eval(operand)?;

                match operator {
                    PunctuatorKind::Plus => Ok(operand),
                    PunctuatorKind::Minus => Ok(-operand),
                    _ => Err(format!("Unexpected operator: {:?}", operator)),
                }
            }
            Node::CallExpression(callee, arguments) => {
                match callee.deref() {
                    Node::Identifier(name) => {
                        match name.as_str() {
                            "double" => {
                                let val = self.eval(arguments.first().unwrap())?;
                                Ok(val * 2f64)
                            }
                            "print" => {
                                for argument in arguments {
                                    let value = self.eval(argument)?;
                                    println!("{}", value);
                                }
                                Ok(0.0)
                            }
                            _ => Err(format!("Unknown function: {}", name))
                        }
                    }
                    _ => Err(format!("Failed to call {:?}", callee))
                }
            }
            Node::NumericLiteral(value) => Ok(*value),
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

    fn declare_variable(&mut self, name: &str, value: f64) {
        match self.environments.last_mut() {
            Some(environment) => environment.declare_variable(name, value),
            None => panic!("No environment"),
        }
    }

    fn get_variable(&self, name: &String) -> Result<f64, String> {
        for environment in self.environments.iter().rev() {
            if let Some(value) = environment.get_variable(name) {
                return Ok(*value);
            }
        }

        Err(format!("Undefined variable: {}", name))
    }

    fn set_variable(&mut self, name: &String, value: f64) -> Result<(), String> {
        for environment in self.environments.iter_mut().rev() {
            if environment.set_variable(name, value).is_ok() {
                return Ok(());
            }
        }

        Err(format!("Undefined variable: {}", name))
    }
}

pub fn eval(input: &str) -> Result<f64, String> {
    VM::new().eval(&parse(input)?)
}

#[cfg(test)]
mod tests {
    use super::eval;

    #[test]
    fn test_eval() {
        assert_eq!(eval("1+2").unwrap(), 3.0);
        assert_eq!(eval("1+2*\n3").unwrap(), 7.0);
        assert_eq!(eval("(1+2)*3").unwrap(), 9.0);
        assert_eq!(eval("print(1)").unwrap(), 0.0);
        assert_eq!(eval("double(1)+double(double(3))").unwrap(), 14.0);
        assert_eq!(eval("{1+2}").unwrap(), 3.0);
        assert_eq!(eval("if (1) 2 else 3").unwrap(), 2.0);
        assert_eq!(eval("if (double(3)) 2 else 3").unwrap(), 2.0);
        assert_eq!(eval("if (1) { print(123); 2 } else { 3 }").unwrap(), 2.0);
        assert_eq!(eval("if (1) 2 else 3*3").unwrap(), 2.0);
        assert_eq!(eval("1 + if (0) 2 else 3 * 3").unwrap(), 10.0);
        assert_eq!(eval("1 + if (0) 2 else if (0) 3 else 4").unwrap(), 5.0);
        assert_eq!(eval("let x; if (x) 1 else 2").unwrap(), 2.0);
        assert_eq!(eval("let x=1; if (x) 1 else 2").unwrap(), 1.0);
        assert_eq!(eval("let x=1; let y=if (x) 2 else 3; let z=y*y; z").unwrap(), 4.0);
        assert_eq!(eval("let x=1; x=2; x").unwrap(), 2.0);
        assert_eq!(eval("let x=5; x=x*x; x").unwrap(), 25.0);

        assert_eq!(eval("\
        let x = 0;
        for (i in 0 to 10) {\
            print(i);\
            x = x + i;\
        }\
        x\
        ").unwrap(), 45.0);
    }

    #[test]
    fn for_loop() {
        assert_eq!(eval("
        let i = 100
        for (i in 0 to 10) {
            print(i)
        }
        print(i)
        i
        ").unwrap(), 100.0);
    }

    #[test]
    fn shadowing() {
        assert_eq!(eval("
        let x = 0;
        {
            let x
            x = 100
            print(x)
        }
        print(x)
        x
        ").unwrap(), 0.0);
    }

    #[test]
    fn use_variable_in_outer_scope() {
        assert_eq!(eval("
        let x = 0
        {
            x = 100
            print(x)
        }
        print(x)
        x
        ").unwrap(), 100.0);
    }
}