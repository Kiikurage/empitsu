pub mod bytecode;
pub mod generator;
mod bytecode_like;

use crate::error::Error;
use crate::parser::parse;
use crate::vm::bytecode::ByteCode;
use crate::vm::generator::Generator;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq)]
enum EMObject {
    String(EMString),
    UserFunction(EMUserFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EMString {
    value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EMUserFunction {
    body_ip: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Ref(usize),
}

impl Value {
    pub fn number(&self) -> f64 {
        match self {
            Value::Number(value) => *value,
            _ => panic!("Value is not a number"),
        }
    }

    pub fn bool(&self) -> bool {
        match self {
            Value::Bool(value) => *value,
            _ => panic!("Value is not a bool"),
        }
    }

    pub fn ref_(&self) -> usize {
        match self {
            Value::Ref(value) => *value,
            _ => panic!("Value is not a ref"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Ref(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct StackFrame {
    stack_offset: usize,
    return_ip: usize,
}

pub struct VM {
    heap: HashMap<usize, EMObject>,
    stack: Vec<Value>,
    call_stack: Vec<StackFrame>,
    ip: usize,
    codes: Vec<ByteCode>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            heap: HashMap::new(),
            stack: vec![],
            ip: 0,
            codes: vec![],
            call_stack: vec![],
        }
    }

    pub fn eval(&mut self, program: &str) -> Result<&Value, Error> {
        let parse_result = parse(program);
        if !parse_result.errors.is_empty() {
            return Err(parse_result.errors[0].clone());
        }

        let codes = Generator::generate(&parse_result.program)?;
        self.eval_codes(&codes)?;

        Ok(self.stack.last().unwrap())
    }

    fn eval_codes(&mut self, codes: &[ByteCode]) -> Result<(), Error> {
        self.codes = codes.to_vec();
        self.ip = 0;
        self.call_stack.clear();
        self.call_stack.push(StackFrame {
            stack_offset: 0,
            return_ip: 0,
        });

        self.eval_op_codes()?;

        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn truncate(&mut self, size: usize) {
        self.stack.truncate(size);
    }

    fn load(&mut self, index: usize) {
        let stack_frame = self.call_stack.last().unwrap();
        let value = self.stack[stack_frame.stack_offset + index].clone();
        self.push(value);
    }

    fn store(&mut self, index: usize) {
        let value = self.stack.last().unwrap().clone();
        let stack_frame = self.call_stack.last_mut().unwrap();
        self.stack[stack_frame.stack_offset + index] = value;
    }

    fn eval_op_codes(&mut self) -> Result<(), Error> {
        while self.ip < self.codes.len() {
            let code = &self.codes[self.ip];
            self.ip += 1;
            match code {
                ByteCode::Constant(value) => {
                    self.push(value.clone())
                }
                ByteCode::Load(index) => {
                    self.load(*index);
                }
                ByteCode::Store(index) => {
                    self.store(*index);
                }
                ByteCode::Jump(address) => {
                    self.ip = *address;
                }
                ByteCode::JumpIfFalse(address) => {
                    let address = *address;
                    let flag = self.pop().bool();
                    if !flag {
                        self.ip = address
                    }
                }
                ByteCode::DefineFunction(body_size) => {
                    let function = EMObject::UserFunction(EMUserFunction {
                        body_ip: self.ip
                    });
                    self.ip += body_size;
                    let ref_ = self.heap.len();
                    self.heap.insert(ref_, function);
                    self.push(Value::Ref(ref_));
                }
                ByteCode::Call(stack_address_of_fn_address) => {
                    let stack_offset = self.call_stack.last().unwrap().stack_offset;
                    let fn_address = self.stack[stack_offset + stack_address_of_fn_address].ref_();
                    let function = match self.heap.get(&fn_address).unwrap() {
                        EMObject::UserFunction(function) => function,
                        _ => panic!("Not a function"),
                    };

                    self.call_stack.push(StackFrame {
                        stack_offset: stack_offset + stack_address_of_fn_address + 1,
                        return_ip: self.ip,
                    });
                    self.ip = function.body_ip;
                }
                ByteCode::PopCallStack => {
                    let frame = self.call_stack.pop().unwrap();
                    let ret_value = if self.stack.len() > frame.stack_offset {
                        Some(self.stack.last().unwrap().clone())
                    } else {
                        None
                    };

                    self.ip = frame.return_ip;
                    self.truncate(frame.stack_offset - 1); // -1 for function address

                    if let Some(ret_value) = ret_value {
                        self.push(ret_value.clone());
                    }
                }
                ByteCode::Flush(size) => {
                    self.truncate(*size);
                }
                ByteCode::Add => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Number(lhs + rhs));
                }
                ByteCode::Subtract => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Number(lhs - rhs));
                }
                ByteCode::Multiply => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Number(lhs * rhs));
                }
                ByteCode::Divide => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Number(lhs / rhs));
                }
                ByteCode::LogicalAnd => {
                    let rhs = self.pop().bool();
                    let lhs = self.pop().bool();
                    self.push(Value::Bool(lhs && rhs));
                }
                ByteCode::LogicalOr => {
                    let rhs = self.pop().bool();
                    let lhs = self.pop().bool();
                    self.push(Value::Bool(lhs || rhs));
                }
                ByteCode::LessThan => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs < rhs));
                }
                ByteCode::LessThanOrEqual => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs <= rhs));
                }
                ByteCode::GreaterThan => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs > rhs));
                }
                ByteCode::GreaterThanOrEqual => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs >= rhs));
                }
                ByteCode::Equal => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs == rhs));
                }
                ByteCode::NotEqual => {
                    let rhs = self.pop().number();
                    let lhs = self.pop().number();
                    self.push(Value::Bool(lhs != rhs));
                }
                ByteCode::Negative => {
                    let operand = self.pop().number();
                    self.push(Value::Number(-operand));
                }
                ByteCode::LogicalNot => {
                    let operand = self.pop().bool();
                    self.push(Value::Bool(!operand));
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::vm::VM;
    use crate::vm::{Generator, Value};

    fn test(program: &str, expected: Value) {
        let mut vm = VM::new();
        let result = parse(program);
        assert_eq!(result.errors, vec![]);

        let codes = Generator::generate(&result.program).unwrap();

        vm.eval_codes(&codes).unwrap();

        assert_eq!(vm.stack.pop().unwrap(), expected);
    }

    #[test]
    fn constant_f64() {
        test("1", Value::Number(1.0f64));
    }

    #[test]
    fn constant_true() {
        test("true", Value::Bool(true));
    }

    #[test]
    fn constant_false() {
        test("false", Value::Bool(false));
    }

    #[test]
    fn multiple_constants() {
        test("1; 2", Value::Number(2.0f64));
    }

    #[test]
    fn add() {
        test("1+2", Value::Number(3.0f64));
    }

    #[test]
    fn sub() {
        test("3-2", Value::Number(1.0));
    }

    #[test]
    fn mul() {
        test("3*2", Value::Number(6.0));
    }

    #[test]
    fn div() {
        test("8/2", Value::Number(4.0));
    }

    #[test]
    fn compound_binary_operation() {
        test("2*(3+4)/(1+6)", Value::Number(2.0));
    }

    #[test]
    fn logical_and() {
        test("true && true", Value::Bool(true));
        test("true && false", Value::Bool(false));
        test("false && true", Value::Bool(false));
        test("false && false", Value::Bool(false));
    }

    #[test]
    fn logical_or() {
        test("true || true", Value::Bool(true));
        test("true || false", Value::Bool(true));
        test("false || true", Value::Bool(true));
        test("false || false", Value::Bool(false));
    }

    #[test]
    fn less_than() {
        test("1 < 2", Value::Bool(true));
        test("2 < 1", Value::Bool(false));
        test("1 < 1", Value::Bool(false));
    }

    #[test]
    fn less_than_or_equal() {
        test("1 <= 2", Value::Bool(true));
        test("2 <= 1", Value::Bool(false));
        test("1 <= 1", Value::Bool(true));
    }

    #[test]
    fn greater_than() {
        test("1 > 2", Value::Bool(false));
        test("2 > 1", Value::Bool(true));
        test("1 > 1", Value::Bool(false));
    }

    #[test]
    fn greater_than_or_equal() {
        test("1 >= 2", Value::Bool(false));
        test("2 >= 1", Value::Bool(true));
        test("1 >= 1", Value::Bool(true));
    }

    #[test]
    fn negative() {
        test("-1", Value::Number(-1.0));
    }

    #[test]
    fn logical_not() {
        test("!true", Value::Bool(false));
        test("!false", Value::Bool(true));
    }

    #[test]
    fn block() {
        test("let x=1 {1+1}", Value::Number(2.0));
    }

    #[test]
    fn block_scope() {
        test("let x=1 { let x = 2 { x = 10 } }", Value::Number(10.0));
    }

    #[test]
    fn nested_block() {
        test("let x = 0 {1 { 1+1 } 2+1 }", Value::Number(3.0));
    }

    #[test]
    fn declare_variable_with_initializer() {
        test("let x = 1", Value::Number(1.0));
    }

    #[test]
    fn read_variable() {
        test("let x = 1; x", Value::Number(1.0));
    }

    #[test]
    fn read_variable_in_complex_expression() {
        test("let x = 1; x*(2+x)", Value::Number(3.0));
    }

    #[test]
    fn assignment() {
        test("let x=1; let y=2; x=3", Value::Number(3.0));
    }

    #[test]
    fn assignment_in_expression() {
        test("let x=1; (x=2)*3", Value::Number(6.0));
    }

    #[test]
    fn declare_and_initialize_later() {
        test("let x: number; 1+2; x=10", Value::Number(10.0));
    }

    #[test]
    fn scope() {
        test("let x=1 { let x:number; x=2 } x=3", Value::Number(3.0));
    }

    #[test]
    fn for_loop() {
        test("let x=0; for (i in range(0,5)) { x = x + i } x", Value::Number(10.0));
    }

    #[test]
    fn if_expression_true_branch() {
        test("let x=0; x = if(true) 10 else 20", Value::Number(10.0));
    }

    #[test]
    fn if_expression_false_branch() {
        test("let x=0; x = if(false) 10 else 20", Value::Number(20.0));
    }

    #[test]
    fn detect_reading_uninitialized_variable() {
        VM::new()
            .eval("let x:number; let y = x")
            .expect_err("Reading uninitialized variable should raise an error");
    }

    #[test]
    fn detect_reading_uninitialized_variable_in_if_branch() {
        VM::new().eval(r#"
            let x:number
            let y = 0
            if (false) {
                x = 1
            } else {
                y = x
            }
        "#).expect_err("Reading uninitialized variable should raise an error");
    }

    #[test]
    fn detect_reading_uninitialized_variable_in_for_loop() {
        VM::new().eval(r#"
            let x: number
            let y: number

            for (i in range) {
                y = x
            }

            y
        "#).expect_err("Reading uninitialized variable should raise an error");
    }

    #[test]
    fn if_statement_true_branch() {
        test("let x = 0; if (true) { x = 1 } else { x = 2 }", Value::Number(1.0));
    }

    #[test]
    fn if_statement_false_branch() {
        test("let x = 0; if (false) { x = 1 } else { x = 2 }", Value::Number(2.0));
    }

    #[test]
    fn for_break() {
        test(r#"
                let x = 100;
                for (i in range(0, 5)) {
                    x = x + i;
                    if (i == 3) {
                        break
                    }
                }
            "#,
             Value::Number(106.0),
        );
    }

    #[test]
    fn block_expression_must_return_a_value() {
        test(r#"
    let x = {
        1
        2
        3
    }
    x
"#, Value::Number(3f64));
    }

    #[test]
    fn if_expression_must_return_a_value() {
        test(r#"
    let x = if (true) { 1 } else { 2 }
    x
"#, Value::Number(1f64));
    }

    #[test]
    fn initialize_in_child_scope() {
        test(r#"
    let x
    if (true) {
        x = 1
    } else {
        x = 2
    }
    x
"#, Value::Number(1f64));
    }

    #[test]
    fn redeclare_variable() {
        test(r#"
    let x = true;
    let x = 123;
    x
"#, Value::Number(123f64));
    }

    #[test]
    fn redeclare_variable_with_initializing_with_old_value() {
        test(r#"
    let x = 123;
    let x = x + 100;
    x
"#, Value::Number(223f64));
    }

    #[cfg(test)]
    mod function {
        use crate::vm::tests::test;
        use crate::vm::Value;

        #[test]
        fn call_static_function() {
            test(r#"
                fn double(x: number): number { x*2 }

                double(5)
            "#, Value::Number(10f64));
        }

        #[test]
        fn call_function_object() {
            test(r#"
                let double = fn(x: number): number { x*2 }

                double(5)
            "#, Value::Number(10f64));
        }

        #[test]
        fn call_function_twice() {
            test(r#"
                let double = fn(x: number): number { x*2 }
                double(double(5))
            "#, Value::Number(20f64));
        }

        #[test]
        fn call_function_immediately() {
            test(r#"
                (fn(x: number): number { x*2 })(5)
            "#, Value::Number(10f64));
        }


        #[test]
        fn call_inner_function() {
            test(r#"
                fn triple_double(x: number): number {
                    fn double(x: number): number { x*2 }

                    double(x*3)
                }

                triple_double(5)
            "#, Value::Number(30f64));
        }
    }
}