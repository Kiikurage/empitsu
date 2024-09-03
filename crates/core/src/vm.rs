pub mod bytecode;
pub mod generator;
mod bytecode_like;

use crate::error::Error;
use crate::parser::parse;
use crate::util::{AsU8Slice, ParseAs};
use crate::vm::bytecode::{ByteCode, EMBool, EMNumber};
use crate::vm::generator::Generator;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub struct EMObject {
    memory: Vec<u8>,
    type_: String,
}

pub struct VM {
    heap: HashMap<usize, EMObject>,
    stack: Vec<u8>,
    ip: usize,
    codes: Vec<u8>,
}

impl VM {
    pub fn new() -> Self {
        Self { heap: HashMap::new(), stack: vec![], ip: 0, codes: vec![] }
    }

    pub fn eval(&mut self, program: &str) -> Result<&f64, Error> {
        let parse_result = parse(program);
        if !parse_result.errors.is_empty() {
            return Err(parse_result.errors[0].clone());
        }

        let codes = Generator::generate(&parse_result.program)?;
        self.eval_codes(&codes)?;
        Ok(self.stack.parse_as::<EMNumber>(self.stack.len() - size_of::<EMNumber>()))
    }

    fn push<T: Debug>(&mut self, value: T) {
        self.stack.extend(value.as_u8_slice());
    }

    fn pop<T: Clone>(&mut self) -> T {
        let len = self.stack.len();
        // TODO: clone消したい
        let value = self.stack.parse_as::<T>(len - size_of::<T>()).clone();
        self.stack.truncate(len - size_of::<T>());

        value
    }

    fn read<T: Clone>(&mut self) -> &T {
        let value = self.codes.parse_as::<T>(self.ip);
        self.ip += size_of::<T>();
        value
    }

    fn read_stack<T>(&mut self, index: usize) -> &T {
        self.stack.parse_as::<T>(index)
    }

    fn eval_codes(&mut self, codes: &[u8]) -> Result<(), Error> {
        self.codes = codes.to_vec();
        self.ip = 0;

        let num_literals = *self.read::<u32>();
        let mut literal_index = Vec::new();
        for _ in 0..num_literals {
            let offset = *self.read::<usize>();
            let size = *self.read::<usize>();
            literal_index.push((offset, size));
        }

        let literal_table_offset = self.ip;
        let (last_literal_offset, last_literal_size) = literal_index.last().unwrap_or(&(0, 0));
        for (offset, size) in literal_index.iter() {
            let binary = codes[literal_table_offset + offset..literal_table_offset + offset + size]
                .to_vec();

            self.heap.insert(self.heap.len(), EMObject { memory: binary, type_: "Literal".to_string() });
        }

        let op_code_offset = literal_table_offset + last_literal_offset + last_literal_size;

        self.codes = self.codes[op_code_offset..].to_vec();
        self.ip = 0;

        self.eval_op_codes()?;

        Ok(())
    }

    fn eval_op_codes(&mut self) -> Result<(), Error> {
        while self.ip < self.codes.len() {
            let code = self.read::<ByteCode>();
            match code {
                ByteCode::ConstantNumber => {
                    let values = *self.read::<[u8; 8]>();
                    self.stack.extend(values);
                }
                ByteCode::ConstantBool => {
                    let values = *self.read::<[u8; 1]>();
                    self.stack.extend(values);
                }
                ByteCode::LoadNumber => {
                    let index = *self.read::<usize>();
                    let value = *self.read_stack::<EMNumber>(index);
                    self.stack.extend(value.as_u8_slice());
                }
                ByteCode::LoadBool => {
                    let index = *self.read::<usize>();
                    let value = *self.read_stack::<EMBool>(index);
                    self.stack.extend(value.as_u8_slice());
                }
                ByteCode::StoreNumber => {
                    let index = *self.read::<usize>();
                    let len = self.stack.len();
                    for i in 0..size_of::<EMNumber>() {
                        let value = self.stack[len - size_of::<EMNumber>() + i];
                        self.stack[index + i] = value;
                    }
                }
                ByteCode::StoreBool => {
                    let index = *self.read::<usize>();
                    let len = self.stack.len();
                    for i in 0..size_of::<EMBool>() {
                        let value = self.stack[len - size_of::<EMNumber>() + i];
                        self.stack[index + i] = value;
                    }
                }
                ByteCode::Jump => {
                    let ip = *self.read::<usize>();
                    self.ip = ip;
                }
                ByteCode::JumpIfFalse => {
                    let flag = self.pop::<EMBool>();
                    if flag {
                        // Skip the jump address
                        self.ip += size_of::<usize>();
                    } else {
                        let ip = *self.read::<usize>();
                        self.ip = ip;
                    }
                }
                ByteCode::LoadLiteral => {
                    let index = *self.read::<u32>() as usize;
                    let ref_ = index;
                    self.push(ref_);
                }
                ByteCode::Flush => {
                    let length = *self.read::<usize>();
                    self.stack.truncate(length);
                }
                ByteCode::Add => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs + rhs);
                }
                ByteCode::Subtract => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs - rhs);
                }
                ByteCode::Multiply => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs * rhs);
                }
                ByteCode::Divide => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs / rhs);
                }
                ByteCode::LogicalAnd => {
                    let rhs = self.pop::<EMBool>();
                    let lhs = self.pop::<EMBool>();
                    self.push(lhs && rhs);
                }
                ByteCode::LogicalOr => {
                    let rhs = self.pop::<EMBool>();
                    let lhs = self.pop::<EMBool>();
                    self.push(lhs || rhs);
                }
                ByteCode::LessThan => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs < rhs);
                }
                ByteCode::LessThanOrEqual => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs <= rhs);
                }
                ByteCode::GreaterThan => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs > rhs);
                }
                ByteCode::GreaterThanOrEqual => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs >= rhs);
                }
                ByteCode::Equal => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs == rhs);
                }
                ByteCode::NotEqual => {
                    let rhs = self.pop::<EMNumber>();
                    let lhs = self.pop::<EMNumber>();
                    self.push(lhs != rhs);
                }
                ByteCode::Negative => {
                    let operand = self.pop::<EMNumber>();
                    self.push(-operand);
                }
                ByteCode::LogicalNot => {
                    let operand = self.pop::<EMBool>();
                    self.push(!operand);
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::vm::Generator;
    use crate::vm::VM;
    use std::fmt::Debug;

    fn test<T: Clone + PartialEq + Debug>(program: &str, expected: T) {
        let mut vm = VM::new();
        let result = parse(program);
        assert_eq!(result.errors, vec![]);

        let codes = Generator::generate(&result.program).unwrap();

        vm.eval_codes(&codes).unwrap();

        assert_eq!(vm.pop::<T>(), expected);
    }

    #[test]
    fn constant_f64() {
        test("1", 1.0f64);
    }

    #[test]
    fn constant_true() {
        test("true", true);
    }

    #[test]
    fn constant_false() {
        test("false", false);
    }

    #[test]
    fn multiple_constants() {
        test("1; 2", 2.0f64);
    }

    #[test]
    fn add() {
        test("1+2", 3.0f64);
    }

    #[test]
    fn sub() {
        test("3-2", 1.0);
    }

    #[test]
    fn mul() {
        test("3*2", 6.0);
    }

    #[test]
    fn div() {
        test("8/2", 4.0);
    }

    #[test]
    fn compound_binary_operation() {
        test("2*(3+4)/(1+6)", 2.0);
    }

    #[test]
    fn logical_and() {
        test("true && true", true);
        test("true && false", false);
        test("false && true", false);
        test("false && false", false);
    }

    #[test]
    fn logical_or() {
        test("true || true", true);
        test("true || false", true);
        test("false || true", true);
        test("false || false", false);
    }

    #[test]
    fn less_than() {
        test("1 < 2", true);
        test("2 < 1", false);
        test("1 < 1", false);
    }

    #[test]
    fn less_than_or_equal() {
        test("1 <= 2", true);
        test("2 <= 1", false);
        test("1 <= 1", true);
    }

    #[test]
    fn greater_than() {
        test("1 > 2", false);
        test("2 > 1", true);
        test("1 > 1", false);
    }

    #[test]
    fn greater_than_or_equal() {
        test("1 >= 2", false);
        test("2 >= 1", true);
        test("1 >= 1", true);
    }

    #[test]
    fn negative() {
        test("-1", -1.0);
    }

    #[test]
    fn logical_not() {
        test("!true", false);
        test("!false", true);
    }

    #[test]
    fn block() {
        test("let x=1 {1+1}", 2.0);
    }

    #[test]
    fn block_scope() {
        test("let x=1 { let x = 2 { x = 10 } }", 10.0);
    }

    #[test]
    fn nested_block() {
        test("let x = 0 {1 { 1+1 } 2+1 }", 3.0);
    }

    #[test]
    fn declare_variable_with_initializer() {
        test("let x = 1", 1.0);
    }

    #[test]
    fn read_variable() {
        test("let x = 1; x", 1.0);
    }

    #[test]
    fn read_variable_in_complex_expression() {
        test("let x = 1; x*(2+x)", 3.0);
    }

    #[test]
    fn assignment() {
        test("let x=1; let y=2; x=3", 3.0);
    }

    #[test]
    fn assignment_in_expression() {
        test("let x=1; (x=2)*3", 6.0);
    }

    #[test]
    fn declare_and_initialize_later() {
        test("let x: number; 1+2; x=10", 10.0);
    }

    #[test]
    fn scope() {
        test("let x=1 { let x:number; x=2 } x=3", 3.0);
    }

    #[test]
    fn for_loop() {
        test("let x=0; for (i in range(0,5)) { x = x + i } x", 10.0);
    }

    #[test]
    fn if_expression_true_branch() {
        test("let x=0; x = if(true) 10 else 20", 10.0);
    }

    #[test]
    fn if_expression_false_branch() {
        test("let x=0; x = if(false) 10 else 20", 20.0);
    }

    #[test]
    fn uninitialized_variable() {
        test("let x:number; if (false) { x = 1 } else { x = 2 }", 2.0);
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
        test("let x = 0; if (true) { x = 1 } else { x = 2 }", 1.0);
    }

    #[test]
    fn if_statement_false_branch() {
        test("let x = 0; if (false) { x = 1 } else { x = 2 }", 2.0);
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
             106.0,
        );
    }

    #[test]
    fn string_literal() {
        let mut vm = VM::new();
        vm.eval(r#"
            "Hello"
            "World"
        "#).unwrap();

        assert_eq!(vm.heap.len(), 2);
        println!("heap: {:?}", vm.heap);
        println!("stack: {:?}", vm.stack);
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
"#, 3f64);
    }

    #[test]
    fn if_expression_must_return_a_value() {
        test(r#"
    let x = if (true) { 1 } else { 2 }
    x
"#, 1f64);
    }

    #[test]
    fn initialize_in_child_scope() {
        // TODO: 子スコープでの初期化情報を親スコープへ引き継ぐ
        test(r#"
    let x
    if (true) {
        x = 1
    } else {
        x = 2
    }
    x
"#, 1f64);
    }
}