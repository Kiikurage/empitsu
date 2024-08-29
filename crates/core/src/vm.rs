use crate::bytecode::ByteCode;
use crate::code_generator::generate;
use crate::parser::parse;

pub struct VM {
    stack: Vec<f64>,
}

impl VM {
    pub fn new() -> Self {
        Self { stack: vec![] }
    }

    pub fn eval(&mut self, program: &str) -> f64{
        self.eval_codes(&generate(&parse(program).node));
        *self.stack.last().unwrap_or(&0.0)
    }

    fn eval_codes(&mut self, codes: &[ByteCode]) {
        let mut instruction_pointer = 0;

        while instruction_pointer < codes.len() {
            let code = &codes[instruction_pointer];

            match code {
                ByteCode::Constant(value) => self.stack.push(*value),
                ByteCode::Flush(index) => {
                    self.stack.truncate(*index);
                }
                ByteCode::Load(index) => self.stack.push(self.stack[*index]),
                ByteCode::Store(index) => {
                    self.stack[*index] = *self.stack.last().unwrap();
                }
                ByteCode::Jump(ip) => {
                    instruction_pointer = *ip;
                    continue;
                }
                ByteCode::JumpIfZero(ip) => {
                    if self.stack.pop().unwrap() == 0.0 {
                        instruction_pointer = *ip;
                        continue;
                    }
                }
                ByteCode::Add => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(lhs + rhs);
                }
                ByteCode::Subtract => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(lhs - rhs);
                }
                ByteCode::Multiply => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(lhs * rhs);
                }
                ByteCode::Divide => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(lhs / rhs);
                }
                ByteCode::LogicalAnd => {
                    let rhs = self.stack.pop().unwrap() == 1.0f64;
                    let lhs = self.stack.pop().unwrap() == 1.0f64;
                    self.stack.push(if lhs && rhs { 1.0 } else { 0.0 });
                }
                ByteCode::LogicalOr => {
                    let rhs = self.stack.pop().unwrap() == 1.0;
                    let lhs = self.stack.pop().unwrap() == 1.0;
                    self.stack.push(if lhs || rhs { 1.0 } else { 0.0 });
                }
                ByteCode::LessThan => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs < rhs { 1.0 } else { 0.0 });
                }
                ByteCode::LessThanOrEqual => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs <= rhs { 1.0 } else { 0.0 });
                }
                ByteCode::GreaterThan => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs > rhs { 1.0 } else { 0.0 });
                }
                ByteCode::GreaterThanEqual => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs >= rhs { 1.0 } else { 0.0 });
                }
                ByteCode::Equal => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs == rhs { 1.0 } else { 0.0 });
                }
                ByteCode::NotEqual => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(if lhs != rhs { 1.0 } else { 0.0 });
                }
                ByteCode::Negative => {
                    let operand = self.stack.pop().unwrap();
                    self.stack.push(-operand);
                }
                ByteCode::LogicalNot => {
                    let operand = self.stack.pop().unwrap() == 1.0;
                    self.stack.push(if !operand { 1.0 } else { 0.0 });
                }
            }

            instruction_pointer += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::code_generator::generate;
    use crate::parser::parse;
    use crate::vm::VM;

    fn test(program: &str, expected: &[f64]) {
        let mut vm = VM::new();
        let result = parse(program);
        assert_eq!(result.errors, vec![]);

        let codes = generate(&result.node);
        println!("{:?}", codes.len());

        vm.eval_codes(&codes);
        assert_eq!(vm.stack, Vec::from(expected));
    }

    #[test]
    fn push() {
        test("1", &[1.0]);
    }

    #[test]
    fn add() {
        test("1+2", &[3.0]);
    }

    #[test]
    fn sub() {
        test("3-2", &[1.0]);
    }

    #[test]
    fn mul() {
        test("3*2", &[6.0]);
    }

    #[test]
    fn div() {
        test("8/2", &[4.0]);
    }

    #[test]
    fn compound_binary_operation() {
        test("2*(3+4)/(1+6)", &[2.0]);
    }

    #[test]
    fn logical_and() {
        test("true && true", &[1.0]);
        test("true && false", &[0.0]);
        test("false && true", &[0.0]);
        test("false && false", &[0.0]);
    }

    #[test]
    fn logical_or() {
        test("true || true", &[1.0]);
        test("true || false", &[1.0]);
        test("false || true", &[1.0]);
        test("false || false", &[0.0]);
    }

    #[test]
    fn less_than() {
        test("1 < 2", &[1.0]);
        test("2 < 1", &[0.0]);
        test("1 < 1", &[0.0]);
    }

    #[test]
    fn less_than_or_equal() {
        test("1 <= 2", &[1.0]);
        test("2 <= 1", &[0.0]);
        test("1 <= 1", &[1.0]);
    }

    #[test]
    fn greater_than() {
        test("1 > 2", &[0.0]);
        test("2 > 1", &[1.0]);
        test("1 > 1", &[0.0]);
    }

    #[test]
    fn greater_than_or_equal() {
        test("1 >= 2", &[0.0]);
        test("2 >= 1", &[1.0]);
        test("1 >= 1", &[1.0]);
    }

    #[test]
    fn negative() {
        test("-1", &[-1.0]);
    }

    #[test]
    fn logical_not() {
        test("!true", &[0.0]);
        test("!false", &[1.0]);
    }

    #[test]
    fn block() {
        test("{1+1}", &[]);
        test("{1; { 1+1 }; 2+1 }", &[]);
    }

    #[test]
    fn declare_variable_with_initializer() {
        test("let x = 1", &[1.0]);
    }

    #[test]
    fn read_variable() {
        test("let x = 1; x", &[1.0, 1.0]);
    }

    #[test]
    fn read_variable_in_complex_expression() {
        test("let x = 1; x*(2+x)", &[1.0, 3.0]);
    }

    #[test]
    fn assignment() {
        test("let x=1; x=2", &[2.0, 2.0]);
    }

    #[test]
    fn assignment_in_expression() {
        test("let x=1; (x=2)*3", &[2.0, 6.0]);
    }

    #[test]
    fn declare_and_initialize_later() {
        test("let x; 1+2; x=10", &[3.0, 10.0]);
    }

    #[test]
    fn scope() {
        test("let x=1; { let x; x=2 }; x=3", &[3.0, 3.0]);
    }

    #[test]
    fn for_loop() {
        test("let x=0; for (i in range(0,5)) { x = x +i }; x = 3", &[3.0, 3.0]);
    }

    #[test]
    fn if_expression_true_branch() {
        test("let x=0; x = if(true) 10 else 20", &[10.0, 10.0]);
    }

    #[test]
    fn if_expression_false_branch() {
        test("let x=0; x = if(false) 10 else 20", &[20.0, 20.0]);
    }

    #[test]
    fn if_statement_true_branch() {
        test("let x = 0; if (true) { x = 1 } else { x = 2 }", &[1.0]);
    }

    #[test]
    fn if_statement_false_branch() {
        test("let x = 0; if (false) { x = 1 } else { x = 2 }", &[2.0]);
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
             &[106.0],
        );
    }
}