use crate::analysis::type_::Type;
use crate::analysis::Analysis;
use crate::analyzer::analyze_program;
use crate::ast::get_range::GetRange;
use crate::ast::node::Node;
use crate::ast::program::Program;
use crate::error::Error;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use crate::util::AsU8Slice;
use crate::vm::bytecode::ByteCode;
use crate::vm::bytecode_like::{ByteCodeLike, Label, LabelId};
use std::collections::HashMap;
use std::ops::{Deref, Range};

impl Type {
    fn size(&self) -> usize {
        match self {
            Type::Number => size_of::<f64>(),
            Type::Bool => size_of::<bool>(),
            Type::Ref => size_of::<usize>(),
            Type::Void => 0,
            _ => unreachable!("Unsupported type: {:?}", self),
        }
    }
}

struct Scope {
    /// Range of the node that produces this stack frame
    range: Range<Position>,

    /// Maps of the position where variable is defined, to their stack offset in each scope
    stack_offset: usize,

    /// List of instruction pointer that requires to be patched with ip_end
    patch_ips: Vec<usize>,

    /// Label to jump when break statement is executed
    break_label_id: Option<LabelId>,
}

impl Scope {
    fn new(offset: usize, range: Range<Position>, break_label_id: Option<LabelId>) -> Self {
        Self {
            range,
            stack_offset: offset,
            patch_ips: vec![],
            break_label_id,
        }
    }
}

struct StackFrame {
    scopes: Vec<Scope>,
    variable_offset: HashMap<Range<Position>, usize>,
    stack_size: usize,
}

impl StackFrame {
    fn new() -> Self {
        Self {
            scopes: vec![],
            variable_offset: HashMap::new(),
            stack_size: 0,
        }
    }
}

pub struct Generator {
    labels: HashMap<usize, Label>,
    opcodes: Vec<ByteCodeLike>,
    frames: Vec<StackFrame>,
    literals: Vec<Vec<u8>>,
    analysis: Analysis,
}

///
/// - 4byte: number of literal in literal tables (N)
/// - 16byte * N: (offset: usize, length: usize) of each literal.
///               Offset is from the start of the literal table.
/// - Literal table: each literal binary
/// - op codes
///

impl Generator {
    pub fn generate(program: &Program) -> Result<Vec<u8>, Error> {
        let analysis = analyze_program(program);
        if !analysis.errors.is_empty() {
            return Err(analysis.errors.first().unwrap().clone());
        }

        let mut frame = StackFrame::new();
        frame.scopes.push(Scope::new(0, program.range(), None));
        let frames = vec![frame];

        let mut generator = Self {
            literals: vec![],
            opcodes: vec![],
            frames,
            analysis,
            labels: HashMap::new(),
        };

        generator.generate_program(program);
        Ok(generator.emit_codes())
    }

    fn generate_program(&mut self, program: &Program) {
        for statement in program.statements.iter() {
            self.generate_node(statement);
        }
    }

    fn generate_node(&mut self, node: &Node) {
        match node {
            Node::Program(program) => {
                self.generate_program(program);
            }
            Node::IfStatement(if_statement) => {
                let label_false_branch = self.create_label();

                self.generate_node(&if_statement.condition);
                self.jump_if_false(label_false_branch);

                let stack_size = self.get_current_frame().stack_size;
                self.generate_node(&if_statement.true_branch);

                match &if_statement.false_branch {
                    Some(false_branch) => {
                        let label_end = self.create_label();
                        self.jump(label_end);

                        self.insert_label(label_false_branch);
                        self.get_current_frame_mut().stack_size = stack_size;
                        self.generate_node(false_branch);

                        self.insert_label(label_end);
                    }
                    None => {
                        self.insert_label(label_false_branch);
                    }
                }
            }
            Node::ForStatement(for_) => {
                let label_end_for = self.create_label();
                let label_main_loop = self.create_label();

                self.enter_scope(for_.range(), Some(label_end_for));
                {
                    // initializer
                    self.push_constant_number(0.0);
                    self.allocate_variable_in_stack(&for_.variable.range());

                    // condition
                    self.insert_label(label_main_loop);

                    self.load(&for_.variable.range());
                    self.push_constant_number(5.0);
                    self.less_than();
                    self.jump_if_false(label_end_for);

                    // body
                    self.generate_node(&for_.body);

                    // update
                    self.load(&for_.variable.range());
                    self.push_constant_number(1.0);
                    self.add();
                    self.store(&for_.variable.range());

                    // loop
                    self.jump(label_main_loop);

                    self.insert_label(label_end_for);
                }
                self.exit_scope(self.get_current_scope().stack_offset);
            }
            Node::VariableDeclaration(ref variable_declaration) => {
                let variable = self.analysis
                    .get_variable_info(&variable_declaration.range()).unwrap();

                match &variable_declaration.initializer {
                    Some(initializer) => self.generate_node(initializer),
                    None => {
                        match variable.type_ {
                            Type::Number => self.push_constant_number(0.0),
                            Type::Bool => self.push_constant_bool(false),
                            _ => unreachable!("Expected primitive type"),
                        }
                    }
                }

                self.allocate_variable_in_stack(&variable_declaration.range());
            }
            Node::FunctionDeclaration(_function) => {
                // 関数本体
                // 戻り値をスタックフレームの一番下に配置
            }
            Node::StructDeclaration(_) => unreachable!("StructDeclaration"),
            Node::InterfaceDeclaration(_) => unreachable!("InterfaceDeclaration"),
            Node::ImplStatement(_) => unreachable!("ImplStatement"),
            Node::Return(_) => unreachable!("Return"),
            Node::Break(_break_) => {
                let label = self.get_current_loop_label();
                self.jump(label);
            }
            Node::FunctionExpression(_) => unreachable!("FunctionExpression"),
            Node::IfExpression(if_expression) => {
                let label_false_branch = self.create_label();
                let label_end_if = self.create_label();

                self.generate_node(&if_expression.condition);
                self.jump_if_false(label_false_branch);

                let stack_size = self.get_current_frame().stack_size;
                self.generate_node(&if_expression.true_branch);
                self.jump(label_end_if);

                self.insert_label(label_false_branch);
                self.get_current_frame_mut().stack_size = stack_size;
                self.generate_node(&if_expression.false_branch);
                self.insert_label(label_end_if)
            }
            Node::Block(block) => {
                self.enter_scope(block.range(), None);
                for node in block.nodes.iter() {
                    self.generate_node(node);
                }

                // Keep the last value in the stack
                let scope = self.get_current_scope();
                let stack_offset = scope.stack_offset;
                let type_ = self.analysis.get_expression_type(&block.range()).unwrap_or(&Type::Void);
                let flush_dest = stack_offset + type_.size();

                match type_ {
                    Type::Number => self.store_number(stack_offset),
                    Type::Bool => self.store_bool(stack_offset),
                    Type::Void => (),
                    _ => unreachable!("Expected primitive type"),
                }

                self.exit_scope(flush_dest);
            }
            Node::AssignmentExpression(expression) => {
                match &expression.lhs.deref() {
                    Node::Identifier(name) => {
                        self.generate_node(&expression.rhs);
                        self.store(&name.range())
                    }
                    _ => unreachable!("Assign to complex target is not supported")
                }
            }
            Node::BinaryExpression(expression) => {
                self.generate_node(&expression.lhs);
                self.generate_node(&expression.rhs);
                match &expression.operator {
                    PunctuationKind::Plus => self.add(),
                    PunctuationKind::Minus => self.subtract(),
                    PunctuationKind::Asterisk => self.multiply(),
                    PunctuationKind::Slash => self.divide(),
                    PunctuationKind::AndAnd => self.logical_and(),
                    PunctuationKind::VerticalLineVerticalLine => self.logical_or(),
                    PunctuationKind::LeftChevron => self.less_than(),
                    PunctuationKind::LeftChevronEqual => self.less_than_or_equal(),
                    PunctuationKind::RightChevron => self.greater_than(),
                    PunctuationKind::RightChevronEqual => self.greater_than_or_equal(),
                    PunctuationKind::EqualEqual => self.equal(),
                    PunctuationKind::ExclamationEqual => self.not_equal(),
                    _ => unreachable!("Unsupported binary operator: {:?}", expression.operator),
                }
            }
            Node::UnaryExpression(expression) => {
                self.generate_node(&expression.operand);
                match &expression.operator {
                    PunctuationKind::Plus => (),
                    PunctuationKind::Minus => self.negative(),
                    PunctuationKind::Exclamation => self.logical_not(),
                    _ => unreachable!("Unsupported unary operator: {:?}", expression.operator),
                }
            }
            Node::CallExpression(_expression) => {
                // Analyzerに必要な機能
                // - function type
                //      - 引数の順番を正規化したうえで、すべての引数及び戻り値が同じ変数は同じ型とみなす
                // - function declaration
                //      - 引数の順番を正規化する
                //
                // f(x:number, z:number=1, y:number=2)
                //
                // f(arg: { x:number, z:number, y:number })
                // f(arg: (number, number, number))
                //
                // f(x=3, y=true, z=4)  // O: 正規の順番、名前あり
                // f(3, true, 1)        // O: 正規の順番、名前なし
                // f(x=3, y=true, 1)    // X: 正規の順番、一部名前なし、名前なしが後に位置する
                // f(1, x=3)            // O: 正規の順番、一部名前なし、名前なしが前に位置する
                // f(x=3, z=4, y=true)  // O: 異なる順番、名前あり
                // f(3, 1, true)        // O: 異なる順番、名前なし (型エラー)
                // f(y=true, x=3, 1)    // X: 異なる順番、一部名前なし、名前なしが後に位置する
                // f(1, z=3, y=true)    // O: 異なる順番、一部名前なし、名前なしが前に位置する
                //
                // 名前付きは引数リストの後ろ側になければいけない
                //
                // 解析: - 呼び出し時に与えたパラメータを順番に解析する
                //   　　- 名前なし引数は、関数宣言の引数リストの順番に従って解析する

                // どうやってスタックに配置する順番を特定するか
                // - 関数宣言の引数の順番を正規化する

                // TODO: 期待されている引数の順番
                // let expected_parameter_names = vec![];
                //
                // for name in expected_parameter_names.iter() {
                //     expression.parameters.iter()
                //         .find(|parameter| parameter.name == *name)
                //     for parameter in .iter() {
                //         self.generate_node(parameter.value.deref());
                //     }
                // }

                // 残りをクリーンアップ
                // ipを呼び出し元へ戻す
            }
            Node::MemberExpression(_expression) => unreachable!("MemberExpression"),
            Node::Identifier(name) => {
                self.load(&name.range());
            }
            Node::NumberLiteral(value) => {
                self.push_constant_number(value.value);
            }
            Node::BoolLiteral(value) => {
                self.push_constant_bool(value.value);
            }
            Node::StringLiteral(string_literal) => {
                // unreachable!("TypeExpression")
                self.load_literal(string_literal.value.as_bytes().to_vec());
            }
            Node::TypeExpression(_) => unreachable!("TypeExpression"),
        }
    }

    // Push codes into buffer

    fn pop(&mut self, type_: Type) {
        self.get_current_frame_mut().stack_size -= type_.size();
    }

    fn push(&mut self, type_: Type) {
        self.get_current_frame_mut().stack_size += type_.size();
    }

    fn push_constant_number(&mut self, value: f64) {
        self.opcodes.push(ByteCodeLike::ConstantNumber(value));
        self.push(Type::Number);
    }

    fn push_constant_bool(&mut self, value: bool) {
        self.opcodes.push(ByteCodeLike::ConstantBool(value));
        self.push(Type::Bool);
    }

    fn store_number(&mut self, offset: usize) {
        self.opcodes.push(ByteCodeLike::StoreNumber(offset));
    }

    fn store_bool(&mut self, offset: usize) {
        self.opcodes.push(ByteCodeLike::StoreBool(offset));
    }

    fn store(&mut self, token_range: &Range<Position>) {
        let variable = self.analysis
            .get_variable_info(token_range).unwrap();

        let name = variable.name.clone();
        let range = variable.range();
        let type_ = variable.type_.clone();

        let frame = self.get_current_frame();
        if let Some(offset) = frame.variable_offset.get(&range) {
            let offset = *offset;
            match type_ {
                Type::Number => self.store_number(offset),
                Type::Bool => self.store_bool(offset),
                _ => unreachable!("Expected primitive type"),
            }
            return;
        }

        unreachable!("Variable {} is not declared", name)
    }

    fn load_number(&mut self, offset: usize) {
        self.opcodes.push(ByteCodeLike::LoadNumber(offset));
        self.push(Type::Number);
    }

    fn load_bool(&mut self, offset: usize) {
        self.opcodes.push(ByteCodeLike::LoadBool(offset));
        self.push(Type::Bool);
    }

    fn load_literal(&mut self, value: Vec<u8>) {
        // unimplemented!()
        let literal_id = self.literals.len() as u32;
        self.literals.push(value);
        self.opcodes.push(ByteCodeLike::LoadLiteral(literal_id));
        self.push(Type::Ref);
    }

    fn load(&mut self, token_range: &Range<Position>) {
        let variable = self.analysis
            .get_variable_info(token_range).unwrap();

        let range = variable.range();
        let type_ = variable.type_.clone();
        let name = variable.name.clone();

        if let Some(offset) = self.get_current_frame().variable_offset.get(&range) {
            let offset = *offset;
            match type_ {
                Type::Number => self.load_number(offset),
                Type::Bool => self.load_bool(offset),
                _ => unreachable!("Expected primitive type"),
            }
            return;
        }
        unreachable!("Variable {} is not declared", name);
    }

    fn jump(&mut self, label_id: LabelId) {
        self.opcodes.push(ByteCodeLike::Jump(label_id));
    }

    fn jump_if_false(&mut self, label_id: LabelId) {
        self.pop(Type::Bool);
        self.opcodes.push(ByteCodeLike::JumpIfFalse(label_id));
    }

    fn add(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Add);
        self.push(Type::Number);
    }

    fn subtract(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Subtract);
        self.push(Type::Number);
    }

    fn multiply(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Multiply);
        self.push(Type::Number);
    }

    fn divide(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Divide);
        self.push(Type::Number);
    }

    fn logical_and(&mut self) {
        self.pop(Type::Bool);
        self.pop(Type::Bool);
        self.opcodes.push(ByteCodeLike::LogicalAnd);
        self.push(Type::Bool);
    }

    fn logical_or(&mut self) {
        self.pop(Type::Bool);
        self.pop(Type::Bool);
        self.opcodes.push(ByteCodeLike::LogicalOr);
        self.push(Type::Bool);
    }

    fn less_than(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::LessThan);
        self.push(Type::Bool);
    }

    fn less_than_or_equal(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::LessThanOrEqual);
        self.push(Type::Bool);
    }

    fn greater_than(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::GreaterThan);
        self.push(Type::Bool);
    }

    fn greater_than_or_equal(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::GreaterThanOrEqual);
        self.push(Type::Bool);
    }

    fn equal(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Equal);
        self.push(Type::Bool);
    }

    fn not_equal(&mut self) {
        self.pop(Type::Number);
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::NotEqual);
        self.push(Type::Bool);
    }

    fn negative(&mut self) {
        self.pop(Type::Number);
        self.opcodes.push(ByteCodeLike::Negative);
        self.push(Type::Number);
    }

    fn logical_not(&mut self) {
        self.pop(Type::Bool);
        self.opcodes.push(ByteCodeLike::LogicalNot);
        self.push(Type::Bool);
    }

    fn create_label(&mut self) -> LabelId {
        let id = self.labels.len();
        let label = Label::new(id);
        self.labels.insert(id, label);
        id
    }

    fn insert_label(&mut self, label_id: LabelId) {
        self.opcodes.push(ByteCodeLike::Label(label_id));
    }

    /// Declare a new variable. Current stack top value is used as the initial value.
    fn allocate_variable_in_stack(&mut self, range: &Range<Position>) {
        let variable = self.analysis
            .get_variable_info(range).unwrap();
        let range = variable.range();
        let type_ = variable.type_.clone();

        let frame = self.get_current_frame_mut();
        frame.variable_offset.insert(range, frame.stack_size - type_.size());
    }

    fn get_current_frame(&self) -> &StackFrame {
        self.frames.last().unwrap()
    }

    fn get_current_frame_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().unwrap()
    }

    fn get_current_scope(&self) -> &Scope {
        self.get_current_frame().scopes.last().unwrap()
    }

    fn get_current_scope_mut(&mut self) -> &mut Scope {
        self.get_current_frame_mut().scopes.last_mut().unwrap()
    }

    fn get_current_loop_label(&self) -> LabelId {
        for scope in self.get_current_frame().scopes.iter().rev() {
            if let Some(label) = &scope.break_label_id {
                return *label;
            }
        }

        unreachable!("Break statement is not in loop")
    }

    fn enter_scope(&mut self, scope_range: Range<Position>, break_label_id: Option<LabelId>) {
        let frame = self.get_current_frame_mut();
        let new_scope = Scope::new(frame.stack_size, scope_range, break_label_id);
        frame.scopes.push(new_scope);
    }

    fn exit_scope(&mut self, expected_size: usize) {
        self.get_current_frame_mut().scopes.pop().unwrap();
        self.opcodes.push(ByteCodeLike::Flush(expected_size));
        self.get_current_frame_mut().stack_size = expected_size;
    }

    // Emit codes

    fn emit_codes(&mut self) -> Vec<u8> {
        let mut buffer = vec![];

        buffer.extend((self.literals.len() as u32).as_u8_slice());
        let mut offset = 0;

        for i in 0..self.literals.len() {
            let binary = &self.literals[i];

            buffer.extend(offset.as_u8_slice());
            buffer.extend(binary.len().as_u8_slice());

            offset += binary.len();
        }

        for i in 0..self.literals.len() {
            buffer.extend(&self.literals[i]);
        }

        let opcodes = self.opcodes.clone();
        let mut buffer_opcodes = vec![];
        for code in opcodes.iter() {
            match code {
                ByteCodeLike::ConstantNumber(value) => {
                    buffer_opcodes.extend_from_slice(ByteCode::ConstantNumber.as_u8_slice());
                    buffer_opcodes.extend_from_slice(value.as_u8_slice());
                }
                ByteCodeLike::ConstantBool(value) => {
                    buffer_opcodes.extend_from_slice(ByteCode::ConstantBool.as_u8_slice());
                    buffer_opcodes.extend_from_slice(value.as_u8_slice());
                }
                ByteCodeLike::LoadNumber(address) => {
                    buffer_opcodes.extend_from_slice(ByteCode::LoadNumber.as_u8_slice());
                    buffer_opcodes.extend_from_slice(address.as_u8_slice());
                }
                ByteCodeLike::LoadBool(address) => {
                    buffer_opcodes.extend_from_slice(ByteCode::LoadBool.as_u8_slice());
                    buffer_opcodes.extend_from_slice(address.as_u8_slice());
                }
                ByteCodeLike::StoreNumber(address) => {
                    buffer_opcodes.extend_from_slice(ByteCode::StoreNumber.as_u8_slice());
                    buffer_opcodes.extend_from_slice(address.as_u8_slice());
                }
                ByteCodeLike::StoreBool(address) => {
                    buffer_opcodes.extend_from_slice(ByteCode::StoreNumber.as_u8_slice());
                    buffer_opcodes.extend_from_slice(address.as_u8_slice());
                }
                ByteCodeLike::LoadLiteral(index) => {
                    buffer_opcodes.extend_from_slice(ByteCode::LoadLiteral.as_u8_slice());
                    buffer_opcodes.extend_from_slice(index.as_u8_slice());
                }
                ByteCodeLike::Jump(label_id) => {
                    buffer_opcodes.extend_from_slice(ByteCode::Jump.as_u8_slice());
                    self.emit_label(&mut buffer_opcodes, label_id);
                }
                ByteCodeLike::JumpIfFalse(label_id) => {
                    buffer_opcodes.extend_from_slice(ByteCode::JumpIfFalse.as_u8_slice());
                    self.emit_label(&mut buffer_opcodes, label_id);
                }
                ByteCodeLike::Flush(size) => {
                    buffer_opcodes.extend_from_slice(ByteCode::Flush.as_u8_slice());
                    buffer_opcodes.extend_from_slice(size.as_u8_slice());
                }
                ByteCodeLike::Add => {
                    buffer_opcodes.extend_from_slice(ByteCode::Add.as_u8_slice());
                }
                ByteCodeLike::Subtract => {
                    buffer_opcodes.extend_from_slice(ByteCode::Subtract.as_u8_slice());
                }
                ByteCodeLike::Multiply => {
                    buffer_opcodes.extend_from_slice(ByteCode::Multiply.as_u8_slice());
                }
                ByteCodeLike::Divide => {
                    buffer_opcodes.extend_from_slice(ByteCode::Divide.as_u8_slice());
                }
                ByteCodeLike::LogicalAnd => {
                    buffer_opcodes.extend_from_slice(ByteCode::LogicalAnd.as_u8_slice());
                }
                ByteCodeLike::LogicalOr => {
                    buffer_opcodes.extend_from_slice(ByteCode::LogicalOr.as_u8_slice());
                }
                ByteCodeLike::LessThan => {
                    buffer_opcodes.extend_from_slice(ByteCode::LessThan.as_u8_slice());
                }
                ByteCodeLike::LessThanOrEqual => {
                    buffer_opcodes.extend_from_slice(ByteCode::LessThanOrEqual.as_u8_slice());
                }
                ByteCodeLike::GreaterThan => {
                    buffer_opcodes.extend_from_slice(ByteCode::GreaterThan.as_u8_slice());
                }
                ByteCodeLike::GreaterThanOrEqual => {
                    buffer_opcodes.extend_from_slice(ByteCode::GreaterThanOrEqual.as_u8_slice());
                }
                ByteCodeLike::Equal => {
                    buffer_opcodes.extend_from_slice(ByteCode::Equal.as_u8_slice());
                }
                ByteCodeLike::NotEqual => {
                    buffer_opcodes.extend_from_slice(ByteCode::NotEqual.as_u8_slice());
                }
                ByteCodeLike::Negative => {
                    buffer_opcodes.extend_from_slice(ByteCode::Negative.as_u8_slice());
                }
                ByteCodeLike::LogicalNot => {
                    buffer_opcodes.extend_from_slice(ByteCode::LogicalNot.as_u8_slice());
                }
                ByteCodeLike::Label(label_id) => {
                    let address = buffer_opcodes.len();
                    let label = self.labels.get_mut(label_id).unwrap();
                    label.address = Some(address);
                    for pending in label.pending.iter() {
                        buffer_opcodes[*pending..*pending + size_of::<usize>()].copy_from_slice(address.as_u8_slice());
                    }
                }
            };
        }

        buffer.extend(&buffer_opcodes);
        buffer
    }

    fn emit_label(&mut self, buffer: &mut Vec<u8>, label_id: &LabelId) {
        let label = self.labels.get_mut(label_id).unwrap();
        match label.address {
            Some(address) => {
                buffer.extend_from_slice(address.as_u8_slice());
            }
            None => {
                label.pending.push(buffer.len());
                buffer.extend(/* dummy */ 0usize.as_u8_slice());
            }
        }
    }
}