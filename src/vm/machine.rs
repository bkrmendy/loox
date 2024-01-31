use super::{compiler::Program, opcodes::OpCode};

#[derive(Debug, Clone, Copy)]
pub enum StackEntry {
    ImmediateNumber(f64),
    ImmediateBoolean(bool),
}

pub struct Machine {
    program: Vec<OpCode>,
    ip: usize,
    stack: Vec<StackEntry>,
}

impl Machine {
    pub fn new(program: Program) -> Self {
        Machine {
            ip: 0,
            program: program.0,
            stack: Vec::new(),
        }
    }

    pub fn peek_stack_top(&self) -> Option<&StackEntry> {
        self.stack.last()
    }

    fn get_imm_number(&mut self) -> f64 {
        match self.stack.pop() {
            Some(StackEntry::ImmediateNumber(value)) => value,
            Some(entry) => panic!("Unexpected stack entry: {:?}", entry),
            None => panic!("Stack underflow"),
        }
    }

    fn get_imm_boolean(&mut self) -> bool {
        match self.stack.pop() {
            Some(StackEntry::ImmediateBoolean(value)) => value,
            Some(entry) => panic!("Unexpected stack entry: {:?}", entry),
            None => panic!("Stack underflow"),
        }
    }

    fn do_binary_number_op(&mut self, op: fn(f64, f64) -> StackEntry) -> StackEntry {
        let left = self.get_imm_number();
        let right = self.get_imm_number();
        op(left, right)
    }

    fn do_binary_boolean_op(&mut self, op: fn(bool, bool) -> StackEntry) -> StackEntry {
        let left = self.get_imm_boolean();
        let right = self.get_imm_boolean();
        op(left, right)
    }

    pub fn run(&mut self) {
        loop {
            let opcode = self.program.get(self.ip);
            if opcode.is_none() {
                panic!("IP out of bounds")
            }

            match opcode.unwrap() {
                OpCode::PushImmediateNumber(number) => {
                    self.stack.push(StackEntry::ImmediateNumber(*number));
                    self.ip += 1;
                }
                OpCode::PushBoolean(boolean) => {
                    self.stack.push(StackEntry::ImmediateBoolean(*boolean));
                    self.ip += 1;
                }
                OpCode::Add => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateNumber(a + b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Sub => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateNumber(a - b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Mul => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateNumber(a * b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Div => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateNumber(a / b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::And => {
                    let result =
                        self.do_binary_boolean_op(|a, b| StackEntry::ImmediateBoolean(a && b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Or => {
                    let result =
                        self.do_binary_boolean_op(|a, b| StackEntry::ImmediateBoolean(a || b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Eq => {
                    let result =
                        match self.peek_stack_top() {
                            None => panic!("Stack underflow"),
                            Some(StackEntry::ImmediateBoolean(_)) => self
                                .do_binary_boolean_op(|a, b| StackEntry::ImmediateBoolean(a == b)),
                            Some(StackEntry::ImmediateNumber(_)) => self
                                .do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a == b)),
                        };
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::NEq => {
                    let result =
                        match self.peek_stack_top() {
                            None => panic!("Stack underflow"),
                            Some(StackEntry::ImmediateBoolean(_)) => self
                                .do_binary_boolean_op(|a, b| StackEntry::ImmediateBoolean(a != b)),
                            Some(StackEntry::ImmediateNumber(_)) => self
                                .do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a != b)),
                        };
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Lt => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a < b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Lte => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a <= b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Gt => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a > b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Gte => {
                    let result =
                        self.do_binary_number_op(|a, b| StackEntry::ImmediateBoolean(a >= b));
                    self.stack.push(result);
                    self.ip += 1;
                }
                OpCode::Hlt => return,
            }
        }
    }
}
