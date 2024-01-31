use super::{compiler::Program, opcodes::OpCode};

#[derive(Debug, Clone, Copy)]
pub enum StackEntry {
    ImmediateNumber(f64),
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
            Some(StackEntry::ImmediateNumber(number)) => number,
            None => panic!("Stack underflow"),
        }
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
                OpCode::Add => {
                    let left = self.get_imm_number();
                    let right = self.get_imm_number();
                    let result = left + right;
                    self.stack.push(StackEntry::ImmediateNumber(result));
                    self.ip += 1;
                }
                OpCode::Hlt => return,
                _ => todo!(),
            }
        }
    }
}
