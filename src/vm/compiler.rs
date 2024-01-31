use crate::parse::BinaryOp;

use super::opcodes::OpCode;

pub struct Program(pub Vec<OpCode>);

pub struct Compiler {
    tape: Vec<OpCode>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { tape: Vec::new() }
    }

    pub fn finish(self) -> Program {
        let mut tape = self.tape;
        tape.push(OpCode::Hlt);
        Program(tape)
    }

    pub fn push_immediate_number(&mut self, value: f64) {
        self.tape.push(OpCode::PushImmediateNumber(value));
    }

    pub fn push_boolean(&mut self, value: bool) {
        self.tape.push(OpCode::PushBoolean(value));
    }

    pub fn binary_op(&mut self, op: &BinaryOp) {
        match op {
            BinaryOp::Plus => self.tape.push(OpCode::Add),
            BinaryOp::Times => self.tape.push(OpCode::Mul),
            BinaryOp::Minus => self.tape.push(OpCode::Sub),
            BinaryOp::Div => self.tape.push(OpCode::Div),
            BinaryOp::And => self.tape.push(OpCode::And),
            BinaryOp::Or => self.tape.push(OpCode::Or),
            BinaryOp::Equals => self.tape.push(OpCode::Eq),
            BinaryOp::NEquals => self.tape.push(OpCode::NEq),
            BinaryOp::Lt => self.tape.push(OpCode::Lt),
            BinaryOp::Lte => self.tape.push(OpCode::Lte),
            BinaryOp::Gt => self.tape.push(OpCode::Gt),
            BinaryOp::Gte => self.tape.push(OpCode::Gte),
        }
    }
}
