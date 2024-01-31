#[derive(Debug)]
pub enum OpCode {
    PushImmediateNumber(f64),
    PushBoolean(bool),
    Add,
    Sub,
    Mul,
    Div,
    Hlt,
    And,
    Or,
    Eq,
    NEq,
    Lt,
    Lte,
    Gt,
    Gte,
}
