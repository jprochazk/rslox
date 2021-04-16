use std::fmt::{self, Display, Formatter};

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    Constant,
    Closure,
    Nil,
    True,
    False,

    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,

    Not,
    Equal,
    Less,
    Greater,

    Print,
    Pop,

    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,

    Call,

    JumpIfFalse,
    Jump,
    Loop,
    Return,

    Max = 254,
}

impl Opcode {
    #[inline]
    pub fn decode_unchecked(v: u8) -> Opcode {
        unsafe { ::std::mem::transmute(v) }
    }

    #[inline]
    pub fn decode(v: u8) -> Option<Opcode> {
        if v >= Opcode::Max as u8 {
            None
        } else {
            Some(Self::decode_unchecked(v))
        }
    }

    pub fn operands(&self) -> usize {
        use Opcode::*;
        match self {
            Constant => 1,
            Closure => 1,
            Nil => 0,
            True => 0,
            False => 0,
            Negate => 0,
            Add => 0,
            Subtract => 0,
            Multiply => 0,
            Divide => 0,
            Not => 0,
            Equal => 0,
            Less => 0,
            Greater => 0,
            Print => 0,
            Pop => 0,
            DefineGlobal => 1,
            GetGlobal => 1,
            SetGlobal => 1,
            GetLocal => 1,
            SetLocal => 1,
            GetUpvalue => 1,
            SetUpvalue => 1,
            Call => 1,
            JumpIfFalse => 2,
            Jump => 2,
            Loop => 2,
            Return => 0,
            Max => 0,
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
