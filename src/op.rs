use std::fmt::{self, Display, Formatter};

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    Constant,
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
        use Opcode::*;
        write!(
            f,
            "{}",
            match self {
                Constant => "OP_CONSTANT",
                Nil => "OP_NIL",
                True => "OP_TRUE",
                False => "OP_FALSE",
                Negate => "OP_NEGATE",
                Add => "OP_ADD",
                Subtract => "OP_SUBTRACT",
                Multiply => "OP_MULTIPLY",
                Divide => "OP_DIVIDE",
                Not => "OP_NOT",
                Equal => "OP_EQUAL",
                Less => "OP_LESS",
                Greater => "OP_GREATER",
                Print => "OP_PRINT",
                Pop => "OP_POP",
                DefineGlobal => "OP_DEFINE_GLOBAL",
                GetGlobal => "OP_GET_GLOBAL",
                SetGlobal => "OP_SET_GLOBAL",
                GetLocal => "OP_GET_LOCAL",
                SetLocal => "OP_SET_LOCAL",
                Call => "OP_CALL",
                JumpIfFalse => "OP_JUMP_IF_FALSE",
                Jump => "OP_JUMP",
                Loop => "OP_LOOP",
                Return => "OP_RETURN",
                Max => "OP_MAX",
            }
        )
    }
}
