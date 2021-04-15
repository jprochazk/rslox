use crate::op::*;
use crate::value::Value;

#[derive(Clone)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
    pub buffer: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            constants: Vec::new(),
            lines: Vec::new(),
            buffer: Vec::new(),
        }
    }

    pub fn push_byte(&mut self, v: u8, line: usize) {
        self.buffer.push(v);
        self.lines.push(line);
    }
    pub fn push_bytes(&mut self, v: &[u8], line: usize) {
        self.buffer.extend_from_slice(v);
        for _ in 0..v.len() {
            self.lines.push(line);
        }
    }
    pub fn push_const(&mut self, v: Value) -> u8 {
        self.constants.push(v);
        (self.constants.len() - 1) as u8
    }
}

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.buffer.len() {
        offset += disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.lines[offset]);
    }

    let op = Opcode::decode_unchecked(chunk.buffer[offset]);
    print!("{}", op);
    use Opcode::*;
    match op {
        Constant => println!(
            " VALUE {}",
            chunk.constants[chunk.buffer[offset + 1] as usize]
        ),
        DefineGlobal => println!(
            " VALUE {}",
            chunk.constants[chunk.buffer[offset + 1] as usize]
        ),
        GetGlobal => println!(
            " NAME {}",
            chunk.constants[chunk.buffer[offset + 1] as usize]
        ),
        SetGlobal => println!(
            " NAME {}",
            chunk.constants[chunk.buffer[offset + 1] as usize]
        ),
        GetLocal => println!(" SLOT {}", chunk.buffer[offset + 1]),
        SetLocal => println!(" SLOT {}", chunk.buffer[offset + 1]),
        Call => println!(
            " ARGS {}",
            chunk.constants[chunk.buffer[offset + 1] as usize]
        ),
        JumpIfFalse => println!(
            " +{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        Jump => println!(
            " +{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        Loop => println!(
            " -{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        _ => println!(),
    };
    op.operands() + 1
}
