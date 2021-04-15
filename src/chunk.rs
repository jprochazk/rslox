use crate::op::*;
use crate::value::{Object, Value};

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

    let mut nested = Vec::<(String, Chunk)>::new();
    let mut offset = 0;
    while offset < chunk.buffer.len() {
        offset += disassemble_instruction(chunk, offset, &mut nested);
    }
    for (name, chunk) in nested.iter() {
        disassemble_chunk(chunk, name);
    }
}

pub fn disassemble_instruction(
    chunk: &Chunk,
    offset: usize,
    nested: &mut Vec<(String, Chunk)>,
) -> usize {
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
        Constant => println!("\t{}", {
            let value = &chunk.constants[chunk.buffer[offset + 1] as usize];
            if let Value::Object(object) = value {
                if let Object::Function(func) = &(*object.borrow()) {
                    nested.push((func.name.clone(), func.chunk.clone()));
                }
            }
            value
        }),
        DefineGlobal => println!("\t{}", chunk.constants[chunk.buffer[offset + 1] as usize]),
        GetGlobal => println!("\t{}", chunk.constants[chunk.buffer[offset + 1] as usize]),
        SetGlobal => println!("\t{}", chunk.constants[chunk.buffer[offset + 1] as usize]),
        GetLocal => println!("\t[{}]", chunk.buffer[offset + 1]),
        SetLocal => println!("\t[{}]", chunk.buffer[offset + 1]),
        Call => println!("\t\t{}", chunk.constants[chunk.buffer[offset + 1] as usize]),
        JumpIfFalse => println!(
            "\t+{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        Jump => println!(
            "\t\t+{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        Loop => println!(
            "\t\t-{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        ),
        _ => println!(),
    };
    op.operands() + 1
}
