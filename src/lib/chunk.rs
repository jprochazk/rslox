use crate::op::*;
use crate::value::{Object, Value};

#[derive(Clone, Debug)]
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

pub fn disassemble_chunk(into: &mut impl std::fmt::Write, chunk: &Chunk, name: &str) -> Result<(), std::fmt::Error> {
    writeln!(into, "== {} ==", name)?;

    let mut nested = Vec::<(String, Chunk)>::new();
    let mut offset = 0;
    while offset < chunk.buffer.len() {
        offset += disassemble_instruction(into, chunk, offset, &mut nested)?;
    }
    for (name, chunk) in nested.iter() {
        disassemble_chunk(into, chunk, name)?;
    }
    Ok(())
}

pub fn disassemble_instruction(
    into: &mut impl std::fmt::Write,
    chunk: &Chunk,
    offset: usize,
    nested: &mut Vec<(String, Chunk)>,
) -> Result<usize, std::fmt::Error> {
    write!(into, "{:04} ", offset)?;
    if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
        write!(into, "   | ")?;
    } else {
        write!(into, "{:4} ", chunk.lines[offset])?;
    }

    let op = Opcode::decode_unchecked(chunk.buffer[offset]);
    write!(into, "{}", op)?;
    use Opcode::*;
    match op {
        Constant => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        Closure => {
            let value = &chunk.constants[chunk.buffer[offset + 1] as usize];
            writeln!(into, "\t{}", value)?;
            if let Value::Object(object) = value {
                if let Object::Function(func) = &(*object.borrow()) {
                    nested.push((func.name.clone(), func.chunk.clone()));

                    let upvalues = func.num_upvalues as usize;
                    let mut local_offset = offset + 1;
                    for _ in 0..upvalues {
                        let is_local = chunk.buffer[local_offset + 1];
                        let index = chunk.buffer[local_offset + 2];
                        write!(
                            into,
                            "{:04}    | \t\t{} {}",
                            local_offset + 1,
                            if is_local == 1 { "local" } else { "upvalue" },
                            index
                        )?;
                        local_offset += 2;
                    }

                    return Ok(op.operands() + 1 + upvalues as usize * 2);
                }
            }
        }
        Class => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        Method => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        DefineGlobal => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        GetGlobal => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        SetGlobal => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        GetLocal => writeln!(into, "\t[{}]", chunk.buffer[offset + 1])?,
        SetLocal => writeln!(into, "\t[{}]", chunk.buffer[offset + 1])?,
        GetUpvalue => writeln!(into, "\t[{}]", chunk.buffer[offset + 1])?,
        SetUpvalue => writeln!(into, "\t[{}]", chunk.buffer[offset + 1])?,
        GetProp => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        SetProp => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        GetSuper => writeln!(into, "\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        Call => writeln!(into, "\t\t{}", chunk.constants[chunk.buffer[offset + 1] as usize])?,
        JumpIfFalse => writeln!(
            into,
            "\t+{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        )?,
        Jump => writeln!(
            into,
            "\t\t+{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        )?,
        Loop => writeln!(
            into,
            "\t\t-{}",
            u16::from_ne_bytes([chunk.buffer[offset + 1], chunk.buffer[offset + 2]])
        )?,
        _ => writeln!(into)?,
    };
    Ok(op.operands() + 1)
}
