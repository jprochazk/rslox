use std::{
    fmt::{self, Display, Formatter},
    ops::{Deref, DerefMut},
};

use crate::{
    chunk::disassemble_instruction,
    op::Opcode,
    stack::Stack,
    value::{Closure, Function, NativeFn, NativeFnPtr, Object, Table, Value},
};

use thiserror::Error;

#[derive(Clone, Debug, Error)]
#[error("{0}")]
pub struct Error(String);
pub type Result<T> = std::result::Result<T, Error>;

struct StackTrace<'a>(&'a Stack<CallFrame>);
impl<'a> Display for StackTrace<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let count = self.0.len() - 1;
        for (i, frame) in self.0.iter().enumerate() {
            if !frame.func().name.is_empty() {
                write!(
                    f,
                    "\tat line {} (inside {}){}",
                    frame.func().chunk.lines[frame.ip - 1],
                    frame.func().name,
                    if i != count { "\n" } else { "" }
                )?;
            }
        }

        Ok(())
    }
}

macro_rules! error {
    ($stack:expr, $($arg:tt)*) => {{
        use std::fmt::Write;
        let mut msg = format!($($arg)*);
        if $stack.len() > 1 {
            write!(msg, "\n{}", StackTrace($stack)).unwrap();
        }
        Error(msg)
    }}
}

pub struct Vm {
    pub frames: Stack<CallFrame>,
    pub stack: Stack<Value>,
    pub globals: Table,
}

const FRAMES_MAX: usize = 64;

#[derive(Debug)]
pub struct CallFrame {
    pub ip: usize,
    closure: *const Closure,
    // HACK: to sidestep lifetime constraints caused by partial mutable borrow of vm state
    slots: *mut Stack<Value>,
    stack_top: usize,
    pop_n: usize,
}

impl CallFrame {
    pub fn new(slots: *mut Stack<Value>, stack_top: usize, closure: *const Closure) -> CallFrame {
        CallFrame {
            ip: 0,
            closure,
            slots,
            stack_top,
            pop_n: unsafe { ((*closure).func().arity + 1) as usize },
        }
    }

    #[inline]
    fn closure(&self) -> &Closure {
        unsafe { &(*self.closure) }
    }

    #[inline]
    fn func(&self) -> &Function {
        self.closure().func()
    }

    #[inline]
    fn stack(&mut self) -> &mut Stack<Value> {
        unsafe { &mut *self.slots }
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        let v = self.func().chunk.buffer[(self.ip - 1)];
        v
    }

    #[inline]
    fn read_bytes(&mut self, n: usize) -> &[u8] {
        self.ip += n;
        unsafe { &self.func().chunk.buffer.get_unchecked((self.ip - n)..self.ip) }
    }

    #[inline]
    fn read_short(&mut self) -> u16 {
        let bytes = self.read_bytes(2);
        u16::from_ne_bytes([bytes[0], bytes[1]])
    }

    #[inline]
    fn read_opcode(&mut self) -> Opcode {
        Opcode::decode_unchecked(self.read_byte())
    }

    #[inline]
    fn read_const(&mut self) -> &Value {
        let offset = self.read_byte() as usize;
        &self.func().chunk.constants[offset]
    }
}

struct CallStack<'a>(&'a Stack<CallFrame>);
impl<'a> Display for CallStack<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let count = self.0.len() - 1;
        for (i, frame) in self.0.iter().enumerate() {
            if !frame.func().name.is_empty() {
                write!(f, "{}{}", frame.func().name, if i != count { " -> " } else { "" })?;
            }
        }

        Ok(())
    }
}
// HACK: To sidestep lifetime constraint of CallFrame caused by partial mutable borrow of vm state
struct CurrentFrame(*mut CallFrame);
impl Deref for CurrentFrame {
    type Target = CallFrame;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}
impl DerefMut for CurrentFrame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            stack: Stack::new(),
            frames: Stack::new(),
            globals: Table::new(),
        }
    }

    pub fn error(&mut self, message: &str) -> Error {
        error!(&self.frames, "{}", message)
    }

    pub fn define_native_fn(&mut self, name: &str, ptr: NativeFnPtr) {
        // TODO: maybe fix this according to 24.7 defineNative(...) if GC is ever implemented
        let name = name.to_string();
        self.globals
            .insert(name.clone(), Value::object(Object::NativeFn(NativeFn::new(name, ptr))));
    }

    pub fn interpret(&mut self, func: &Function) -> Result<()> {
        // empty potential garbage from last run
        self.frames.clear();
        self.stack.clear();
        // CallFrame holds a raw pointer to the function, instead
        // of a reference. This is safe, because the call frames are
        // all popped before this function returns, meaning that
        // the raw pointer will not dangle in case the user drops
        // the Function after interpreting it once.

        // The CallFrame also holds a mutable pointer to the stack.
        // This is safe because we don't ever violate the
        // "only one mutable reference at a time" rule.
        self.frames
            .push(CallFrame::new(&mut self.stack, 0, &Closure::new(func)));
        self.run()
    }

    #[allow(clippy::single_match, unused_assignments)]
    fn run(&mut self) -> Result<()> {
        macro_rules! bin_op {
            ($stack:expr, $frame:ident, $out:ident, $op:tt) => {{
                if let Value::Number(right) = $frame.stack().pop() {
                    if let Value::Number(left) = $frame.stack().pop() {
                        $frame.stack().push(Value::$out(left $op right));
                        continue;
                    }
                    return Err(error!($stack, "Left operand must be a number"));
                }
                return Err(error!($stack, "Right operand must be a number"));
            }};
        }

        let frames_len = self.frames.len();
        let mut frame = CurrentFrame(&mut self.frames[frames_len - 1]);
        loop {
            if cfg!(debug_assertions) {
                println!("        | {}", CallStack(&self.frames));
                println!("        | {}", frame.stack());
                disassemble_instruction(&frame.func().chunk, frame.ip, &mut Vec::new());
            }
            let instruction: Opcode = frame.read_opcode();
            match instruction {
                Opcode::Constant => {
                    let constant = frame.read_const().clone();
                    frame.stack().push(constant);
                    continue;
                }
                Opcode::Closure => {
                    let constant = frame.read_const().clone();
                    let object = constant.as_object();
                    let object = &(*object.borrow());
                    let func = object.as_function();
                    let closure = Closure::new(func);
                    frame.stack().push(Value::object(Object::Closure(closure)));
                    continue;
                }
                Opcode::Nil => {
                    frame.stack().push(Value::Nil);
                    continue;
                }
                Opcode::True => {
                    frame.stack().push(Value::Bool(true));
                    continue;
                }
                Opcode::False => {
                    frame.stack().push(Value::Bool(false));
                    continue;
                }

                Opcode::Add => {
                    let right = frame.stack().pop();
                    let left = frame.stack().pop();
                    if let Value::Number(left) = left {
                        if let Value::Number(right) = right {
                            frame.stack().push(Value::Number(left + right));
                            continue;
                        }
                    }
                    if let Value::Object(left) = left {
                        if let Object::String(left) = &*left.borrow() {
                            if let Value::Object(right) = right {
                                if let Object::String(right) = &*right.borrow() {
                                    let new_obj = Value::object(Object::String(format!("{}{}", left, right)));
                                    frame.stack().push(new_obj);
                                    continue;
                                }
                            }
                        }
                    }
                    return Err(error!(
                        &self.frames,
                        "Operands must be numbers or strings and must also match"
                    ));
                }
                Opcode::Subtract => bin_op!(&self.frames, frame, Number, -),
                Opcode::Multiply => bin_op!(&self.frames, frame, Number, *),
                Opcode::Divide => bin_op!(&self.frames, frame, Number, /),
                Opcode::Negate => {
                    if let Value::Number(value) = frame.stack().pop() {
                        frame.stack().push(Value::Number(-value));
                        continue;
                    }
                    return Err(error!(&self.frames, "Operand must be a number"));
                }

                Opcode::Not => {
                    let value = frame.stack().pop();
                    frame.stack().push(Value::Bool(!value.truthy()));
                    continue;
                }
                Opcode::Equal => {
                    let right = frame.stack().pop();
                    let left = frame.stack().pop();
                    frame.stack().push(Value::Bool(left == right));
                    continue;
                }
                Opcode::Greater => bin_op!(&self.frames, frame, Bool, >),
                Opcode::Less => bin_op!(&self.frames, frame, Bool, <),

                Opcode::Pop => {
                    frame.stack().pop();
                    continue;
                }
                Opcode::DefineGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    let value = frame.stack().pop();
                    self.globals.insert(name.clone(), value.clone());
                    continue;
                }
                Opcode::GetGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    match self.globals.get(name) {
                        Some(value) => frame.stack().push(value.clone()),
                        None => return Err(error!(&self.frames, "Undefined variable '{}'", name)),
                    };
                    continue;
                }
                Opcode::SetGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    match self.globals.get_mut(name) {
                        Some(value) => *value = frame.stack().peek(0).clone(),
                        None => {
                            frame.stack().pop();
                            return Err(error!(&self.frames, "Undefined variable '{}'", name));
                        }
                    }
                    continue;
                }
                Opcode::GetLocal => {
                    let slot = frame.read_byte();
                    let stack_top = frame.stack_top;
                    let value = frame.stack()[stack_top + slot as usize].clone();
                    frame.stack().push(value);
                    continue;
                }
                Opcode::SetLocal => {
                    let slot = frame.read_byte();
                    frame.stack()[slot as usize] = frame.stack().top().clone();
                    continue;
                }
                // TODO
                Opcode::GetUpvalue => {}
                Opcode::SetUpvalue => {}

                Opcode::Print => {
                    let value = frame.stack().pop();
                    println!("{}", value);
                    continue;
                }

                Opcode::Call => {
                    let count = frame.read_byte() as usize;
                    let value = frame.stack().peek(count).clone();
                    if let Value::Object(obj) = value {
                        match &(*obj.borrow()) {
                            /* Object::Function(func) => {
                                if count != func.arity as usize {
                                    frame.stack().pop();
                                    return Err(error!(
                                        &self.frames,
                                        "Expected {} arguments but got {}", func.arity, count
                                    ));
                                }
                                if self.frames.len() == FRAMES_MAX {
                                    frame.stack().pop();
                                    return Err(error!(&self.frames, "Stack overflow"));
                                }
                                // this whole dance is safe because we're not mutating the function
                                let stack_top = self.stack.len() - count;
                                self.frames
                                    .push(CallFrame::new(frame.stack(), stack_top, func));
                                let frames_len = self.frames.len();
                                frame = CurrentFrame(&mut self.frames[frames_len - 1]);
                                continue;
                            } */
                            Object::Closure(closure) => {
                                let func = closure.func();
                                if count != func.arity as usize {
                                    frame.stack().pop();
                                    return Err(error!(
                                        &self.frames,
                                        "Expected {} arguments but got {}", func.arity, count
                                    ));
                                }
                                if self.frames.len() == FRAMES_MAX {
                                    frame.stack().pop();
                                    return Err(error!(&self.frames, "Stack overflow"));
                                }
                                // this whole dance is safe because we're not mutating the function
                                let stack_top = self.stack.len() - count;
                                self.frames.push(CallFrame::new(frame.stack(), stack_top, closure));
                                let frames_len = self.frames.len();
                                frame = CurrentFrame(&mut self.frames[frames_len - 1]);
                                continue;
                            }
                            Object::NativeFn(func) => {
                                let mut args = Vec::new();
                                for _ in 0..count {
                                    args.push(self.stack.pop());
                                }
                                args.reverse();
                                self.stack.pop();
                                let value = (func.ptr)(self, args)?;
                                self.stack.push(value);
                                continue;
                            }
                            _ => (),
                        }
                    }
                    frame.stack().pop();
                    return Err(error!(&self.frames, "Can only call functions and classes"));
                }

                Opcode::JumpIfFalse => {
                    let offset = frame.read_short();
                    if frame.stack().peek(0).truthy() {
                        continue;
                    }
                    frame.ip += offset as usize;
                    continue;
                }
                Opcode::Jump => {
                    let offset = frame.read_short();
                    frame.ip += offset as usize;
                    continue;
                }
                Opcode::Loop => {
                    let offset = frame.read_short();
                    frame.ip -= offset as usize;
                    continue;
                }
                Opcode::Return => {
                    // pop the call frame
                    let popped_frame = self.frames.pop();
                    if self.frames.len() == 0 {
                        // if this is the last frame, we're exiting the program
                        return Ok(());
                    }
                    // pop result from previous call
                    let result = self.stack.pop();
                    // pop call args
                    for _ in 0..popped_frame.pop_n {
                        self.stack.pop();
                    }
                    // pus the result from the previous call onto the stack
                    self.stack.push(result);
                    // then set the current frame to the top of the call frame
                    // this is either the previous call, or the top-level frame
                    let frames_len = self.frames.len();
                    frame = CurrentFrame(&mut self.frames[frames_len - 1]);
                }
                Opcode::Max => unreachable!(),
            }
        }
    }
}
