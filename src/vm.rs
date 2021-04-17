use std::{
    fmt::{self, Display, Formatter},
    ops::{Deref, DerefMut},
};

use crate::{
    chunk::disassemble_instruction,
    op::Opcode,
    stack::Stack,
    value::{make_ptr, Closure, Function, NativeFn, NativeFnPtr, Object, Table, Upvalue, Value},
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
    closure: *mut Closure,
    stack_top: usize,
    pop_n: usize,
}

impl CallFrame {
    pub fn new(stack_top: usize, closure: *mut Closure) -> CallFrame {
        CallFrame {
            ip: 0,
            closure,
            stack_top,
            pop_n: unsafe { ((*closure).func().arity + 1) as usize },
        }
    }

    #[inline]
    fn closure(&self) -> &Closure {
        unsafe { &(*self.closure) }
    }

    #[inline]
    fn closure_mut(&mut self) -> &mut Closure {
        unsafe { &mut (*self.closure) }
    }

    #[inline]
    fn func(&self) -> &Function {
        self.closure().func()
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

    pub fn interpret(&mut self, func: Function) -> Result<()> {
        // empty potential garbage from last run
        self.frames.clear();
        self.stack.clear();
        let mut initial_closure = Closure::new(make_ptr(Object::Function(func)), Vec::new());
        self.frames.push(CallFrame::new(0, &mut initial_closure));
        self.run()
    }

    #[allow(clippy::single_match, unused_assignments)]
    fn run(&mut self) -> Result<()> {
        let frames_len = self.frames.len();
        //let stack = &mut self.stack;
        let mut frame = CurrentFrame(&mut self.frames[frames_len - 1]);
        loop {
            if cfg!(debug_assertions) {
                println!("        | {}", CallStack(&self.frames));
                println!("        | {}", self.stack);
                disassemble_instruction(&frame.func().chunk, frame.ip, &mut Vec::new());
            }
            let instruction: Opcode = frame.read_opcode();
            match instruction {
                Opcode::Constant => {
                    let constant = frame.read_const().clone();
                    self.stack.push(constant);
                    continue;
                }
                Opcode::Closure => {
                    let (func, upvalues) = {
                        let constant = frame.read_const().clone();
                        let object = constant.as_object();
                        let object_ref = &(*object.borrow());
                        let func = object_ref.as_function();
                        let num_upvalues = func.num_upvalues as usize;
                        let mut upvalues = Vec::with_capacity(num_upvalues);
                        for _ in 0..num_upvalues {
                            let is_local = frame.read_byte() == 1;
                            let index = frame.read_byte() as usize;
                            if is_local {
                                let slot = frame.stack_top + index;
                                upvalues.push(Upvalue::new(self.stack[slot].clone()));
                            } else {
                                upvalues.push(frame.closure().upvalues[index].clone());
                            }
                        }
                        (object.clone(), upvalues)
                    };
                    let closure = Closure::new(func, upvalues);
                    self.stack.push(Value::object(Object::Closure(closure)));
                    continue;
                }
                Opcode::Nil => {
                    self.stack.push(Value::Nil);
                    continue;
                }
                Opcode::True => {
                    self.stack.push(Value::Bool(true));
                    continue;
                }
                Opcode::False => {
                    self.stack.push(Value::Bool(false));
                    continue;
                }

                Opcode::Add => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(left) = left {
                        if let Value::Number(right) = right {
                            self.stack.push(Value::Number(left + right));
                            continue;
                        }
                    }
                    if let Value::Object(left) = left {
                        if let Object::String(left) = &*left.borrow() {
                            if let Value::Object(right) = right {
                                if let Object::String(right) = &*right.borrow() {
                                    let new_obj = Value::object(Object::String(format!("{}{}", left, right)));
                                    self.stack.push(new_obj);
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
                Opcode::Subtract => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(right) = right {
                        if let Value::Number(left) = left {
                            self.stack.push(Value::Number(left - right));
                            continue;
                        }
                        return Err(error!(&self.frames, "Left operand must be a number"));
                    }
                    return Err(error!(&self.frames, "Right operand must be a number"));
                }
                Opcode::Multiply => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(right) = right {
                        if let Value::Number(left) = left {
                            self.stack.push(Value::Number(left * right));
                            continue;
                        }
                        return Err(error!(&self.frames, "Left operand must be a number"));
                    }
                    return Err(error!(&self.frames, "Right operand must be a number"));
                }
                Opcode::Divide => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(right) = right {
                        if let Value::Number(left) = left {
                            self.stack.push(Value::Number(left / right));
                            continue;
                        }
                        return Err(error!(&self.frames, "Left operand must be a number"));
                    }
                    return Err(error!(&self.frames, "Right operand must be a number"));
                }
                Opcode::Negate => {
                    if let Value::Number(value) = self.stack.pop() {
                        self.stack.push(Value::Number(-value));
                        continue;
                    }
                    return Err(error!(&self.frames, "Operand must be a number"));
                }

                Opcode::Not => {
                    let value = self.stack.pop();
                    self.stack.push(Value::Bool(!value.truthy()));
                    continue;
                }
                Opcode::Equal => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    self.stack.push(Value::Bool(left == right));
                    continue;
                }
                Opcode::Greater => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(right) = right {
                        if let Value::Number(left) = left {
                            self.stack.push(Value::Bool(left > right));
                            continue;
                        }
                        return Err(error!(&self.frames, "Left operand must be a number"));
                    }
                    return Err(error!(&self.frames, "Right operand must be a number"));
                }
                Opcode::Less => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();
                    if let Value::Number(right) = right {
                        if let Value::Number(left) = left {
                            self.stack.push(Value::Bool(left < right));
                            continue;
                        }
                        return Err(error!(&self.frames, "Left operand must be a number"));
                    }
                    return Err(error!(&self.frames, "Right operand must be a number"));
                }

                Opcode::Pop => {
                    self.stack.pop();
                    continue;
                }
                Opcode::DefineGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    let value = self.stack.pop();
                    self.globals.insert(name.clone(), value.clone());
                    continue;
                }
                Opcode::GetGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    match self.globals.get(name) {
                        Some(value) => {
                            self.stack.push(value.clone());
                        }
                        None => return Err(error!(&self.frames, "Undefined variable '{}'", name)),
                    };
                    continue;
                }
                Opcode::SetGlobal => {
                    let constant = frame.read_const().clone();
                    let object = &(*constant.as_object().borrow());
                    let name = object.as_string();
                    match self.globals.get_mut(name) {
                        Some(value) => *value = self.stack.peek(0).clone(),
                        None => {
                            self.stack.pop();
                            return Err(error!(&self.frames, "Undefined variable '{}'", name));
                        }
                    }
                    continue;
                }
                Opcode::GetLocal => {
                    let slot = frame.read_byte();
                    let stack_top = frame.stack_top;
                    let value = self.stack[stack_top + slot as usize].clone();
                    self.stack.push(value);
                    continue;
                }
                Opcode::SetLocal => {
                    let slot = frame.read_byte();
                    let stack_top = frame.stack_top;
                    self.stack[stack_top + slot as usize] = self.stack.top().clone();
                    continue;
                }
                Opcode::GetUpvalue => {
                    let slot = frame.read_byte();
                    let value = frame.closure().upvalues[slot as usize].clone();
                    self.stack.push(value.capture);
                    continue;
                }
                Opcode::SetUpvalue => {
                    let slot = frame.read_byte();
                    frame.closure_mut().upvalues[slot as usize].capture = self.stack.peek(0).clone();
                    continue;
                }

                Opcode::Print => {
                    let value = self.stack.pop();
                    println!("{}", value);
                    continue;
                }

                Opcode::Call => {
                    let count = frame.read_byte() as usize;
                    let value = self.stack.peek(count).clone();
                    if let Value::Object(obj) = value {
                        match &mut (*obj.borrow_mut()) {
                            /* Object::Function(func) => {
                                if count != func.arity as usize {
                                    self.stack.pop();
                                    return Err(error!(
                                        &self.frames,
                                        "Expected {} arguments but got {}", func.arity, count
                                    ));
                                }
                                if self.frames.len() == FRAMES_MAX {
                                    self.stack.pop();
                                    return Err(error!(&self.frames, "self.stack overflow"));
                                }
                                let stack_top = self.stack.len() - count;
                                self.frames
                                    .push(CallFrame::new(self.stack, stack_top, func));
                                let frames_len = self.frames.len();
                                frame = CurrentFrame(&mut self.frames[frames_len - 1]);
                                continue;
                            } */
                            Object::Closure(closure) => {
                                let func = closure.func();
                                if count != func.arity as usize {
                                    self.stack.pop();
                                    return Err(error!(
                                        &self.frames,
                                        "Expected {} arguments but got {}", func.arity, count
                                    ));
                                }
                                if self.frames.len() == FRAMES_MAX {
                                    self.stack.pop();
                                    return Err(error!(&self.frames, "Stack overflow"));
                                }
                                let stack_top = self.stack.len() - count;
                                self.frames.push(CallFrame::new(stack_top, closure));
                                let frames_len = self.frames.len();
                                frame = CurrentFrame(&mut self.frames[frames_len - 1]);
                                continue;
                            }
                            Object::NativeFn(func) => {
                                let args = {
                                    let mut args = Vec::new();
                                    for _ in 0..count {
                                        args.push(self.stack.pop());
                                    }
                                    args.reverse();
                                    self.stack.pop();
                                    args
                                };
                                let value = (func.ptr)(self, args)?;
                                self.stack.push(value);
                                continue;
                            }
                            _ => (),
                        }
                    }
                    self.stack.pop();
                    return Err(error!(&self.frames, "Can only call functions and classes"));
                }

                Opcode::JumpIfFalse => {
                    let offset = frame.read_short();
                    if self.stack.peek(0).truthy() {
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
