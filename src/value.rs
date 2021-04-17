use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    rc::Rc,
};

use crate::{chunk::Chunk, vm};

pub type Table = HashMap<String, Value>;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub num_upvalues: u8,
    pub chunk: Chunk,
}

impl Function {
    pub fn new() -> Function {
        Function {
            name: String::new(),
            arity: 0,
            num_upvalues: 0,
            chunk: Chunk::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Closure {
    func_ptr: *const Function,
    _func: Ptr<Object>,
    pub upvalues: Vec<Upvalue>,
}

impl Closure {
    pub fn new(func: Ptr<Object>, upvalues: Vec<Upvalue>) -> Closure {
        let func_ptr = {
            let object = &(*func.borrow());
            object.as_function() as *const _
        };
        Closure {
            func_ptr,
            _func: func,
            upvalues,
        }
    }

    #[inline]
    pub fn func(&self) -> &Function {
        unsafe { &*self.func_ptr }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
}

impl Class {
    pub fn new(name: String) -> Class {
        Class { name }
    }
}

#[derive(Clone, Debug)]
pub struct Upvalue {
    pub capture: Value,
}

impl Upvalue {
    pub fn new(capture: Value) -> Upvalue {
        Upvalue { capture }
    }
}

pub type NativeFnPtr = fn(&mut vm::Vm, Vec<Value>) -> vm::Result<Value>;

// TODO: arity?
#[derive(Clone)]
pub struct NativeFn {
    pub name: String,
    pub ptr: Box<NativeFnPtr>,
}

impl NativeFn {
    pub fn new(name: String, ptr: NativeFnPtr) -> NativeFn {
        NativeFn {
            name,
            ptr: Box::new(ptr),
        }
    }
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFn")
            .field("name", &self.name)
            .field("ptr", &(self.ptr.as_ref() as *const _ as usize))
            .finish()
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    String(String),
    Function(Function),
    NativeFn(NativeFn),
    Closure(Closure),
    Class(Class),
}

impl Object {
    pub fn as_string(&self) -> &String {
        if let Object::String(value) = self {
            value
        } else {
            panic!("Not a string");
        }
    }

    pub fn as_function(&self) -> &Function {
        if let Object::Function(value) = self {
            value
        } else {
            panic!("Not a function");
        }
    }
}

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn make_ptr<T>(value: T) -> Ptr<T> {
    Rc::new(RefCell::new(value))
}

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(Ptr<Object>),
}

impl Value {
    /// Heap-allocated value
    pub fn object(inner: Object) -> Value {
        Value::Object(make_ptr(inner))
    }

    pub fn as_object(&self) -> &Ptr<Object> {
        if let Value::Object(inner) = self {
            inner
        } else {
            panic!("Not an object");
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(v) => *v,
            _ => true,
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => {
                // two objects are equal if their address is equal
                a.as_ptr() as usize == b.as_ptr() as usize
            }
            _ => false,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(v) => write!(f, "{}", v),
            Object::Function(v) => write!(f, "{}", v),
            Object::NativeFn(v) => write!(f, "{}", v),
            Object::Closure(v) => write!(f, "{}", v.func()),
            Object::Class(v) => write!(f, "{}", v),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", self.name)
        }
    }
}

impl Display for NativeFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Number(v) => write!(f, "{}", v),
            Value::Object(v) => write!(f, "{}", v.borrow()),
        }
    }
}
