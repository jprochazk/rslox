use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use crate::{chunk::Chunk, vm};

pub type Table = HashMap<String, Value>;

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub chunk: Chunk,
}

impl Function {
    pub fn new() -> Function {
        Function {
            name: String::new(),
            arity: 0,
            chunk: Chunk::new(),
        }
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

#[derive(Clone)]
pub enum Object {
    String(String),
    Function(Function),
    NativeFn(NativeFn),
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

    pub fn as_function_mut(&mut self) -> &mut Function {
        if let Object::Function(value) = self {
            value
        } else {
            panic!("Not a function");
        }
    }

    pub fn as_native_fn(&self) -> &NativeFn {
        if let Self::NativeFn(v) = self {
            v
        } else {
            panic!("Not a native function");
        }
    }

    pub fn as_native_fn_mut(&mut self) -> &mut NativeFn {
        if let Self::NativeFn(v) = self {
            v
        } else {
            panic!("Not a native function");
        }
    }

    /// Returns `true` if the object is [`String`].
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    /// Returns `true` if the object is [`Function`].
    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(..))
    }

    /// Returns `true` if the object is [`NativeFn`].
    pub fn is_native_fn(&self) -> bool {
        matches!(self, Self::NativeFn(..))
    }
}

pub type Ptr<T> = Rc<RefCell<T>>;

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(Ptr<Object>),
}

impl Value {
    /// Returns `true` if the value is [`Nil`].
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    /// Returns `true` if the value is [`Bool`].
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    /// Returns `true` if the value is [`Number`].
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(..))
    }

    /// Returns `true` if the value is [`Object`].
    pub fn is_object(&self) -> bool {
        matches!(self, Self::Object(..))
    }

    /// Heap-allocated value
    pub fn object(inner: Object) -> Value {
        Value::Object(Rc::new(RefCell::new(inner)))
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
