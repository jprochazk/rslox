#![feature(maybe_uninit_uninit_array, maybe_uninit_ref)]
#![allow(
    irrefutable_let_patterns,
    clippy::blocks_in_if_conditions,
    clippy::unnecessary_wraps,
    clippy::not_unsafe_ptr_arg_deref
)]

use rslox::compiler;
use rslox::value::{Object, Value};
use rslox::vm::{self, Vm};
use rustyline::{error::ReadlineError, Editor};

fn clock_wrapper(_vm: &mut Vm, _args: Vec<Value>) -> vm::Result<Value> {
    Ok(Value::Number(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64(),
    ))
}

fn str_wrapper(vm: &mut Vm, args: Vec<Value>) -> vm::Result<Value> {
    if args.len() != 1 {
        return Err(vm.error(&format!("Invalid number of args: {}", args.len())));
    }
    Ok(Value::object(Object::String(format!("{}", args[0]))))
}

fn panic_wrapper(vm: &mut Vm, args: Vec<Value>) -> vm::Result<Value> {
    if args.len() == 1 {
        if let Value::Object(object) = &args[0] {
            if let Object::String(string) = &(*object.borrow()) {
                return Err(vm.error(string));
            }
        }
    }
    Err(vm.error("Error with no message"))
}

fn log_wrapper(_vm: &mut Vm, args: Vec<Value>) -> vm::Result<Value> {
    let mut iter = args.iter().peekable();
    while let Some(arg) = iter.next() {
        print!("{}", arg);
        if iter.peek().is_some() {
            print!(" ");
        } else {
            println!();
        }
    }
    Ok(Value::Nil)
}

fn main() {
    let mut vm = Vm::new();
    vm.define_native_fn("clock", clock_wrapper);
    vm.define_native_fn("str", str_wrapper);
    vm.define_native_fn("panic", panic_wrapper);
    vm.define_native_fn("log", log_wrapper);
    if let Some(function) = compiler::compile(
        r##"
class 😂 {
    method() {
        print "😂 method";
    }
}
class B < 😂 {
    method() {
        print "B method";
    }
    test() {
        this.method();
        super.method();
        var m = super.method;
        m();
    }
}
class C < B {}
C().test();
        "##,
    ) {
        if let Err(err) = vm.interpret(function) {
            eprintln!("Error: {}", err);
        }
    }
    /* let mut rl = Editor::<()>::new();
    loop {
        let line = rl.readline("> ");
        match line {
            Ok(line) if &line == "exit" => {
                break;
            }
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Some(func) = compiler::compile(&line) {
                    if let Err(err) = vm.interpret(&func) {
                        eprintln!("Error: {}", err);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Readline error: {:?}", err);
                break;
            }
        }
    } */
}
