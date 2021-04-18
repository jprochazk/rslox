#![feature(maybe_uninit_uninit_array, maybe_uninit_ref)]
#![allow(
    irrefutable_let_patterns,
    clippy::blocks_in_if_conditions,
    clippy::unnecessary_wraps,
    clippy::not_unsafe_ptr_arg_deref,
    clippy::new_without_default,
    clippy::should_implement_trait
)]

pub mod chunk;
pub mod compiler;
pub mod op;
pub mod scanner;
pub mod stack;
pub mod value;
pub mod vm;
