use std::{
    fmt::{self, Display, Formatter},
    ops::{Index, IndexMut},
    slice::SliceIndex,
};

// TODO: write tests for this

#[derive(Debug)]
pub struct Stack<T> {
    stack: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack { stack: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Stack<T> {
        Stack {
            stack: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.stack.capacity()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn top(&self) -> &T {
        let top = self.len() - 1;
        &self.stack[top]
    }

    pub fn top_mut(&mut self) -> &mut T {
        let top = self.len() - 1;
        &mut self.stack[top]
    }

    pub fn push(&mut self, value: T) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> T {
        self.stack.pop().unwrap()
    }

    pub fn peek(&self, depth: usize) -> &T {
        if (self.len() as isize) - (depth as isize) < 0isize {
            panic!("Stack is not {} values deep", depth);
        }
        &self.stack[self.len() - 1 - depth]
    }

    pub fn peek_mut(&mut self, depth: usize) -> &mut T {
        if (self.len() as isize) - (depth as isize) < 0isize {
            panic!("Stack is not {} values deep", depth);
        }
        let top = self.len() - 1;
        &mut self.stack[top - depth]
    }

    pub fn iter(&self) -> core::slice::Iter<'_, T> {
        self.stack.iter()
    }

    pub fn clear(&mut self) {
        self.stack.clear();
    }
}

impl<T: Display> Display for Stack<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for i in 0..self.len() {
            write!(f, "[{}]", self.stack[i])?;
        }
        Ok(())
    }
}

impl<Idx, T> Index<Idx> for Stack<T>
where
    Idx: std::slice::SliceIndex<[T]>,
{
    type Output = Idx::Output;
    fn index(&self, index: Idx) -> &Self::Output {
        &self.stack[index]
    }
}

impl<Idx, T> IndexMut<Idx> for Stack<T>
where
    Idx: std::slice::SliceIndex<[T]>,
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.stack[index]
    }
}
