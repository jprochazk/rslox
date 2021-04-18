use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::Add,
    u8,
};

use crate::{
    chunk::{disassemble_chunk, Chunk},
    op::Opcode,
    scanner::{Scanner, Token, TokenKind},
    stack::Stack,
    value::{Function, Object, Value},
};

pub fn compile(source: &str) -> Option<Function> {
    let mut compiler = Compiler::new(source);
    compiler.advance();

    while !compiler.maybe(TokenKind::Eof) {
        compiler.declaration();
    }

    compiler
        .state
        .top_mut()
        .func
        .chunk
        .push_byte(Opcode::Return as u8, compiler.previous.line);

    if cfg!(debug_assertions) && !compiler.errored {
        let func = &compiler.state.top().func;
        disassemble_chunk(&func.chunk, if func.name.is_empty() { "MAIN" } else { &func.name });
        println!("==========");
    }
    if compiler.errored {
        None
    } else {
        Some(compiler.state.pop().func)
    }
}

// TODO: https://craftinginterpreters.com/methods-and-initializers.html#misusing-this
// CompilerState should also keep track of current Class, not just current Function
// i have no clue how that should work.

#[derive(Debug)]
struct Compiler<'a> {
    scanner: Scanner<'a>,
    state: Stack<CompilerState<'a>>,
    current: Token<'a>,
    previous: Token<'a>,
    errored: bool,
    panic_mode: bool,
}

#[derive(Debug)]
struct CompilerState<'a> {
    func: Function,
    kind: FunctionKind,
    class: Option<Token<'a>>,
    superclass: Option<Token<'a>>,
    locals: Locals<'a>,
    upvalues: Stack<Upvalue>,
}

impl<'a> CompilerState<'a> {
    pub fn new(
        kind: FunctionKind,
        enclosing_class: Option<Token<'a>>,
        superclass: Option<Token<'a>>,
    ) -> CompilerState<'a> {
        CompilerState {
            func: Function::new(),
            kind,
            class: enclosing_class,
            superclass,
            locals: Locals::new(),
            upvalues: Stack::new(),
        }
    }
}

impl<'a> Compiler<'a> {
    fn new(source: &'a str) -> Compiler<'a> {
        let mut state = Stack::new();
        state.push(CompilerState::new(FunctionKind::Script, None, None));
        Compiler {
            scanner: Scanner::new(source),
            state,
            current: Token {
                kind: TokenKind::Error,
                lexeme: "UNINITIALIZED",
                line: 123456789,
            },
            previous: Token {
                kind: TokenKind::Error,
                lexeme: "UNINITIALIZED",
                line: 123456789,
            },
            errored: false,
            panic_mode: false,
        }
    }

    fn error(&mut self, message: &str, token: Token<'a>) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprintln!("[line {}] {} at '{}'", token.line, message, token.lexeme);
        self.errored = true;
    }

    fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.next();
            if self.current.kind != TokenKind::Error {
                break;
            }

            self.error(self.current.lexeme, self.current);
        }
    }

    fn consume(&mut self, kind: TokenKind, message: &str) {
        if self.current.kind != kind {
            return self.error(message, self.current);
        }

        self.advance()
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    fn maybe(&mut self, kind: TokenKind) -> bool {
        if !self.check(kind) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.kind != TokenKind::Eof {
            if self.previous.kind == TokenKind::Semicolon {
                return;
            }

            match self.current.kind {
                TokenKind::Class => return,
                TokenKind::Fun => return,
                TokenKind::Var => return,
                TokenKind::For => return,
                TokenKind::If => return,
                TokenKind::While => return,
                TokenKind::Print => return,
                TokenKind::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn emit_raw(&mut self, raw: &[u8]) {
        let line = self.previous.line;
        let chunk = &mut self.state.top_mut().func.chunk;
        chunk.push_bytes(raw, line);
    }

    fn emit_raw_in(&mut self, chunk: &mut Chunk, raw: &[u8]) {
        let line = self.previous.line;
        chunk.push_bytes(raw, line)
    }

    fn emit_op(&mut self, op: Opcode) {
        let line = self.previous.line;
        let chunk = &mut self.state.top_mut().func.chunk;
        chunk.push_byte(op as u8, line);
    }

    fn emit_ops(&mut self, ops: &[Opcode]) {
        let ops = unsafe { &*(ops as *const [Opcode] as *const [u8]) };
        let line = self.previous.line;
        let chunk = &mut self.state.top_mut().func.chunk;
        chunk.push_bytes(ops, line)
    }

    fn emit_ops_in(&mut self, chunk: &mut Chunk, ops: &[Opcode]) {
        let ops = unsafe { &*(ops as *const [Opcode] as *const [u8]) };
        let line = self.previous.line;
        chunk.push_bytes(ops, line)
    }

    fn emit_const(&mut self, value: Value) -> u8 {
        let chunk = &mut self.state.top_mut().func.chunk;
        let out = chunk.push_const(value);
        if chunk.constants.len() == std::u8::MAX as usize {
            self.error("Too many constants in one chunk", self.previous);
        }
        out
    }

    fn emit_jump(&mut self, op: Opcode) -> usize {
        self.emit_raw(&[op as u8, 0xFF, 0xFF]);
        &self.state.top().func.chunk.buffer.len() - 2
    }

    fn emit_loop(&mut self, start: usize) {
        self.emit_op(Opcode::Loop);
        let offset = self.state.top().func.chunk.buffer.len() - start + 2;
        if offset > std::u16::MAX as usize {
            self.error("Loop body too large", self.previous);
        }
        let bytes = offset.to_ne_bytes();
        self.emit_raw(&[bytes[0], bytes[1]]);
    }

    fn begin_scope(&mut self) {
        self.state.top_mut().locals.current_depth += 1;
    }

    fn end_scope(&mut self) {
        self.state.top_mut().locals.current_depth -= 1;

        while self.state.top_mut().locals.stack.len() > 0
            && self.state.top_mut().locals.stack.top().depth > self.state.top_mut().locals.current_depth as isize
        {
            self.emit_op(Opcode::Pop);
            self.state.top_mut().locals.stack.pop();
        }
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.state.top_mut().locals.stack.len() == std::u8::MAX as usize {
            self.error("Too many local variables", name);
            return;
        }
        self.state.top_mut().locals.stack.push(Local {
            name,
            depth: -1,
            is_captured: false,
        })
    }

    fn add_upvalue(&mut self, state_idx: usize, index: u8, is_local: bool) -> u8 {
        let state = &mut self.state[state_idx];
        let upvalue_count = state.func.num_upvalues as usize;
        for i in 0..upvalue_count {
            let upvalue = &state.upvalues[i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        if upvalue_count == std::u8::MAX as usize {
            self.error("Too many captured variables", self.previous);
            return 0;
        }

        state.upvalues.push(Upvalue { is_local, index });
        state.func.num_upvalues += 1;
        state.func.num_upvalues - 1
    }

    fn resolve_local(&mut self, state_idx: usize, name: Token<'a>) -> i8 {
        let state = &self.state[state_idx];
        for i in (0..state.locals.stack.len()).rev() {
            let local = &state.locals.stack[i];
            if local.name.lexeme == name.lexeme {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer", name);
                }
                return i as i8;
            }
        }
        -1
    }

    fn resolve_upvalue(&mut self, state_idx: usize, name: Token<'a>) -> i8 {
        if self.state.len() >= 3 {
            // we're in a nested scope
            // try to find the variable in the enclosing scope
            let enclosing = state_idx - 1;
            let local = self.resolve_local(enclosing, name);
            if local != -1 {
                self.state[state_idx - 1].locals.stack[local as usize].is_captured = true;
                // if found, add it as an upvalue of the current function and return its index
                return self.add_upvalue(state_idx, local as u8, true) as i8;
            }
            // if it's not in the enclosing scope, it could be found further down the scope stack
            // try to find the variable in the enclosing scope's upvalues
            let upvalue = self.resolve_upvalue(state_idx - 1, name);
            if upvalue != -1 {
                // since this
                return self.add_upvalue(state_idx, upvalue as u8, false) as i8;
            }
        }
        -1
    }

    fn mark_initialized(&mut self) {
        if self.state.top_mut().locals.current_depth == 0 {
            return;
        }
        self.state.top_mut().locals.stack.top_mut().depth = self.state.top_mut().locals.current_depth as isize;
    }

    fn expression(&mut self) {
        self.precedence(Precedence::Assign);
    }

    fn precedence(&mut self, which: Precedence) {
        self.advance();
        let prefix_rule = match get_rule(self.previous.kind).prefix {
            Some(prefix_rule) => prefix_rule,
            None => {
                return self.error("Expected expression", self.previous);
            }
        };
        let can_assign = which <= Precedence::Assign;
        prefix_rule(self, which <= Precedence::Assign);

        while which <= get_rule(self.current.kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.kind).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.maybe(TokenKind::Equal) {
            /* return  */
            self.error("Invalid assignment target", self.previous);
        }
    }

    fn declaration(&mut self) {
        if self.maybe(TokenKind::Var) {
            self.var_declaration();
        } else if self.maybe(TokenKind::Fun) {
            self.fun_declaration();
        } else if self.maybe(TokenKind::Class) {
            self.class_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn class_declaration(&mut self) {
        self.consume(TokenKind::Identifier, "Expected class name");
        let class_name = self.previous;
        let name_constant = self.identifier_constant(self.previous);
        self.declare_variable();

        self.emit_raw(&[Opcode::Class as u8, name_constant]);
        self.define_variable(name_constant);

        let previous_class = self.state.top_mut().class;
        self.state.top_mut().class = Some(class_name);

        if self.maybe(TokenKind::Less) {
            self.consume(TokenKind::Identifier, "Expected superclass name");
            let superclass_name = self.previous;
            variable(self, false);
            if class_name.lexeme == self.previous.lexeme {
                self.error("Class cannot inherit from itself", self.previous);
            }

            self.begin_scope();
            self.add_local(Token::new(TokenKind::Identifier, "super", self.previous.line));
            self.define_variable(0);

            self.named_variable(class_name, false);
            self.emit_op(Opcode::Inherit);
            self.state.top_mut().superclass = Some(superclass_name);
        }

        self.named_variable(class_name, false);
        self.consume(TokenKind::LeftBrace, "Expected '{'");
        while !self.check(TokenKind::Eof) && !self.check(TokenKind::RightBrace) {
            self.method();
        }
        self.consume(TokenKind::RightBrace, "Expected '}'");
        self.emit_op(Opcode::Pop);

        if self.state.top().superclass.is_some() {
            self.end_scope();
        }

        self.state.top_mut().class = previous_class;
    }

    fn method(&mut self) {
        self.consume(TokenKind::Identifier, "Expected method name");
        let constant = self.identifier_constant(self.previous);

        let kind = if self.previous.lexeme == "init" {
            FunctionKind::Initializer
        } else {
            FunctionKind::Method
        };
        self.function(kind);
        self.emit_raw(&[Opcode::Method as u8, constant]);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name");
        self.mark_initialized();
        self.function(FunctionKind::Function);
        self.define_variable(global);
    }

    fn function(&mut self, kind: FunctionKind) {
        self.state.push(CompilerState::new(
            kind,
            self.state.top().class,
            self.state.top().superclass,
        ));

        let current = self.state.top_mut();
        if kind != FunctionKind::Script {
            current.func.name = self.previous.lexeme.to_string();
        }

        let depth = current.locals.current_depth as isize;
        if kind != FunctionKind::Function {
            current.locals.stack.push(Local {
                name: Token::new(TokenKind::Identifier, "this", self.previous.line),
                depth,
                is_captured: false,
            });
        } else {
            current.locals.stack.push(Local {
                name: Token::new(TokenKind::Identifier, "", self.previous.line),
                depth,
                is_captured: false,
            });
        }

        self.begin_scope();
        self.consume(TokenKind::LeftParen, "Expected '('");
        if !self.check(TokenKind::RightParen) {
            loop {
                let current = self.state.top_mut();
                current.func.arity += 1;
                if current.func.arity == std::u8::MAX {
                    self.error("Functions can't have more than 255 parameters", self.previous);
                }
                let param_const = self.parse_variable("Expected parameter name");
                self.define_variable(param_const);
                if !self.maybe(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenKind::RightParen, "Expected ')'");
        self.consume(TokenKind::LeftBrace, "Expected '{'");
        self.block();

        let mut state = self.state.pop();
        self.emit_raw_in(
            &mut state.func.chunk,
            match kind {
                FunctionKind::Initializer => &[Opcode::GetLocal as u8, 0, Opcode::Return as u8],
                _ => &[Opcode::Nil as u8, Opcode::Return as u8],
            },
        );

        let num_upvalues = state.func.num_upvalues;
        let value = Value::object(Object::Function(state.func));
        let offset = self.emit_const(value);
        self.emit_raw(&[Opcode::Closure as u8, offset]);
        for i in 0..num_upvalues {
            let upvalue = &state.upvalues[i as usize];
            let is_local = if upvalue.is_local { 1u8 } else { 0u8 };
            self.emit_raw(&[is_local, upvalue.index]);
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name");
        if self.maybe(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_op(Opcode::Nil);
        }
        self.consume(TokenKind::Semicolon, "Expected ';'");
        self.define_variable(global);
    }

    fn parse_variable(&mut self, error: &str) -> u8 {
        self.consume(TokenKind::Identifier, error);
        self.declare_variable();
        if self.state.top_mut().locals.current_depth > 0 {
            0
        } else {
            self.identifier_constant(self.previous)
        }
    }

    fn declare_variable(&mut self) {
        if self.state.top_mut().locals.current_depth == 0 {
            return;
        }

        let name = self.previous;
        if self
            .state
            .top()
            .locals
            .stack
            .iter()
            .rev()
            .filter(|local| local.depth == -1 || local.depth >= self.state.top().locals.current_depth as isize)
            .any(|local| local.name.lexeme == name.lexeme)
        {
            self.error("Variable with this name already exists within current scope", name);
        }
        self.add_local(name);
    }

    fn define_variable(&mut self, global: u8) {
        if self.state.top_mut().locals.current_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_raw(&[Opcode::DefineGlobal as u8, global]);
    }

    fn identifier_constant(&mut self, name: Token<'a>) -> u8 {
        let value = Value::object(Object::String(name.lexeme.to_string()));
        self.emit_const(value)
    }

    fn statement(&mut self) {
        if self.maybe(TokenKind::Return) {
            self.return_statement();
        } else if self.maybe(TokenKind::Print) {
            self.print_statement();
        } else if self.maybe(TokenKind::If) {
            self.if_statement();
        } else if self.maybe(TokenKind::While) {
            self.while_statement();
        } else if self.maybe(TokenKind::For) {
            self.for_statement();
        } else if self.maybe(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenKind::Eof) && !self.check(TokenKind::RightBrace) {
            self.declaration();
        }

        self.consume(TokenKind::RightBrace, "Expected '}'");
    }

    fn return_statement(&mut self) {
        let fn_kind = self.state.top().kind;
        if fn_kind == FunctionKind::Script {
            self.error("Cannot return from top-level code", self.previous);
        }

        if self.maybe(TokenKind::Semicolon) {
            self.emit_raw(match fn_kind {
                FunctionKind::Initializer => &[Opcode::GetLocal as u8, 0, Opcode::Return as u8],
                _ => &[Opcode::Nil as u8, Opcode::Return as u8],
            });
        } else {
            if fn_kind == FunctionKind::Initializer {
                self.error("Cannot return a value from an initializer", self.previous);
            }
            self.expression();
            self.consume(TokenKind::Semicolon, "Expected ';'");
            self.emit_op(Opcode::Return);
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';'");
        self.emit_op(Opcode::Print);
    }

    fn if_statement(&mut self) {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'if'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expected ')' after condition");

        let then = self.emit_jump(Opcode::JumpIfFalse);
        self.emit_op(Opcode::Pop);
        self.statement();
        let otherwise = self.emit_jump(Opcode::Jump);
        self.patch_jump(then);
        self.emit_op(Opcode::Pop);

        if self.maybe(TokenKind::Else) {
            self.statement();
        }
        self.patch_jump(otherwise);
    }

    fn while_statement(&mut self) {
        let loop_start = self.state.top_mut().func.chunk.buffer.len();

        self.consume(TokenKind::LeftParen, "Expected '(' after 'while'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expected ')' after condition");

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);

        self.emit_op(Opcode::Pop);
        self.statement();

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_op(Opcode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenKind::LeftParen, "Expected '('");
        if self.maybe(TokenKind::Semicolon) {
            // no initializer
        } else if self.maybe(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.state.top_mut().func.chunk.buffer.len();

        let exit_jmp = if !self.maybe(TokenKind::Semicolon) {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expected ';'");
            let exit_jmp = self.emit_jump(Opcode::JumpIfFalse);
            self.emit_op(Opcode::Pop);
            Some(exit_jmp)
        } else {
            None
        };

        if !self.maybe(TokenKind::RightParen) {
            let body_jmp = self.emit_jump(Opcode::Jump);

            let increment_start = self.state.top_mut().func.chunk.buffer.len();
            self.expression();
            self.emit_op(Opcode::Pop);
            self.consume(TokenKind::RightParen, "Expected ')'");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jmp);
        }

        self.statement();

        self.emit_loop(loop_start);

        if let Some(exit_jmp) = exit_jmp {
            self.patch_jump(exit_jmp);
            self.emit_op(Opcode::Pop);
        }

        self.end_scope();
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.state.top_mut().func.chunk.buffer.len() - offset - 2;

        if jump > std::u16::MAX as usize {
            self.error("Jump distance too large", self.previous);
        }
        let bytes = jump.to_ne_bytes();
        self.state.top_mut().func.chunk.buffer[offset] = bytes[0];
        self.state.top_mut().func.chunk.buffer[offset + 1] = bytes[1];
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';'");
        self.emit_op(Opcode::Pop);
    }

    fn named_variable(&mut self, name: Token<'a>, can_assign: bool) {
        let top = self.state.len() - 1;
        let mut arg = self.resolve_local(top, name);
        let (arg, set, get) = if arg != -1 {
            (arg as u8, Opcode::SetLocal as u8, Opcode::GetLocal as u8)
        } else if ({
            arg = self.resolve_upvalue(top, name);
            arg
        }) != -1
        {
            (arg as u8, Opcode::SetUpvalue as u8, Opcode::GetUpvalue as u8)
        } else {
            (
                self.identifier_constant(name),
                Opcode::SetGlobal as u8,
                Opcode::GetGlobal as u8,
            )
        };

        if can_assign && self.maybe(TokenKind::Equal) {
            self.expression();
            self.emit_raw(&[set, arg]);
        } else {
            self.emit_raw(&[get, arg]);
        }
    }

    fn arg_list(&mut self) -> u8 {
        let mut count = 0;
        if !self.check(TokenKind::RightParen) {
            loop {
                self.expression();
                count += 1;
                if count == std::u8::MAX {
                    self.error("Functions can't have more than 255 parameters", self.previous);
                }
                if !self.maybe(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenKind::RightParen, "Expected ')'");
        count
    }
}

fn grouping(compiler: &mut Compiler, _can_assign: bool) {
    compiler.expression();
    compiler.consume(TokenKind::RightParen, "Expected closing ')'");
}

fn unary(compiler: &mut Compiler, _can_assign: bool) {
    let kind = compiler.previous.kind;

    compiler.precedence(Precedence::Unary);

    match kind {
        TokenKind::Bang => compiler.emit_op(Opcode::Not),
        TokenKind::Minus => compiler.emit_op(Opcode::Negate),
        _ => unreachable!(),
    }
}

fn binary(compiler: &mut Compiler, _can_assign: bool) {
    let op = compiler.previous.kind;

    let rule = get_rule(op);
    compiler.precedence(rule.precedence + 1);
    match op {
        TokenKind::Plus => compiler.emit_op(Opcode::Add),
        TokenKind::Minus => compiler.emit_op(Opcode::Subtract),
        TokenKind::Star => compiler.emit_op(Opcode::Multiply),
        TokenKind::Slash => compiler.emit_op(Opcode::Divide),
        TokenKind::BangEqual => compiler.emit_ops(&[Opcode::Equal, Opcode::Not]),
        TokenKind::EqualEqual => compiler.emit_op(Opcode::Equal),
        TokenKind::Greater => compiler.emit_op(Opcode::Greater),
        TokenKind::GreaterEqual => compiler.emit_ops(&[Opcode::Less, Opcode::Not]),
        TokenKind::Less => compiler.emit_op(Opcode::Less),
        TokenKind::LessEqual => compiler.emit_ops(&[Opcode::Greater, Opcode::Not]),
        _ => (),
    }
}

fn number(compiler: &mut Compiler, _can_assign: bool) {
    let value = Value::Number(compiler.previous.lexeme.parse::<f64>().unwrap());
    let offset = compiler.emit_const(value);
    compiler.emit_raw(&[Opcode::Constant as u8, offset]);
}

fn string(compiler: &mut Compiler, _can_assign: bool) {
    let unquoted = &compiler.previous.lexeme[1..compiler.previous.lexeme.len() - 1];
    let value = Value::object(Object::String(unquoted.to_string()));
    let offset = compiler.emit_const(value);
    compiler.emit_raw(&[Opcode::Constant as u8, offset]);
}

fn literal(compiler: &mut Compiler, _can_assign: bool) {
    match compiler.previous.kind {
        TokenKind::Nil => compiler.emit_op(Opcode::Nil),
        TokenKind::True => compiler.emit_op(Opcode::True),
        TokenKind::False => compiler.emit_op(Opcode::False),
        _ => unreachable!(),
    }
}

fn variable(compiler: &mut Compiler, can_assign: bool) {
    compiler.named_variable(compiler.previous, can_assign);
}

fn and(compiler: &mut Compiler, _can_assign: bool) {
    let end_jmp = compiler.emit_jump(Opcode::JumpIfFalse);

    compiler.emit_op(Opcode::Pop);
    compiler.precedence(Precedence::And);

    compiler.patch_jump(end_jmp);
}

fn or(compiler: &mut Compiler, _can_assign: bool) {
    let else_jmp = compiler.emit_jump(Opcode::JumpIfFalse);
    let end_jmp = compiler.emit_jump(Opcode::Jump);

    compiler.patch_jump(else_jmp);
    compiler.emit_op(Opcode::Pop);

    compiler.precedence(Precedence::Or);
    compiler.patch_jump(end_jmp);
}

fn call(compiler: &mut Compiler, _can_assign: bool) {
    let args = compiler.arg_list();
    compiler.emit_raw(&[Opcode::Call as u8, args]);
}

fn dot(compiler: &mut Compiler, can_assign: bool) {
    compiler.consume(TokenKind::Identifier, "Expected property name");
    let name = compiler.identifier_constant(compiler.previous);

    if can_assign && compiler.maybe(TokenKind::Equal) {
        compiler.expression();
        compiler.emit_raw(&[Opcode::SetProp as u8, name]);
    } else {
        compiler.emit_raw(&[Opcode::GetProp as u8, name]);
    }
}

fn this(compiler: &mut Compiler, _can_assign: bool) {
    if compiler.state.top().class.is_none() {
        compiler.error("Cannot use 'this' outside of a class", compiler.previous);
        return;
    }
    variable(compiler, false);
}

fn super_(compiler: &mut Compiler, _can_assign: bool) {
    if compiler.state.top().class.is_none() {
        compiler.error("Cannot use 'super' outside of a class", compiler.previous);
    }
    if let Some(superclass) = compiler.state.top().superclass {
        compiler.consume(TokenKind::Dot, "Expected '.'");
        compiler.consume(TokenKind::Identifier, "Expected superclass method name");
        let name = compiler.identifier_constant(compiler.previous);

        let line = compiler.previous.line;
        compiler.named_variable(Token::new(TokenKind::Identifier, "this", line), false);
        compiler.named_variable(superclass, false);
        compiler.emit_raw(&[Opcode::GetSuper as u8, name]);
    } else {
        compiler.error("Cannot use 'super' in a class with no superclass", compiler.previous);
    }
}

#[rustfmt::skip]
fn get_rule(kind: TokenKind) -> Rule {
    match kind {
        TokenKind::LeftParen      => Rule::new(Some(&grouping),   Some(&call),      Precedence::Call),
        TokenKind::RightParen     => Rule::new(None,              None,             Precedence::None),
        TokenKind::LeftBrace      => Rule::new(None,              None,             Precedence::None),
        TokenKind::RightBrace     => Rule::new(None,              None,             Precedence::None),
        TokenKind::Comma          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Dot            => Rule::new(None,              Some(&dot),       Precedence::Call),
        TokenKind::Minus          => Rule::new(Some(&unary),      Some(&binary),    Precedence::Term),
        TokenKind::Plus           => Rule::new(None,              Some(&binary),    Precedence::Term),
        TokenKind::Semicolon      => Rule::new(None,              None,             Precedence::None),
        TokenKind::Slash          => Rule::new(None,              Some(&binary),    Precedence::Factor),
        TokenKind::Star           => Rule::new(None,              Some(&binary),    Precedence::Factor),
        TokenKind::Bang           => Rule::new(Some(&unary),      None,             Precedence::None),
        TokenKind::BangEqual      => Rule::new(None,              Some(&binary),    Precedence::Equal),
        TokenKind::Equal          => Rule::new(None,              None,             Precedence::None),
        TokenKind::EqualEqual     => Rule::new(None,              Some(&binary),    Precedence::Equal),
        TokenKind::Greater        => Rule::new(None,              Some(&binary),    Precedence::Compare),
        TokenKind::GreaterEqual   => Rule::new(None,              Some(&binary),    Precedence::Compare),
        TokenKind::Less           => Rule::new(None,              Some(&binary),    Precedence::Compare),
        TokenKind::LessEqual      => Rule::new(None,              Some(&binary),    Precedence::Compare),
        TokenKind::Identifier     => Rule::new(Some(&variable),   None,             Precedence::None),
        TokenKind::String         => Rule::new(Some(&string),     None,             Precedence::None),
        TokenKind::Number         => Rule::new(Some(&number),     None,             Precedence::None),
        TokenKind::And            => Rule::new(None,              Some(&and),       Precedence::And),
        TokenKind::Class          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Else           => Rule::new(None,              None,             Precedence::None),
        TokenKind::False          => Rule::new(Some(&literal),    None,             Precedence::None),
        TokenKind::For            => Rule::new(None,              None,             Precedence::None),
        TokenKind::Fun            => Rule::new(None,              None,             Precedence::None),
        TokenKind::If             => Rule::new(None,              None,             Precedence::None),
        TokenKind::Nil            => Rule::new(Some(&literal),    None,             Precedence::None),
        TokenKind::Or             => Rule::new(None,              Some(&or),        Precedence::Or),
        TokenKind::Print          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Return         => Rule::new(None,              None,             Precedence::None),
        TokenKind::Super          => Rule::new(Some(&super_),     None,             Precedence::None),
        TokenKind::This           => Rule::new(Some(&this),       None,             Precedence::None),
        TokenKind::True           => Rule::new(Some(&literal),    None,             Precedence::None),
        TokenKind::Var            => Rule::new(None,              None,             Precedence::None),
        TokenKind::While          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Error          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Eof            => Rule::new(None,              None,             Precedence::None),
    }
}

#[derive(Clone, Copy)]
struct Rule {
    prefix: Option<&'static dyn Fn(&mut Compiler, bool)>,
    infix: Option<&'static dyn Fn(&mut Compiler, bool)>,
    precedence: Precedence,
}

impl Debug for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Rule({}, {}, {:?})",
            self.prefix.is_some(),
            self.infix.is_some(),
            self.precedence
        )
    }
}

#[derive(Clone, Copy, Debug)]
struct Local<'a> {
    name: Token<'a>,
    depth: isize,
    is_captured: bool,
}

impl<'a> Display for Local<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}, [{}])", self.name.lexeme, self.depth)
    }
}

#[derive(Debug)]
struct Locals<'a> {
    stack: Stack<Local<'a>>,
    current_depth: usize,
}

impl<'a> Locals<'a> {
    fn new() -> Locals<'a> {
        Locals {
            stack: Stack::new(),
            current_depth: 0,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Upvalue {
    is_local: bool,
    index: u8,
}

impl Rule {
    fn new(
        prefix: Option<&'static dyn Fn(&mut Compiler, bool)>,
        infix: Option<&'static dyn Fn(&mut Compiler, bool)>,
        precedence: Precedence,
    ) -> Rule {
        Rule {
            prefix,
            infix,
            precedence,
        }
    }
}

#[allow(dead_code)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assign,
    Or,
    And,
    Equal,
    Compare,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
    Max = 254u8,
}

impl Add<u8> for Precedence {
    type Output = Precedence;
    fn add(self, rhs: u8) -> Self::Output {
        let out = rhs as u8 + self as u8;
        if out >= Precedence::Max as u8 {
            panic!("Invalid precedence");
        }
        unsafe { std::mem::transmute(out) }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionKind {
    Script,
    Function,
    Method,
    Initializer,
}
