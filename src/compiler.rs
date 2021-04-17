use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::Add,
};

use crate::{
    chunk::disassemble_chunk,
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
    locals: Locals<'a>,
    upvalues: Stack<Upvalue>,
}

impl<'a> CompilerState<'a> {
    pub fn new(kind: FunctionKind) -> CompilerState<'a> {
        CompilerState {
            func: Function::new(),
            kind,
            locals: Locals::new(),
            upvalues: Stack::new(),
        }
    }
}

macro_rules! emit {
    ($compiler:ident, $chunk:expr, const $value:ident) => {{
        let offset = $chunk.push_const($value);
        if offset >= 254 {
            $compiler.error("Too many constants in one chunk", $compiler.previous);
        }
        offset
    }};
    ($compiler:ident, $chunk:expr, const $value:ident + push) => {{
        let offset = $chunk.push_const($value);
        if offset >= std::u8::MAX {
            $compiler.error("Too many constants in one chunk", $compiler.previous);
        }
        $chunk.push_byte(Opcode::Constant as u8, $compiler.previous.line);
        $chunk.push_byte(offset, $compiler.previous.line);
        offset
    }};
    ($compiler:ident, $chunk:expr, loop $start:ident) => {{
        emit!($compiler, $chunk, op Loop);
        let offset = $chunk.buffer.len() - $start + 2;
        if offset > std::u16::MAX as usize {
            $compiler.error("Loop body too large", $compiler.previous);
        }
        let bytes = offset.to_ne_bytes();
        emit!($compiler, $chunk, raw bytes[0], bytes[1]);
    }};
    ($compiler:ident, $chunk:expr, jmp $kind:ident) => {{
        emit!($compiler, $chunk, raw Opcode::$kind, 0xFF, 0xFF);
        $chunk.buffer.len() - 2
    }};
    ($compiler:ident, $chunk:expr, raw $kind:expr) => {{
        $chunk.push_byte($kind.into(), $compiler.previous.line)
    }};
    ($compiler:ident, $chunk:expr, raw $($what:expr),+) => {{
        $chunk.push_bytes(
            &[$($what.into()),+],
            $compiler.previous.line
        )
    }};
    ($compiler:ident, $chunk:expr, op $kind:ident) => {{
        $chunk.push_byte(Opcode::$kind.into(), $compiler.previous.line)
    }};
    ($compiler:ident, $chunk:expr, op $($kind:ident),+) => {{
        $chunk.push_bytes(
            &[$(Opcode::$kind.into()),+],
            $compiler.previous.line
        )
    }};
}

impl<'a> Compiler<'a> {
    fn new(source: &'a str) -> Compiler<'a> {
        let mut state = Stack::new();
        state.push(CompilerState::new(FunctionKind::Script));
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

    fn begin_scope(&mut self) {
        self.state.top_mut().locals.current_depth += 1;
    }

    fn end_scope(&mut self) {
        self.state.top_mut().locals.current_depth -= 1;

        while self.state.top_mut().locals.stack.len() > 0
            && self.state.top_mut().locals.stack.top().depth > self.state.top_mut().locals.current_depth as isize
        {
            /* if self.state.top_mut().locals.stack.top().is_captured {
                emit!(self, self.state.top_mut().func.chunk, op CloseUpvalue);
            } else { */
            emit!(self, self.state.top_mut().func.chunk, op Pop);
            /* } */
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
        if self.maybe(TokenKind::Fun) {
            self.fun_declaration();
        } else if self.maybe(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name");
        self.mark_initialized();
        self.function(FunctionKind::Function);
        self.define_variable(global);
    }

    fn function(&mut self, kind: FunctionKind) {
        self.state.push(CompilerState::new(kind));
        let current = self.state.top_mut();
        if kind != FunctionKind::Script {
            current.func.name = self.previous.lexeme.to_string();
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
        emit!(self, state.func.chunk, op Nil, Return);

        let num_upvalues = state.func.num_upvalues;
        let value = Value::object(Object::Function(state.func));
        let offset = emit!(self, self.state.top_mut().func.chunk, const value);
        emit!(self, self.state.top_mut().func.chunk, raw Opcode::Closure, offset);
        for i in 0..num_upvalues {
            let upvalue = &state.upvalues[i as usize];
            let is_local = if upvalue.is_local { 1u8 } else { 0u8 };
            emit!(self, self.state.top_mut().func.chunk, raw is_local, upvalue.index);
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name");
        if self.maybe(TokenKind::Equal) {
            self.expression();
        } else {
            emit!(self, self.state.top_mut().func.chunk, op Nil);
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
        emit!(self, self.state.top_mut().func.chunk, raw Opcode::DefineGlobal, global);
    }

    fn identifier_constant(&mut self, name: Token<'a>) -> u8 {
        let value = Value::object(Object::String(name.lexeme.to_string()));
        emit!(self, self.state.top_mut().func.chunk, const value)
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
        if self.state.top().kind == FunctionKind::Script {
            self.error("Cannot return from top-level code", self.previous);
        }

        if self.maybe(TokenKind::Semicolon) {
            emit!(self, self.state.top_mut().func.chunk, op Nil, Return);
        } else {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expected ';'");
            emit!(self, self.state.top_mut().func.chunk, op Return);
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expected ';'");
        emit!(self, self.state.top_mut().func.chunk, op Print);
    }

    fn if_statement(&mut self) {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'if'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expected ')' after condition");

        let then = emit!(self, self.state.top_mut().func.chunk, jmp JumpIfFalse);
        emit!(self, self.state.top_mut().func.chunk, op Pop);
        self.statement();
        let otherwise = emit!(self, self.state.top_mut().func.chunk, jmp Jump);
        self.patch_jump(then);
        emit!(self, self.state.top_mut().func.chunk, op Pop);

        if self.maybe(TokenKind::Else) {
            self.statement();
            self.patch_jump(otherwise);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.state.top_mut().func.chunk.buffer.len();

        self.consume(TokenKind::LeftParen, "Expected '(' after 'while'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expected ')' after condition");

        let exit_jump = emit!(self, self.state.top_mut().func.chunk, jmp JumpIfFalse);

        emit!(self, self.state.top_mut().func.chunk, op Pop);
        self.statement();

        emit!(self, self.state.top_mut().func.chunk, loop loop_start);

        self.patch_jump(exit_jump);
        emit!(self, self.state.top_mut().func.chunk, op Pop);
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
            let exit_jmp = emit!(self, self.state.top_mut().func.chunk, jmp JumpIfFalse);
            emit!(self, self.state.top_mut().func.chunk, op Pop);
            Some(exit_jmp)
        } else {
            None
        };

        if !self.maybe(TokenKind::RightParen) {
            let body_jmp = emit!(self, self.state.top_mut().func.chunk, jmp Jump);

            let increment_start = self.state.top_mut().func.chunk.buffer.len();
            self.expression();
            emit!(self, self.state.top_mut().func.chunk, op Pop);
            self.consume(TokenKind::RightParen, "Expected ')'");

            emit!(self, self.state.top_mut().func.chunk, loop loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jmp);
        }

        self.statement();

        emit!(self, self.state.top_mut().func.chunk, loop loop_start);

        if let Some(exit_jmp) = exit_jmp {
            self.patch_jump(exit_jmp);
            emit!(self, self.state.top_mut().func.chunk, op Pop);
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
        emit!(self, self.state.top_mut().func.chunk, op Pop);
    }

    fn named_variable(&mut self, name: Token<'a>, can_assign: bool) {
        let top = self.state.len() - 1;
        let mut arg = self.resolve_local(top, name);
        let (arg, set, get) = if arg != -1 {
            (arg as u8, Opcode::SetLocal, Opcode::GetLocal)
        } else if ({
            arg = self.resolve_upvalue(top, name);
            arg
        }) != -1
        {
            (arg as u8, Opcode::SetUpvalue, Opcode::GetUpvalue)
        } else {
            (self.identifier_constant(name), Opcode::SetGlobal, Opcode::GetGlobal)
        };

        if can_assign && self.maybe(TokenKind::Equal) {
            self.expression();
            emit!(self, self.state.top_mut().func.chunk, raw set, arg);
        } else {
            emit!(self, self.state.top_mut().func.chunk, raw get, arg);
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
        TokenKind::Bang => emit!(compiler, compiler.state.top_mut().func.chunk, op Not),
        TokenKind::Minus => emit!(compiler, compiler.state.top_mut().func.chunk, op Negate),
        _ => unreachable!(),
    }
}

fn binary(compiler: &mut Compiler, _can_assign: bool) {
    let op = compiler.previous.kind;

    let rule = get_rule(op);
    compiler.precedence(rule.precedence + 1);
    match op {
        TokenKind::Plus => emit!(compiler, compiler.state.top_mut().func.chunk, op Add),
        TokenKind::Minus => emit!(compiler, compiler.state.top_mut().func.chunk, op Subtract),
        TokenKind::Star => emit!(compiler, compiler.state.top_mut().func.chunk, op Multiply),
        TokenKind::Slash => emit!(compiler, compiler.state.top_mut().func.chunk, op Divide),
        TokenKind::BangEqual => {
            emit!(compiler, compiler.state.top_mut().func.chunk, op Equal, Not)
        }
        TokenKind::EqualEqual => emit!(compiler, compiler.state.top_mut().func.chunk, op Equal),
        TokenKind::Greater => emit!(compiler, compiler.state.top_mut().func.chunk, op Greater),
        TokenKind::GreaterEqual => {
            emit!(compiler, compiler.state.top_mut().func.chunk, op Less, Not)
        }
        TokenKind::Less => emit!(compiler, compiler.state.top_mut().func.chunk, op Less),
        TokenKind::LessEqual => {
            emit!(compiler, compiler.state.top_mut().func.chunk, op Greater, Not)
        }
        _ => (),
    }
}

fn number(compiler: &mut Compiler, _can_assign: bool) {
    let value = Value::Number(compiler.previous.lexeme.parse::<f64>().unwrap());
    emit!(compiler, compiler.state.top_mut().func.chunk, const value + push);
}

fn string(compiler: &mut Compiler, _can_assign: bool) {
    let unquoted = &compiler.previous.lexeme[1..compiler.previous.lexeme.len() - 1];
    let value = Value::object(Object::String(unquoted.to_string()));
    emit!(compiler, compiler.state.top_mut().func.chunk, const value + push);
}

fn literal(compiler: &mut Compiler, _can_assign: bool) {
    match compiler.previous.kind {
        TokenKind::Nil => emit!(compiler, compiler.state.top_mut().func.chunk, op Nil),
        TokenKind::True => emit!(compiler, compiler.state.top_mut().func.chunk, op True),
        TokenKind::False => emit!(compiler, compiler.state.top_mut().func.chunk, op False),
        _ => unreachable!(),
    }
}

fn variable(compiler: &mut Compiler, can_assign: bool) {
    compiler.named_variable(compiler.previous, can_assign);
}

fn and(compiler: &mut Compiler, _can_assign: bool) {
    let end_jmp = emit!(compiler, compiler.state.top_mut().func.chunk, jmp JumpIfFalse);

    emit!(compiler, compiler.state.top_mut().func.chunk, op Pop);
    compiler.precedence(Precedence::And);

    compiler.patch_jump(end_jmp);
}

fn or(compiler: &mut Compiler, _can_assign: bool) {
    let else_jmp = emit!(compiler, compiler.state.top_mut().func.chunk, jmp JumpIfFalse);
    let end_jmp = emit!(compiler, compiler.state.top_mut().func.chunk, jmp Jump);

    compiler.patch_jump(else_jmp);
    emit!(compiler, compiler.state.top_mut().func.chunk, op Pop);

    compiler.precedence(Precedence::Or);
    compiler.patch_jump(end_jmp);
}

fn call(compiler: &mut Compiler, _can_assign: bool) {
    let args = compiler.arg_list();
    emit!(compiler, compiler.state.top_mut().func.chunk, raw Opcode::Call, args);
}

#[rustfmt::skip]
fn get_rule(kind: TokenKind) -> Rule {
    match kind {
        TokenKind::LeftParen      => Rule::new(Some(&grouping),   Some(&call),      Precedence::Call),
        TokenKind::RightParen     => Rule::new(None,              None,             Precedence::None),
        TokenKind::LeftBrace      => Rule::new(None,              None,             Precedence::None),
        TokenKind::RightBrace     => Rule::new(None,              None,             Precedence::None),
        TokenKind::Comma          => Rule::new(None,              None,             Precedence::None),
        TokenKind::Dot            => Rule::new(None,              None,             Precedence::None),
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
        TokenKind::Super          => Rule::new(None,              None,             Precedence::None),
        TokenKind::This           => Rule::new(None,              None,             Precedence::None),
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
}
