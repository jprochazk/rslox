#[derive(Clone, Copy, Debug)]
pub struct Scanner<'a> {
    pub source: &'a str,
    pub start: usize,
    pub current: usize,
    pub line: usize,
}

macro_rules! token {
    ($self:ident, Error, $reason:literal) => {{
        Token::new(TokenKind::Error, $reason, $self.line)
    }};
    ($self:ident, Identifier) => {{
        Token::new(
            $self.ident_kind(),
            &$self.source[$self.start..$self.current],
            $self.line,
        )
    }};
    ($self:ident, $kind:ident) => {{
        Token::new(
            TokenKind::$kind,
            &$self.source[$self.start..$self.current],
            $self.line,
        )
    }};
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        // TODO this will panic for anything non-ascii
        self.source.as_bytes()[self.current - 1] as char
    }

    fn peek(&self) -> char {
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next(&self) -> char {
        self.source.as_bytes()[self.current + 1] as char
    }

    fn end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn remaining(&self) -> usize {
        self.source.len() - self.current
    }

    fn matches(&mut self, it: char) -> bool {
        if self.end() || self.peek() != it {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn lexeme(&self) -> &str {
        &self.source[self.start..self.current]
    }

    fn skip_whitespace(&mut self) {
        while !self.end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.end() {
                            self.advance();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            };
        }
    }

    fn string(&mut self) -> Token<'a> {
        while self.peek() != '"' && !self.end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.end() {
            return token!(self, Error, "Unterminated string");
        }

        self.advance();
        token!(self, String)
    }

    fn number(&mut self) -> Token<'a> {
        while !self.end() && self.peek().is_digit(10) {
            self.advance();
        }

        if self.remaining() >= 2 && self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();

            while !self.end() && self.peek().is_digit(10) {
                self.advance();
            }
        }

        token!(self, Number)
    }

    fn ident_kind(&mut self) -> TokenKind {
        match self.lexeme() {
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier,
        }
    }

    fn ident(&mut self) -> Token<'a> {
        while !self.end() && self.peek().is_alphanumeric() {
            self.advance();
        }

        token!(self, Identifier)
    }

    pub fn next(&mut self) -> Token<'a> {
        self.skip_whitespace();
        if self.end() {
            return token!(self, Eof);
        }

        self.start = self.current;
        let c = self.advance();
        if c.is_alphabetic() {
            return self.ident();
        }
        if c.is_digit(10) {
            return self.number();
        }
        match c {
            // single character
            '(' => token!(self, LeftParen),
            ')' => token!(self, RightParen),
            '{' => token!(self, LeftBrace),
            '}' => token!(self, RightBrace),
            ';' => token!(self, Semicolon),
            ',' => token!(self, Comma),
            '.' => token!(self, Dot),
            '-' => token!(self, Minus),
            '+' => token!(self, Plus),
            '/' => token!(self, Slash),
            '*' => token!(self, Star),
            '!' if self.matches('=') => token!(self, BangEqual),
            '!' => token!(self, Bang),
            '=' if self.matches('=') => token!(self, EqualEqual),
            '=' => token!(self, Equal),
            '<' if self.matches('=') => token!(self, LessEqual),
            '<' => token!(self, Less),
            '>' if self.matches('=') => token!(self, GreaterEqual),
            '>' => token!(self, Greater),
            '"' => self.string(),
            _ => token!(self, Error, "Unexpected character"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Token<'a> {
        Token { kind, lexeme, line }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    LeftParen, // Single-character tokens.
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang, // One or two character tokens.
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier, // Literals.
    String,
    Number,
    And, // Keywords.
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
}
