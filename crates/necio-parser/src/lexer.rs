#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Identifier(String),
    Colon,
    Assign, // :=
    TypeInteger,
    TypeString,
    TypeFloat,
    TypeBoolean,
    TypeVoid,
    StringLiteral(String),
    NumberLiteral(String),
    FloatLiteral(String),
    Plus,
    Minus, // -
    Star, // *
    Slash, // /
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket, // [
    RBracket, // ]
    Comma,
    SemiColon,
    Return,
    If,
    Else,
    Do,
    While,
    True,
    False,
    Interface,
    Enum,
    Class,
    Public,
    Private,
    This,
    Dot, // .
    LT, // <
    GT, // >
    EqEq, // ==
    Increment, // ++
    EOF,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.position >= self.input.len() {
            return Token::EOF;
        }

        let ch = self.input[self.position];

        match ch {
            '/' => {
                if self.peek() == '/' {
                    self.skip_comment();
                    self.next_token()
                } else {
                    self.position += 1;
                    Token::Slash
                }
            }
            ':' => {
                if self.peek() == '=' {
                    self.position += 2;
                    Token::Assign
                } else {
                    self.position += 1;
                    Token::Colon
                }
            }
            ';' => {
                self.position += 1;
                Token::SemiColon
            }
            '+' => {
                if self.peek() == '+' {
                    self.position += 2;
                    Token::Increment
                } else {
                    self.position += 1;
                    Token::Plus
                }
            }
            '-' => {
                self.position += 1;
                Token::Minus
            }
            '*' => {
                self.position += 1;
                Token::Star
            }
            '(' => {
                self.position += 1;
                Token::LParen
            }
            ')' => {
                self.position += 1;
                Token::RParen
            }
            '{' => {
                self.position += 1;
                Token::LBrace
            }
            '}' => {
                self.position += 1;
                Token::RBrace
            }
            '[' => {
                self.position += 1;
                Token::LBracket
            }
            ']' => {
                self.position += 1;
                Token::RBracket
            }
            ',' => {
                self.position += 1;
                Token::Comma
            }
            '.' => {
                self.position += 1;
                Token::Dot
            }
            '<' => {
                self.position += 1;
                Token::LT
            }
            '>' => {
                self.position += 1;
                Token::GT
            }
            '=' => {
                if self.peek() == '=' {
                    self.position += 2;
                    Token::EqEq
                } else {
                    self.position += 1;
                    panic!("Unexpected char =");
                }
            }
            '"' => self.read_string(),
            _ if ch.is_alphabetic() => self.read_identifier(),
            _ if ch.is_numeric() => self.read_number(),
            _ => {
                self.position += 1;
                self.next_token()
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() && self.input[self.position].is_whitespace() {
            self.position += 1;
        }
    }

    fn skip_comment(&mut self) {
        while self.position < self.input.len() && self.input[self.position] != '\n' {
            self.position += 1;
        }
    }

    fn peek(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input[self.position + 1]
        }
    }

    fn read_string(&mut self) -> Token {
        self.position += 1; // Skip opening quote
        let start = self.position;
        while self.position < self.input.len() && self.input[self.position] != '"' {
            self.position += 1;
        }
        let value: String = self.input[start..self.position].iter().collect();
        self.position += 1; // Skip closing quote
        Token::StringLiteral(value)
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.position;
        while self.position < self.input.len() && (self.input[self.position].is_alphanumeric() || self.input[self.position] == '_') {
            self.position += 1;
        }
        let value: String = self.input[start..self.position].iter().collect();
        match value.as_str() {
            "let" => Token::Let,
            "integer" => Token::TypeInteger,
            "string" => Token::TypeString,
            "float" => Token::TypeFloat,
            "boolean" => Token::TypeBoolean,
            "void" => Token::TypeVoid,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "do" => Token::Do,
            "while" => Token::While,
            "true" => Token::True,
            "false" => Token::False,
            "interface" => Token::Interface,
            "enum" => Token::Enum,
            "class" => Token::Class,
            "public" => Token::Public,
            "private" => Token::Private,
            "this" => Token::This,
            _ => Token::Identifier(value),
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.position;
        let mut is_float = false;
        while self.position < self.input.len() && (self.input[self.position].is_numeric() || self.input[self.position] == '.') {
            if self.input[self.position] == '.' {
                is_float = true;
            }
            self.position += 1;
        }
        let value: String = self.input[start..self.position].iter().collect();
        if is_float {
            Token::FloatLiteral(value)
        } else {
            Token::NumberLiteral(value)
        }
    }
}
