use crate::token::{Token, TokenType};

pub struct Lexer<'input> {
    pub input: &'input str,
    pub position: usize,
    pub read_position: usize,
    pub ch: Option<char>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token<'input> {
        self.skip_whitespace();

        let tok = match self.ch {
            // TODO: This might be a function
            Some('=') => {
                let tok: Token;
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    tok = Token::new(TokenType::EQ, "==");
                } else {
                    tok = Token::new(TokenType::ASSIGN, "=");
                }
                tok
            }
            Some('!') => {
                let tok: Token;
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    tok = Token::new(TokenType::NOT_EQ, "!=");
                } else {
                    tok = Token::new(TokenType::BANG, "!");
                }
                tok
            }
            Some('+') => Token::new(TokenType::PLUS, "+"),
            Some('-') => Token::new(TokenType::MINUS, "-"),
            Some('/') => Token::new(TokenType::SLASH, "/"),
            Some('*') => Token::new(TokenType::ASTERISK, "*"),
            Some('<') => Token::new(TokenType::LT, "<"),
            Some('>') => Token::new(TokenType::GT, ">"),
            Some(';') => Token::new(TokenType::SEMICOLON, ";"),
            Some(',') => Token::new(TokenType::COMMA, ","),
            Some('{') => Token::new(TokenType::LBRACE, "{"),
            Some('}') => Token::new(TokenType::RBRACE, "}"),
            Some('(') => Token::new(TokenType::LPAREN, "("),
            Some(')') => Token::new(TokenType::RPAREN, ")"),
            None => Token::new(TokenType::EOF, ""),
            Some(ch) => {
                if Token::is_letter(ch) {
                    let identifier = self.read_identifier();
                    return Token::from_ident(identifier);
                } else if Token::is_number(ch) {
                    return Token::new(TokenType::INT, self.read_number());
                }
                Token::new(
                    TokenType::ILLEGAL,
                    &self.input[self.position..self.position + 1],
                )
            }
        };

        self.read_char();
        tok
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if !ch.is_ascii_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    pub fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position);
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    pub fn read_identifier(&mut self) -> &'input str {
        let position = self.position;

        while let Some(ch) = self.ch {
            if !Token::is_letter(ch) {
                break;
            }
            self.read_char();
        }
        &self.input[position..self.position]
    }

    pub fn read_number(&mut self) -> &'input str {
        let position = self.position;

        while let Some(ch) = self.ch {
            if !Token::is_number(ch) {
                break;
            }
            self.read_char();
        }
        &self.input[position..self.position]
    }
}
