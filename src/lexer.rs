use crate::token::{Token, TokenType};

pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            // TODO: This might be a function
            Some('=') => {
                let tok: Token;
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    tok = Token::new(TokenType::EQ, "==".to_owned());
                } else {
                    tok = Token::new(TokenType::ASSIGN, "=".to_owned());
                }
                tok
            }
            Some('!') => {
                let tok: Token;
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    tok = Token::new(TokenType::NOT_EQ, "!=".to_owned());
                } else {
                    tok = Token::new(TokenType::BANG, "!".to_owned());
                }
                tok
            }
            Some('+') => Token::new(TokenType::PLUS, "+".to_owned()),
            Some('-') => Token::new(TokenType::MINUS, "-".to_owned()),
            Some('/') => Token::new(TokenType::SLASH, "/".to_owned()),
            Some('*') => Token::new(TokenType::ASTERISK, "*".to_owned()),
            Some('<') => Token::new(TokenType::LT, "<".to_owned()),
            Some('>') => Token::new(TokenType::GT, ">".to_owned()),
            Some(';') => Token::new(TokenType::SEMICOLON, ";".to_owned()),
            Some(',') => Token::new(TokenType::COMMA, ",".to_owned()),
            Some('{') => Token::new(TokenType::LBRACE, "{".to_owned()),
            Some('}') => Token::new(TokenType::RBRACE, "}".to_owned()),
            Some('(') => Token::new(TokenType::LPAREN, "(".to_owned()),
            Some(')') => Token::new(TokenType::RPAREN, ")".to_owned()),
            None => Token::new(TokenType::EOF, String::new()),
            Some(ch) => {
                if Token::is_letter(ch) {
                    let identifier = self.read_identifier();
                    return Token::from_ident(identifier);
                } else if Token::is_number(ch) {
                    return Token::new(TokenType::INT, self.read_number());
                }
                Token::new(
                    TokenType::ILLEGAL,
                    String::from(&self.input[self.position..self.position + 1]),
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

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;

        while let Some(ch) = self.ch {
            if !Token::is_letter(ch) {
                break;
            }
            self.read_char();
        }
        String::from(&self.input[position..self.position])
    }

    pub fn read_number(&mut self) -> String {
        let position = self.position;

        while let Some(ch) = self.ch {
            if !Token::is_number(ch) {
                break;
            }
            self.read_char();
        }
        String::from(&self.input[position..self.position])
    }
}
