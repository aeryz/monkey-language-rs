#[derive(Hash, Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub tok_type: TokenType,
    pub literal: String,
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Token {
            tok_type: self.tok_type.clone(),
            literal: self.literal.clone(),
        }
    }
}

impl Token {
    pub fn new(tok_type: TokenType, literal: String) -> Self {
        Token { tok_type, literal }
    }

    pub fn empty() -> Self {
        Token {
            tok_type: TokenType::EOF,
            literal: String::new(),
        }
    }

    pub fn from_ident(identifier: String) -> Self {
        Token {
            tok_type: Token::str_to_ident(identifier.as_str()),
            literal: identifier,
        }
    }

    pub fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    pub fn is_number(ch: char) -> bool {
        ch.is_numeric()
    }

    pub fn str_to_ident(input: &str) -> TokenType {
        match input {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }
}
