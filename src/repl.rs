use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::Write;

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().expect("Flush error.");
        let mut buffer = String::new();

        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Unexpected error occured.");

        let mut l = Lexer::new(&buffer);

        loop {
            let tok = l.next_token();
            if tok.tok_type == TokenType::EOF {
                break;
            }
            println!("{:?}", tok);
        }
    }
}
