use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;
use std::io::Write;

const PROMPT: &str = ">>";

fn print_parser_errors(errors: &Vec<String>) {
    for e in errors {
        println!("{}", e);
    }
}

pub fn start() {
    loop {
        print!("{} ", PROMPT);
        std::io::stdout().flush().expect("Flush error.");
        let mut buffer = String::new();

        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Unexpected error occured.");

        let l = Lexer::new(&buffer);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
        }

        println!("{}", program);
    }
}
