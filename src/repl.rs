use crate::evaluator::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;
use std::io::Write;
use std::rc::Rc;

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

        let eval = Evaluator::new();

        match eval.eval_statements(&program.statements) {
            Ok(obj) => println!("{}", obj),
            Err(msg) => println!("ERROR: {}", msg),
        }
    }
}
