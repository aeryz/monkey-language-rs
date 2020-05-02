use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;

macro_rules! hashmap(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

#[derive(PartialOrd, PartialEq, Copy, Clone)]
pub enum Presedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

type PrefixFn = fn(&mut Parser) -> Result<Expression, String>;
type InfixFn = fn(&mut Parser, Expression) -> Result<Expression, String>;

pub struct Parser {
    l: Lexer,

    cur_token: Token,
    peek_token: Token,

    prefix_fns: HashMap<TokenType, PrefixFn>,
    infix_fns: HashMap<TokenType, InfixFn>,

    precedences: HashMap<TokenType, Presedence>,

    errors: Vec<String>,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            l,
            cur_token: Token::empty(),
            peek_token: Token::empty(),
            errors: Vec::new(),
            prefix_fns: HashMap::new(),
            infix_fns: HashMap::new(),
            precedences: HashMap::new(),
        };

        p.precedences = hashmap! [
            TokenType::EQ => Presedence::EQUALS,
            TokenType::NOT_EQ => Presedence::EQUALS,
            TokenType::LT => Presedence::LESSGREATER,
            TokenType::GT => Presedence::LESSGREATER,
            TokenType::PLUS => Presedence::SUM,
            TokenType::MINUS => Presedence::SUM,
            TokenType::SLASH => Presedence::PRODUCT,
            TokenType::ASTERISK => Presedence::PRODUCT,
            TokenType::LPAREN => Presedence::CALL
        ];

        p.infix_fns
            .insert(TokenType::PLUS, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::MINUS, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::SLASH, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::ASTERISK, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::EQ, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::NOT_EQ, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::LT, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::GT, Parser::parse_infix_expression);
        p.infix_fns
            .insert(TokenType::LPAREN, Parser::parse_call_expression);

        p.prefix_fns
            .insert(TokenType::IDENT, Parser::parse_identifier);
        p.prefix_fns
            .insert(TokenType::INT, Parser::parse_integer_literal);
        p.prefix_fns
            .insert(TokenType::BANG, Parser::parse_prefix_expression);
        p.prefix_fns
            .insert(TokenType::MINUS, Parser::parse_prefix_expression);
        p.prefix_fns.insert(TokenType::TRUE, Parser::parse_boolean);
        p.prefix_fns.insert(TokenType::FALSE, Parser::parse_boolean);
        p.prefix_fns
            .insert(TokenType::LPAREN, Parser::parse_grouped_expression);
        p.prefix_fns
            .insert(TokenType::RPAREN, Parser::parse_grouped_expression);
        p.prefix_fns
            .insert(TokenType::IF, Parser::parse_if_expression);
        p.prefix_fns
            .insert(TokenType::FUNCTION, Parser::parse_function_literal);

        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.tok_type != TokenType::EOF {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.next_token();
        }
        program
    }

    pub fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, String> {
        Ok(Expression::Call(
            Box::new(function),
            self.parse_call_arguments()?,
        ))
    }

    pub fn parse_call_arguments(&mut self) -> Result<Vec<Box<Expression>>, String> {
        let mut args = Vec::new();

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(Box::new(self.parse_expression(Presedence::LOWEST)?));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(Box::new(self.parse_expression(Presedence::LOWEST)?));
        }

        self.expect_peek(TokenType::RPAREN)?;

        Ok(args)
    }

    pub fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.expect_peek(TokenType::LPAREN)?;

        self.next_token();
        let cond = self.parse_expression(Presedence::LOWEST)?;

        self.expect_peek(TokenType::RPAREN)?;

        self.expect_peek(TokenType::LBRACE)?;

        let consequence = self.parse_block_statement()?;

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            self.expect_peek(TokenType::LBRACE)?;

            return Ok(Expression::If(
                Box::new(cond),
                Box::new(consequence),
                Some(Box::new(self.parse_block_statement()?)),
            ));
        }

        Ok(Expression::If(Box::new(cond), Box::new(consequence), None))
    }

    pub fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        self.next_token();

        let exp = self.parse_expression(Presedence::LOWEST);

        self.expect_peek(TokenType::RPAREN)?;

        exp
    }

    pub fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let literal = self.cur_token.literal.clone();
        let precedence = *self.cur_precedence();
        self.next_token();
        Ok(Expression::Infix(
            Box::new(left),
            literal,
            Box::new(self.parse_expression(precedence)?),
        ))
    }

    pub fn parse_boolean(&mut self) -> Result<Expression, String> {
        if self.cur_token_is(TokenType::TRUE) {
            return Ok(Expression::Boolean(true));
        } else if self.cur_token_is(TokenType::FALSE) {
            return Ok(Expression::Boolean(false));
        }
        Err(format!(
            "Expected true of false, got: {}",
            self.cur_token.literal
        ))
    }

    pub fn parse_identifier(&mut self) -> Result<Expression, String> {
        Ok(Expression::Identifier(self.cur_token.literal.clone()))
    }

    pub fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let operator = self.cur_token.literal.clone();

        self.next_token();

        Ok(Expression::Prefix(
            operator,
            Box::new(self.parse_expression(Presedence::PREFIX)?),
        ))
    }

    pub fn parse_expression(&mut self, p: Presedence) -> Result<Expression, String> {
        let prefix_fn = match self.prefix_fns.get(&self.cur_token.tok_type) {
            Some(func) => func,
            None => {
                return Err(format!(
                    "No prefix function defined for {:?}",
                    self.cur_token.tok_type,
                ));
            }
        };
        let mut left_exp = prefix_fn(self)?;

        while !self.peek_token_is(&TokenType::SEMICOLON) && &p < self.peek_precedence() {
            let infix_fn = match self.infix_fns.get(&self.peek_token.tok_type) {
                Some(func) => func.clone(),
                None => return Ok(left_exp),
            };

            self.next_token();

            left_exp = infix_fn(self, left_exp)?;
        }

        Ok(left_exp)
    }

    pub fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        let int_val = match self.cur_token.literal.parse::<i64>() {
            Ok(val) => val,
            Err(err) => {
                return Err(err.to_string());
            }
        };

        Ok(Expression::IntegerLiteral(int_val))
    }

    pub fn parse_function_literal(&mut self) -> Result<Expression, String> {
        self.expect_peek(TokenType::LPAREN)?;

        let params = self.parse_function_parameters();

        self.expect_peek(TokenType::LBRACE)?;

        Ok(Expression::FunctionLiteral(
            params?,
            Box::new(self.parse_block_statement()?),
        ))
    }

    pub fn parse_function_parameters(&mut self) -> Result<Vec<Box<Expression>>, String> {
        let mut identifiers: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(Box::new(Expression::Identifier(
            self.cur_token.literal.clone(),
        )));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();

            identifiers.push(Box::new(Expression::Identifier(
                self.cur_token.literal.clone(),
            )));
        }

        self.expect_peek(TokenType::RPAREN)?;

        Ok(identifiers)
    }

    // PARSING STATEMENTS
    pub fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let mut stmts: Vec<Statement> = Vec::new();
        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            self.next_token();
        }
        Ok(Statement::Block(stmts))
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token.tok_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let stmt = Statement::Expression(self.parse_expression(Presedence::LOWEST)?);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }
    pub fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let ret_val = self.parse_expression(Presedence::LOWEST)?;

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::Return(ret_val))
    }

    pub fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.expect_peek(TokenType::IDENT)?;

        let identifier = Expression::Identifier(self.cur_token.literal.clone());

        self.expect_peek(TokenType::ASSIGN)?;

        self.next_token();

        let value = self.parse_expression(Presedence::LOWEST)?;

        if self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let(identifier, value))
    }

    // HELPER FUNCTIONS
    pub fn cur_token_is(&mut self, tok_type: TokenType) -> bool {
        self.cur_token.tok_type == tok_type
    }

    pub fn peek_token_is(&mut self, tok_type: &TokenType) -> bool {
        self.peek_token.tok_type == *tok_type
    }

    pub fn expect_peek(&mut self, tok_type: TokenType) -> Result<(), String> {
        if self.peek_token_is(&tok_type) {
            self.next_token();
            Ok(())
        } else {
            Err(format!(
                "Expected next token to be {:?}, got {:?} instead.",
                tok_type, self.peek_token.tok_type
            ))
        }
    }

    pub fn cur_precedence(&mut self) -> &Presedence {
        self.precedences
            .get(&self.cur_token.tok_type)
            .unwrap_or(&Presedence::LOWEST)
    }

    pub fn peek_precedence(&mut self) -> &Presedence {
        self.precedences
            .get(&self.peek_token.tok_type)
            .unwrap_or(&Presedence::LOWEST)
    }

    pub fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.l.next_token());
    }

    // GETTERS
    pub fn errors(&mut self) -> &Vec<String> {
        &self.errors
    }
}

#[cfg(test)]
pub mod tests {
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn parse_let_stmt() {
        // TODO: unimplemented
        let input = String::from("let x = 5;");

        let mut l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let mut program = parser.parse_program();
    }

    #[test]
    fn parse_identifier_expression() {
        let input = String::from("foobar");

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        assert_eq!(
            *stmt,
            Statement::Expression(Expression::Identifier(String::from("foobar")))
        );
    }

    #[test]
    fn parse_integer_expression() {
        let input = String::from("5;");

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        assert_eq!(*stmt, Statement::Expression(Expression::IntegerLiteral(5)));
    }

    #[test]
    fn parse_prefix_statement() {
        let prefix_tests = vec![
            (String::from("!5;"), "!", 5),
            (String::from("-15;"), "-", 15),
        ];
        for pt in prefix_tests {
            let l = Lexer::new(pt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            println!("{}", program.statements[0]);

            let (op, expression) = match &program.statements[0] {
                Statement::Expression(Expression::Prefix(op, expression)) => {
                    (Some(op), Some(expression))
                }
                _ => (None, None),
            };

            let op = op.unwrap();
            let expression = expression.unwrap();

            assert_eq!(op, &pt.1);
            assert_eq!(**expression, Expression::IntegerLiteral(pt.2));
        }
    }

    #[test]
    fn parse_boolean() {
        let bool_tests = vec![(String::from("true"), true), (String::from("false"), false)];

        for bt in bool_tests {
            let lexer = Lexer::new(bt.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(program.statements.len(), 1);
            assert_eq!(
                program.statements[0],
                Statement::Expression(Expression::Boolean(bt.1))
            );
        }
    }

    #[test]
    fn parse_infix_statement() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];
        for pt in infix_tests {
            let l = Lexer::new(String::from(pt.0));
            let mut p = Parser::new(l);
            let program = p.parse_program();

            println!("Values are: {:?}", p.errors);

            assert_eq!(program.statements.len(), 1);

            println!("{}", program.statements[0]);

            let (lhs, op, rhs) = match &program.statements[0] {
                Statement::Expression(Expression::Infix(lhs, op, rhs)) => {
                    (Some(lhs), Some(op), Some(rhs))
                }
                _ => (None, None, None),
            };

            let lhs = lhs.unwrap();
            let op = op.unwrap();
            let rhs = rhs.unwrap();

            assert_eq!(op, &pt.2);
            assert_eq!(**lhs, Expression::IntegerLiteral(pt.1));
            assert_eq!(**rhs, Expression::IntegerLiteral(pt.3));
        }
    }

    #[test]
    fn parse_function_literal() {
        let input = String::from("fn(x, y, z) { x + y + z; })");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let (params, block) = match &program.statements[0] {
            Statement::Expression(Expression::FunctionLiteral(p, b)) => (Some(p), Some(b)),
            _ => (None, None),
        };

        let params = params.unwrap();
        let block = match &**block.unwrap() {
            Statement::Block(b) => Some(b),
            _ => None,
        };

        assert_eq!(*params[0], Expression::Identifier("x".to_owned()));
        assert_eq!(*params[1], Expression::Identifier("y".to_owned()));
        assert_eq!(*params[2], Expression::Identifier("z".to_owned()));
    }

    #[test]
    fn parse_call_expression() {
        let input = String::from("test(1, 2 + 4, 3 * 7)");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 1);

        let (function, arguments) = match &program.statements[0] {
            Statement::Expression(Expression::Call(f, a)) => (Some(f), Some(a)),
            _ => (None, None),
        };

        let function = function.unwrap();
        let arguments = arguments.unwrap();

        assert_eq!(**function, Expression::Identifier("test".to_owned()));
        assert_eq!(*arguments[0], Expression::IntegerLiteral(1));
    }
}
