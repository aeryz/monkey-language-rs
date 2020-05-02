use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Expression, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    Prefix(String, Box<Expression>),
    Infix(Box<Expression>, String, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    FunctionLiteral(Vec<Box<Expression>>, Box<Statement>),
    Call(Box<Expression>, Vec<Box<Expression>>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{}", val),
            Expression::IntegerLiteral(val) => write!(f, "{}", val),
            Expression::Prefix(op, exp) => write!(f, "({}{})", op, exp),
            Expression::Infix(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::Boolean(val) => write!(f, "{}", val),
            Expression::If(cond, cons, alter) => {
                if let Some(alter) = alter {
                    write!(f, "if {}{{{}}} else {{{}}}", cond, cons, alter)
                } else {
                    write!(f, "if {} {{{}}}", cond, cons)
                }
            }
            Expression::FunctionLiteral(params, statement) => {
                let mut out = String::from("fn (");

                if params.len() == 0 {
                    return write!(f, "{})", out);
                }

                for p in params {
                    out = format!("{}{}, ", out, p);
                }
                let out = (&out[0..out.len() - 2]).to_string();
                write!(f, "{}) {{{}}}", out, statement)
            }
            Expression::Call(function, arguments) => {
                let mut out = format!("{}(", *function);

                if arguments.len() == 0 {
                    return write!(f, "{})", out);
                }

                for a in arguments {
                    out = format!("{}{}, ", out, a);
                }

                let out = (&out[0..out.len() - 2]).to_string();
                write!(f, "{})", out)
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(name, val) => write!(f, "let {} = {};", name, val),
            Statement::Return(val) => write!(f, "return {};", val),
            Statement::Expression(exp) => write!(f, "{};", exp),
            Statement::Block(stmts) => {
                let mut out = String::new();
                for stmt in stmts {
                    out = format!("{}{}", out, stmt);
                }
                write!(f, "{}", &out)
            }
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        for s in &self.statements {
            out = format!("{} {}", out, s);
        }
        write!(f, "{}", out)
    }
}
