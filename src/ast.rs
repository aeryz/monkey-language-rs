use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let(Expression<'a>, Expression<'a>),
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Block(Vec<Statement<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(&'a str),
    IntegerLiteral(i64),
    Prefix(&'a str, Box<Expression<'a>>),
    Infix(Box<Expression<'a>>, &'a str, Box<Expression<'a>>),
    Boolean(bool),
    If(
        Box<Expression<'a>>,
        Box<Statement<'a>>,
        Option<Box<Statement<'a>>>,
    ),
    FunctionLiteral(Vec<Box<Expression<'a>>>, Box<Statement<'a>>),
}

impl<'a> fmt::Display for Expression<'a> {
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
                for p in params {
                    out = format!("{}, {}", out, p);
                }
                write!(f, "{}) {{{}}}", out, statement)
            }
        }
    }
}

impl<'a> fmt::Display for Statement<'a> {
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

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
