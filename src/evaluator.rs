use crate::ast::{Expression, Statement};
use crate::environment::Environment;
use crate::object::Object;

pub struct Evaluator {
    pub environment: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Environment::new(),
        }
    }

    pub fn eval_statements(&mut self, statements: Vec<Statement>) -> Result<Object, String> {
        let mut res = Object::Null;
        for s in statements.into_iter() {
            res = self.eval_statement(s)?;

            if let Object::Return(..) = res {
                return Ok(res);
            }
        }

        Ok(res)
    }

    pub fn eval_statement(&mut self, stmt: Statement) -> Result<Object, String> {
        match stmt {
            Statement::Expression(expr) => self.eval_expression(expr),
            Statement::Block(stmts) => self.eval_statements(stmts),
            Statement::Return(expr) => Ok(Object::Return(Box::new(self.eval_expression(expr)?))),
            Statement::Let(Expression::Identifier(name), expr) => {
                println!("Hererer");
                let val = self.eval_expression(expr)?;
                self.environment.set(String::from(name), &val);
                Ok(Object::Null)
            }
            _ => Err(String::from("Invalid statement")),
        }
    }

    pub fn eval_expression(&mut self, expr: Expression) -> Result<Object, String> {
        match expr {
            Expression::IntegerLiteral(val) => Ok(Object::Integer(val)),
            Expression::Boolean(val) => Ok(self.native_bool_to_object(val)),
            Expression::Prefix(op, expr) => {
                let right = self.eval_expression(*expr)?;
                return self.eval_prefix_expression(&op, right);
            }
            Expression::Infix(lhs, op, rhs) => {
                let lhs = self.eval_expression(*lhs)?;
                let rhs = self.eval_expression(*rhs)?;
                return self.eval_infix_expression(lhs, &op, rhs);
            }
            Expression::If(cond, cons, alt) => {
                let cond = self.eval_expression(*cond)?;
                if self.is_truthy(cond) {
                    return self.eval_statement(*cons);
                } else if let Some(alternative) = alt {
                    return self.eval_statement(*alternative);
                } else {
                    return Ok(Object::Null);
                }
            }
            Expression::Identifier(name) => {
                return if let Some(obj) = self.environment.get(&name) {
                    Ok(obj.clone())
                } else {
                    Err(format!("Identifier not found: {}", name))
                }
            }
            Expression::FunctionLiteral(params, stmt) => {
                let mut p: Vec<String> = Vec::new();
                for prm in params {
                    if let Expression::Identifier(ident) = *prm {
                        p.push(String::from(ident));
                    } else {
                        return Err(String::new());
                    }
                }

                Ok(Object::Function(p, stmt, Environment::new()))
            }
            Expression::Call(function, params) => {
                let func = self.eval_expression(*function)?;
                let args = self.eval_expressions(params)?;

                self.apply_function(func, args)
            }
            // _ => Err(String::from("Invalid expression")),
        }
    }

    pub fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Result<Object, String> {
        match func {
            Object::Function(params, body, _) => {
                for (i, p) in params.iter().enumerate() {
                    // TODO: Need to have a valid environment structure.
                    self.environment.set(p.clone(), &args[i]);
                }
                self.eval_statement(*body)
            }
            _ => Err(String::new()),
        }
    }

    pub fn eval_expressions(&mut self, exps: Vec<Box<Expression>>) -> Result<Vec<Object>, String> {
        let mut res = Vec::new();

        for exp in exps {
            let evaluated = self.eval_expression(*exp)?;
            res.push(evaluated);
        }

        Ok(res)
    }

    pub fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Boolean(val) => val,
            Object::Null => false,
            _ => true,
        }
    }

    pub fn eval_prefix_expression(
        &mut self,
        op: &String,
        right: Object,
    ) -> Result<Object, String> {
        match op.as_str() {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_operator_expression(right),
            _ => Err(format!("Unknown operator: {}", op)),
        }
    }

    pub fn eval_bang_operator_expression(&mut self, expr: Object) -> Result<Object, String> {
        match expr {
            Object::Boolean(true) => Ok(Object::Boolean(false)),
            Object::Boolean(false) => Ok(Object::Boolean(true)),
            Object::Null => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        }
    }

    pub fn eval_minus_operator_expression(&mut self, expr: Object) -> Result<Object, String> {
        match expr {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            _ => Err(String::from("Expected an integer")),
        }
    }

    pub fn eval_infix_expression(
        &mut self,
        left: Object,
        op: &String,
        right: Object,
    ) -> Result<Object, String> {
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                if op == "+" {
                    return Ok(Object::Integer(left + right));
                } else if op == "-" {
                    return Ok(Object::Integer(left - right));
                } else if op == "*" {
                    return Ok(Object::Integer(left * right));
                } else if op == "/" {
                    return Ok(Object::Integer(left / right));
                } else if op == "<" {
                    return Ok(self.native_bool_to_object(left < right));
                } else if op == ">" {
                    return Ok(self.native_bool_to_object(left > right));
                } else if op == "!=" {
                    return Ok(self.native_bool_to_object(left != right));
                } else if op == "==" {
                    return Ok(self.native_bool_to_object(left == right));
                } else {
                    return Err(format!("Invalid operator: {}", op));
                }
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                if op == "==" {
                    return Ok(self.native_bool_to_object(left == right));
                } else if op == "!=" {
                    return Ok(self.native_bool_to_object(left != right));
                } else {
                    return Err(format!("Invalid operator: {}", op));
                }
            }
            (_, _) => Err(format!("Expected integer-integer or boolean-boolean")),
        }
    }

    pub fn native_bool_to_object(&self, val: bool) -> Object {
        if val {
            return Object::Boolean(true);
        }
        Object::Boolean(false)
    }
}
