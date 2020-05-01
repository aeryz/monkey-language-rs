use crate::ast::{Expression, Statement};
use crate::object::Object;
use std::rc::Rc;

pub struct Evaluator {
    pub rc_true: Rc<Object>,
    pub rc_false: Rc<Object>,
    pub rc_null: Rc<Object>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            rc_true: Rc::new(Object::Boolean(true)),
            rc_false: Rc::new(Object::Boolean(false)),
            rc_null: Rc::new(Object::Null),
        }
    }

    pub fn eval_statements(&self, statements: &Vec<Statement>) -> Result<Rc<Object>, String> {
        let mut res = Rc::clone(&self.rc_null);
        for s in statements {
            res = self.eval_statement(s)?;

            if let &Object::Return(..) = &*res {
                return Ok(res);
            }
        }

        Ok(res)
    }

    pub fn eval_statement(&self, stmt: &Statement) -> Result<Rc<Object>, String> {
        match stmt {
            Statement::Expression(expr) => self.eval_expression(expr),
            Statement::Block(stmts) => self.eval_statements(stmts),
            Statement::Return(expr) => Ok(Rc::new(Object::Return(self.eval_expression(expr)?))),
            _ => Err(String::from("Invalid statement")),
        }
    }

    pub fn eval_expression(&self, expr: &Expression) -> Result<Rc<Object>, String> {
        match expr {
            Expression::IntegerLiteral(val) => Ok(Rc::new(Object::Integer(*val))),
            Expression::Boolean(val) => {
                if *val {
                    Ok(Rc::clone(&self.rc_true))
                } else {
                    Ok(Rc::clone(&self.rc_false))
                }
            }
            Expression::Prefix(op, expr) => {
                let right = self.eval_expression(&**expr)?;
                return self.eval_prefix_expression(*op, &*right);
            }
            Expression::Infix(lhs, op, rhs) => {
                let lhs = self.eval_expression(&**lhs)?;
                let rhs = self.eval_expression(&**rhs)?;
                return self.eval_infix_expression(&*lhs, *op, &*rhs);
            }
            Expression::If(cond, cons, alt) => {
                let cond = self.eval_expression(&**cond)?;
                if self.is_truthy(&*cond) {
                    return self.eval_statement(cons);
                } else if let Some(alternative) = alt {
                    return self.eval_statement(alternative);
                } else {
                    return Ok(Rc::clone(&self.rc_null));
                }
            }
            _ => Err(String::from("Invalid expression")),
        }
    }

    pub fn is_truthy(&self, obj: &Object) -> bool {
        match obj {
            &Object::Boolean(val) => val,
            &Object::Null => false,
            _ => true,
        }
    }

    pub fn eval_prefix_expression(&self, op: &str, right: &Object) -> Result<Rc<Object>, String> {
        match op {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_operator_expression(right),
            _ => Err(format!("Unknown operator: {}", op)),
        }
    }

    pub fn eval_bang_operator_expression(&self, expr: &Object) -> Result<Rc<Object>, String> {
        match expr {
            Object::Boolean(true) => Ok(Rc::clone(&self.rc_false)),
            Object::Boolean(false) => Ok(Rc::clone(&self.rc_true)),
            Object::Null => Ok(Rc::clone(&self.rc_true)),
            _ => Ok(Rc::clone(&self.rc_false)),
        }
    }

    pub fn eval_minus_operator_expression(&self, expr: &Object) -> Result<Rc<Object>, String> {
        match expr {
            Object::Integer(val) => Ok(Rc::new(Object::Integer(-val))),
            _ => Err(String::from("Expected an integer")),
        }
    }

    pub fn eval_infix_expression(
        &self,
        left: &Object,
        op: &str,
        right: &Object,
    ) -> Result<Rc<Object>, String> {
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                if op == "+" {
                    return Ok(Rc::new(Object::Integer(left + right)));
                } else if op == "-" {
                    return Ok(Rc::new(Object::Integer(left - right)));
                } else if op == "*" {
                    return Ok(Rc::new(Object::Integer(left * right)));
                } else if op == "/" {
                    return Ok(Rc::new(Object::Integer(left / right)));
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

    pub fn native_bool_to_object(&self, val: bool) -> Rc<Object> {
        if val {
            return Rc::clone(&self.rc_true);
        }
        Rc::clone(&self.rc_false)
    }
}
