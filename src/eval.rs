use std::{cell::RefCell, iter::zip, rc::Rc};

use anyhow::{bail, Ok};
use rpds::HashTrieMap;

use crate::parse::{
    BinaryOp, Expression, FunctionDeclarationSyntax, FunctionLiteralSyntax, Literal, Statement,
    UnaryOp,
};

pub type LooxReference<T> = Rc<RefCell<T>>;
fn make_loox_ref<T>(t: T) -> LooxReference<T> {
    Rc::new(RefCell::new(t))
}

pub type Environment = HashTrieMap<String, LooxReference<Expression>>;
pub type EnvPtr = Rc<RefCell<Environment>>;

fn eval_unary_op(op: UnaryOp, expression: &Expression) -> anyhow::Result<Expression> {
    match (op, expression) {
        (UnaryOp::Bang, Expression::Literal(Literal::True)) => {
            Ok(Expression::Literal(Literal::False))
        }
        (UnaryOp::Bang, Expression::Literal(Literal::False)) => {
            Ok(Expression::Literal(Literal::True))
        }
        (UnaryOp::Minus, Expression::Literal(Literal::Number(number))) => {
            Ok(Expression::Literal(Literal::Number(-number)))
        }
        (o, e) => bail!(format!("Invalid expression: {o}{e}")),
    }
}

fn do_number_op(
    left: &Expression,
    right: &Expression,
    f: fn(&f64, &f64) -> f64,
) -> anyhow::Result<Expression> {
    match (left, right) {
        (Expression::Literal(Literal::Number(a)), Expression::Literal(Literal::Number(b))) => {
            Ok(Expression::Literal(Literal::Number(f(a, b))))
        }
        _ => bail!(format!("not numbers")),
    }
}

fn do_string_op(
    left: &Expression,
    right: &Expression,
    f: fn(&String, &String) -> String,
) -> anyhow::Result<Expression> {
    match (left, right) {
        (Expression::Literal(Literal::String(a)), Expression::Literal(Literal::String(b))) => {
            Ok(Expression::Literal(Literal::String(f(a, b))))
        }
        _ => bail!(format!("not numbers")),
    }
}

fn do_number_bool_op(
    left: &Expression,
    right: &Expression,
    f: fn(&f64, &f64) -> bool,
) -> anyhow::Result<Expression> {
    match (left, right) {
        (Expression::Literal(Literal::Number(a)), Expression::Literal(Literal::Number(b))) => {
            let result = if f(a, b) {
                Literal::True
            } else {
                Literal::False
            };
            Ok(Expression::Literal(result))
        }
        _ => bail!(format!("not numbers")),
    }
}

fn do_string_bool_op(
    left: &Expression,
    right: &Expression,
    f: fn(&String, &String) -> bool,
) -> anyhow::Result<Expression> {
    match (left, right) {
        (Expression::Literal(Literal::String(a)), Expression::Literal(Literal::String(b))) => {
            let result = if f(a, b) {
                Literal::True
            } else {
                Literal::False
            };
            Ok(Expression::Literal(result))
        }
        _ => bail!(format!("not numbers")),
    }
}

fn do_bool_op(
    left: &Expression,
    right: &Expression,
    f: fn(bool, bool) -> bool,
) -> anyhow::Result<Expression> {
    let left_bool = match left {
        Expression::Literal(Literal::False) => Ok(false),
        Expression::Literal(Literal::True) => Ok(true),
        _ => bail!("Not a boolean"),
    }?;
    let right_bool = match right {
        Expression::Literal(Literal::False) => Ok(false),
        Expression::Literal(Literal::True) => Ok(true),
        _ => bail!("Not a boolean"),
    }?;
    if f(left_bool, right_bool) {
        Ok(Expression::Literal(Literal::True))
    } else {
        Ok(Expression::Literal(Literal::False))
    }
}

fn eval_binary_op(
    op: BinaryOp,
    left: &Expression,
    right: &Expression,
) -> anyhow::Result<Expression> {
    match op {
        BinaryOp::Plus => {
            do_number_op(left, right, |l, r| l + r).or(do_string_op(left, right, |a, b| {
                let mut base = a.clone();
                base.push_str(b);
                base
            }))
        }
        BinaryOp::Minus => do_number_op(left, right, |l, r| l - r),
        BinaryOp::Times => do_number_op(left, right, |l, r| l * r),
        BinaryOp::Div => do_number_op(left, right, |l, r| l / r),
        BinaryOp::Gt => do_number_bool_op(left, right, |l, r| l >= r),
        BinaryOp::Gte => do_number_bool_op(left, right, |l, r| l > r),
        BinaryOp::Lt => do_number_bool_op(left, right, |l, r| l < r),
        BinaryOp::Lte => do_number_bool_op(left, right, |l, r| l <= r),
        BinaryOp::And => do_bool_op(left, right, |l, r| l && r),
        BinaryOp::Or => do_bool_op(left, right, |l, r| l || r),
        BinaryOp::Equals => do_bool_op(left, right, |l, r| l == r)
            .or(do_number_bool_op(left, right, |l, r| l == r))
            .or(do_string_bool_op(left, right, |a, b| a == b)),
        BinaryOp::NEquals => do_bool_op(left, right, |l, r| l != r)
            .or(do_number_bool_op(left, right, |l, r| l != r))
            .or(do_string_bool_op(left, right, |a, b| a != b)),
    }
}

pub fn eval_expression(
    env: EnvPtr,
    expression: Expression,
) -> anyhow::Result<LooxReference<Expression>> {
    // TODO: it's (maybe) not really necessary to return an env
    match expression {
        Expression::Literal(Literal::Identifier(name)) => {
            let env_for_lookup = env.borrow();
            let value = env_for_lookup
                .get(&name)
                .ok_or(anyhow::Error::msg(format!("Cannot find variable: {name}")))?;
            Ok(value.clone())
        }
        Expression::Literal(_) => Ok(make_loox_ref(expression)),
        Expression::Grouping(expr) => eval_expression(env, *expr),
        Expression::Unary(op, expr) => {
            let e = eval_expression(env, *expr)?;
            let result = eval_unary_op(op, &e.borrow())?;
            Ok(make_loox_ref(result))
        }
        Expression::Binary(op, left, right) => {
            let left_evaled = eval_expression(env.clone(), *left)?;
            let right_evaled = eval_expression(env.clone(), *right)?;
            let result = eval_binary_op(op, &left_evaled.borrow(), &right_evaled.borrow())?;
            Ok(make_loox_ref(result))
        }
        Expression::FunctionLiteral(_) => todo!(),
        Expression::FunctionCall(name, args) => {
            let fn_def = {
                let env_for_lookup = env.borrow();
                env_for_lookup
                    .get(&name)
                    .ok_or(anyhow::Error::msg(format!("Cannot find variable: {name}")))?
                    .clone()
            };
            let fn_def = match fn_def.borrow().clone() {
                // TODO .clone() :(
                Expression::FunctionLiteral(literal) => anyhow::Ok(literal),
                _ => bail!(format!("{name} is not a function")),
            }?;
            let env_for_function = Rc::new(RefCell::new(fn_def.enclosing_env.borrow().clone()));
            for (arg_name, arg_value) in zip(fn_def.params.iter(), args) {
                let value = eval_expression(env.clone(), arg_value)?;
                let next_env = env_for_function.borrow().insert(arg_name.clone(), value);
                *env_for_function.borrow_mut() = next_env;
            }
            let result = eval(env_for_function, fn_def.body.clone())?;
            let result = result.ok_or(anyhow::Error::msg("missing return value"))?;
            Ok(result)
        }
        Expression::Error(err) => bail!(err),
    }
}

fn eval_statement(env: EnvPtr, statement: Statement) -> anyhow::Result<LooxReference<Expression>> {
    // TODO: returing the expression (maybe) is not necessary
    match statement {
        Statement::VariableDeclaration(name, expr) => {
            let val = eval_expression(env.clone(), *expr)?;
            let next_env = { env.borrow().insert(name, val) };
            *env.borrow_mut() = next_env;
            Ok(make_loox_ref(Expression::Literal(Literal::Unit)))
        }
        Statement::VariableAssignment(name, expr) => {
            let evaled = eval_expression(env.clone(), *expr)?;
            let value = {
                let env_for_lookup = env.borrow();
                env_for_lookup
                    .get(&name)
                    .ok_or(anyhow::Error::msg(format!("Cannot find variable: {name}")))?
                    .clone()
            };
            *value.borrow_mut() = evaled.borrow().clone();
            Ok(make_loox_ref(Expression::Literal(Literal::Unit)))
        }
        Statement::FreeStandingExpression(expr) => eval_expression(env, *expr),
        Statement::FunctionDeclaration(FunctionDeclarationSyntax { name, params, body }) => {
            let function_expr = make_loox_ref(Expression::FunctionLiteral(FunctionLiteralSyntax {
                params,
                body,
                enclosing_env: env.clone(),
            }));
            let next_env = { env.borrow().insert(name, function_expr) };
            *env.borrow_mut() = next_env;
            Ok(make_loox_ref(Expression::Literal(Literal::Unit)))
        }
        Statement::If(test, then_part, else_part) => {
            let test_result = eval_expression(env.clone(), *test)?;
            let check_test_result = match test_result.borrow().clone() {
                Expression::Literal(Literal::True) => Ok(true),
                Expression::Literal(Literal::False) => Ok(false),
                _ => bail!("if test result is not a boolean"),
            }?;

            if check_test_result {
                let env_for_scope = Rc::new(RefCell::new(env.borrow().clone()));
                let result = eval(env_for_scope, then_part)?;
                let result = result.ok_or(anyhow::Error::msg("If has empty body"))?;
                return Ok(result);
            }

            if else_part.is_some() && !check_test_result {
                let env_for_scope = Rc::new(RefCell::new(env.borrow().clone()));
                let else_part = else_part.unwrap();
                let result = eval(env_for_scope, else_part)?;
                let result = result.ok_or(anyhow::Error::msg("else has empty body"))?;
                return Ok(result);
            }

            Ok(make_loox_ref(Expression::Literal(Literal::Unit)))
        }
        Statement::While(test, body) => loop {
            let test_result = eval_expression(env.clone(), *test.clone())?;
            let check_test_result = match test_result.borrow().clone() {
                Expression::Literal(Literal::True) => Ok(true),
                Expression::Literal(Literal::False) => Ok(false),
                _ => bail!("if test result is not a boolean"),
            }?;
            if !check_test_result {
                return Ok(make_loox_ref(Expression::Literal(Literal::Unit)));
            }
            let env_for_scope = Rc::new(RefCell::new(env.borrow().clone()));
            let _ = eval(env_for_scope, body.clone())?;
        },
        Statement::Error => bail!("Cannot evaluate malformed expression"),
    }
}

pub fn eval(
    env: EnvPtr,
    statements: Vec<Statement>,
) -> anyhow::Result<Option<LooxReference<Expression>>> {
    let mut last_result: Option<LooxReference<Expression>> = None;

    for statement in statements {
        let result = eval_statement(env.clone(), statement)?;
        last_result = Some(result);
    }

    Ok(last_result)
}
