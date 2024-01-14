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

fn do_number_comparison(
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
        BinaryOp::Plus => do_number_op(left, right, |l, r| l + r),
        BinaryOp::Minus => do_number_op(left, right, |l, r| l - r),
        BinaryOp::Times => do_number_op(left, right, |l, r| l * r),
        BinaryOp::Div => do_number_op(left, right, |l, r| l / r),
        BinaryOp::Gt => do_number_comparison(left, right, |l, r| l >= r),
        BinaryOp::Gte => do_number_comparison(left, right, |l, r| l > r),
        BinaryOp::Lt => do_number_comparison(left, right, |l, r| l < r),
        BinaryOp::Lte => do_number_comparison(left, right, |l, r| l <= r),
        BinaryOp::And => do_bool_op(left, right, |l, r| l && r),
        BinaryOp::Or => do_bool_op(left, right, |l, r| l || r),
        BinaryOp::Equals => {
            do_bool_op(left, right, |l, r| l == r)
                .or(do_number_comparison(left, right, |l, r| l == r))
        }
        BinaryOp::NEquals => {
            do_bool_op(left, right, |l, r| l != r)
                .or(do_number_comparison(left, right, |l, r| l != r))
        }
    }
}

pub fn eval_expression(
    env: &Environment,
    expression: Expression,
) -> anyhow::Result<(Environment, LooxReference<Expression>)> {
    // TODO: it's (maybe) not really necessary to return an env
    match expression {
        Expression::Literal(Literal::Identifier(name)) => {
            let value = env
                .get(&name)
                .ok_or(anyhow::Error::msg(format!("Cannot find variable: {name}")))?;
            Ok((env.clone(), value.clone()))
        }
        Expression::Literal(_) => Ok((env.clone(), make_loox_ref(expression))),
        Expression::Grouping(expr) => eval_expression(env, *expr),
        Expression::Unary(op, expr) => {
            let (env, e) = eval_expression(env, *expr)?;
            let result = eval_unary_op(op, &e.borrow())?;
            Ok((env, make_loox_ref(result)))
        }
        Expression::Binary(op, left, right) => {
            let (env, left_evaled) = eval_expression(env, *left)?;
            let (env, right_evaled) = eval_expression(&env, *right)?;
            let result = eval_binary_op(op, &left_evaled.borrow(), &right_evaled.borrow())?;
            Ok((env, make_loox_ref(result)))
        }
        Expression::FunctionLiteral(_) => todo!(),
        Expression::FunctionCall(name, args) => {
            let fn_def = env
                .get(&name)
                .ok_or(anyhow::Error::msg(format!("Cannot find function: {name}")))?;
            let fn_def = match fn_def.borrow().clone() {
                // TODO :(
                Expression::FunctionLiteral(literal) => anyhow::Ok(literal),
                _ => bail!(format!("{name} is not a function")),
            }?;
            let mut env_for_function = env.clone();
            for (arg_name, arg_value) in zip(fn_def.params.iter(), args) {
                let (_, value) = eval_expression(env, arg_value)?;
                env_for_function = env_for_function.insert(arg_name.clone(), value);
            }
            let (next_env, result) = eval(env_for_function, fn_def.body.clone())?;
            let result = result.ok_or(anyhow::Error::msg("missing return value"))?;
            Ok((next_env, result))
        }
        Expression::Error(err) => bail!(err),
    }
}

fn eval_statement(
    env: &Environment,
    statement: Statement,
) -> anyhow::Result<(Environment, LooxReference<Expression>)> {
    // TODO: returing the expression (maybe) is not necessary
    match statement {
        Statement::VariableDeclaration(name, expr) => {
            let (env, val) = eval_expression(env, *expr)?;
            let next_env = env.insert(name, val.clone());
            Ok((next_env, val))
        }
        Statement::VariableAssignment(name, expr) => {
            let (env, evaled) = eval_expression(env, *expr)?;
            let value = env
                .get(&name)
                .ok_or(anyhow::Error::msg(format!("Cannot find variable: {name}")))?;
            *value.borrow_mut() = evaled.borrow().clone();
            Ok((env.clone(), value.clone()))
        }
        Statement::FreeStandingExpression(expr) => eval_expression(env, *expr),
        Statement::FunctionDeclaration(FunctionDeclarationSyntax { name, params, body }) => {
            let function_expr = make_loox_ref(Expression::FunctionLiteral(FunctionLiteralSyntax {
                params,
                body,
            }));
            let next_env = env.insert(name, function_expr.clone());
            Ok((next_env, function_expr))
        }
        Statement::Error(err) => bail!(err),
    }
}

pub fn eval(
    env: Environment,
    statements: Vec<Statement>,
) -> anyhow::Result<(Environment, Option<LooxReference<Expression>>)> {
    let mut current_env: Environment = env;
    let mut last_result: Option<LooxReference<Expression>> = None;

    for statement in statements {
        let (next_env, result) = eval_statement(&current_env, statement)?;

        current_env = next_env;
        last_result = Some(result);
    }

    Ok((current_env, last_result))
}
