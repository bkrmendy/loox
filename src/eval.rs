use anyhow::{bail, Ok};

use crate::parse::{BinaryOp, Expression, Literal, UnaryOp};

fn eval_unary_op(op: UnaryOp, expression: Expression) -> anyhow::Result<Expression> {
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

fn eval_binary_op(op: BinaryOp, left: Expression, right: Expression) -> anyhow::Result<Expression> {
    match op {
        BinaryOp::Plus => do_number_op(&left, &right, |l, r| l + r),
        BinaryOp::Minus => do_number_op(&left, &right, |l, r| l - r),
        BinaryOp::Times => do_number_op(&left, &right, |l, r| l * r),
        BinaryOp::Div => do_number_op(&left, &right, |l, r| l / r),
        BinaryOp::Gt => do_number_comparison(&left, &right, |l, r| l >= r),
        BinaryOp::Gte => do_number_comparison(&left, &right, |l, r| l > r),
        BinaryOp::Lt => do_number_comparison(&left, &right, |l, r| l < r),
        BinaryOp::Lte => do_number_comparison(&left, &right, |l, r| l <= r),
        BinaryOp::And => do_bool_op(&left, &right, |l, r| l && r),
        BinaryOp::Or => do_bool_op(&left, &right, |l, r| l || r),
        BinaryOp::Equals => do_bool_op(&left, &right, |l, r| l == r).or(do_number_comparison(
            &left,
            &right,
            |l, r| l == r,
        )),
        BinaryOp::NEquals => do_bool_op(&left, &right, |l, r| l != r).or(do_number_comparison(
            &left,
            &right,
            |l, r| l != r,
        )),
    }
}

pub fn eval(expression: Expression) -> anyhow::Result<Expression> {
    match expression {
        Expression::Literal(_) => Ok(expression),
        Expression::Grouping(expr) => eval(*expr),
        Expression::Unary(op, expr) => eval(*expr).and_then(|e| eval_unary_op(op, e)),
        Expression::Binary(op, left, right) => {
            let left_evaled = eval(*left)?;
            let right_evaled = eval(*right)?;
            eval_binary_op(op, left_evaled, right_evaled)
        }
        Expression::Error(err) => bail!(err),
    }
}
