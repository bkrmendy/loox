use crate::parse::{Expression, ExpressionAst, Literal, Statement, StatementAst};

use super::compiler::Compiler;

pub fn compile_program(compiler: &mut Compiler, program: &Vec<StatementAst>) {
    for statement in program {
        compile_statement(compiler, statement);
    }
}

fn compile_statement(compiler: &mut Compiler, statement: &StatementAst) {
    match &statement.statement {
        Statement::FreeStandingExpression(expr) => compile_expression(compiler, expr),
        _ => todo!(),
    }
}

fn compile_literal(compiler: &mut Compiler, literal: &Literal) {
    match &literal {
        Literal::Number(number) => compiler.push_immediate_number(*number),
        Literal::False => compiler.push_boolean(false),
        Literal::True => compiler.push_boolean(true),
        _ => todo!(),
    }
}

fn compile_expression(compiler: &mut Compiler, expression: &ExpressionAst) {
    match &expression.expression {
        Expression::Binary(operator, left, right) => {
            compile_expression(compiler, left);
            compile_expression(compiler, right);
            compiler.binary_op(operator);
        }
        Expression::Grouping(expr) => compile_expression(compiler, expr),
        Expression::Literal(literal) => compile_literal(compiler, literal),
        _ => todo!(),
    }
}
