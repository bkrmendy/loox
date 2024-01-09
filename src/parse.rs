use std::fmt::Display;

use anyhow::bail;

use crate::scan::{Token, TokenLiteral, TokenType};

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(number) => write!(f, "{number}"),
            Literal::String(string) => write!(f, "{string}"),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Bang => write!(f, "!"),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Equals,
    NEquals,
    Lt,
    Lte,
    Gt,
    Gte,
    Or,
    And,
    Plus,
    Minus,
    Times,
    Div,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Equals => write!(f, "=="),
            BinaryOp::NEquals => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Lte => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Gte => write!(f, ">="),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Times => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::And => write!(f, "and"),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    // `(` + expression + `)`
    Grouping(Box<Expression>),
    // unary_op + expression
    Unary(UnaryOp, Box<Expression>),
    // literal
    Literal(Literal),
    // expression + binary op + expression
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({e})"),
            Expression::Unary(op, e) => write!(f, "{op}{e}"),
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::Binary(op, left, right) => write!(f, "({op} {left} {right})"),
        }
    }
}

fn expect_token(tokens: &[Token], token_type: TokenType) -> (Option<TokenType>, &[Token]) {
    match tokens.get(0) {
        None => (None, tokens),
        Some(token) => {
            if token.token_type == token_type {
                (Some(token_type), &tokens[1..])
            } else {
                (None, tokens)
            }
        }
    }
}

fn parse_literal(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    match tokens.get(0) {
        Some(Token {
            token_type: TokenType::Number,
            literal: TokenLiteral::NumberLiteral(number),
            ..
        }) => Some((Expression::Literal(Literal::Number(*number)), &tokens[1..])),
        Some(Token {
            token_type: TokenType::String,
            lexeme,
            ..
        }) => Some((
            Expression::Literal(Literal::String(lexeme.clone())),
            &tokens[1..],
        )),
        Some(Token {
            token_type: TokenType::True,
            ..
        }) => Some((Expression::Literal(Literal::True), &tokens[1..])),
        Some(Token {
            token_type: TokenType::False,
            ..
        }) => Some((Expression::Literal(Literal::False), &tokens[1..])),
        Some(Token {
            token_type: TokenType::Nil,
            ..
        }) => Some((Expression::Literal(Literal::Nil), &tokens[1..])),
        _ => None,
    }
}

fn parse_grouping(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::LeftParen,
            ..
        } => {
            let (expr, rest) = parse_expression(&tokens[1..])?;
            let (_, rest) = expect_token(rest, TokenType::RightParen);
            Some((Expression::Grouping(Box::new(expr)), rest))
        }
        _ => None,
    }
}

fn parse_unary_expression(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::Minus,
            ..
        } => {
            let (expr, rest) = parse_expression(&tokens[1..])?;
            Some((Expression::Unary(UnaryOp::Minus, Box::new(expr)), rest))
        }
        Token {
            token_type: TokenType::Bang,
            ..
        } => {
            let (expr, rest) = parse_expression(&tokens[1..])?;
            Some((Expression::Unary(UnaryOp::Bang, Box::new(expr)), rest))
        }
        _ => None,
    }
}

fn parse_binary_op(tokens: &[Token]) -> Option<(BinaryOp, &[Token])> {
    if tokens.is_empty() {
        return None;
    }

    let next_token = tokens.get(0).unwrap();
    let rest = &tokens[1..];
    match next_token.token_type {
        TokenType::Greater => Some((BinaryOp::Gt, rest)),
        TokenType::GreaterEqual => Some((BinaryOp::Gte, rest)),
        TokenType::Less => Some((BinaryOp::Lt, rest)),
        TokenType::LessEqual => Some((BinaryOp::Lte, rest)),
        TokenType::Plus => Some((BinaryOp::Plus, rest)),
        TokenType::Minus => Some((BinaryOp::Minus, rest)),
        TokenType::Star => Some((BinaryOp::Times, rest)),
        TokenType::Slash => Some((BinaryOp::Div, rest)),
        TokenType::EqualEqual => Some((BinaryOp::Equals, rest)),
        TokenType::BangEqual => Some((BinaryOp::NEquals, rest)),
        TokenType::And => Some((BinaryOp::And, rest)),
        TokenType::Or => Some((BinaryOp::Or, rest)),
        _ => None, // TODO
    }
}

fn precedence(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::Plus | BinaryOp::Minus => 1,
        BinaryOp::Div | BinaryOp::Times => 2,
        BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte => 3,
        BinaryOp::And | BinaryOp::Or => 4,
        BinaryOp::Equals | BinaryOp::NEquals => 5,
    }
}

fn parse_binary(tokens: &[Token], current_prec: u8) -> Option<(Expression, &[Token])> {
    let (mut expr, mut rest) = parse_literal(tokens)
        .or(parse_grouping(tokens))
        .or(parse_unary_expression(tokens))?;

    loop {
        if let Some((op, rest_after_op)) = parse_binary_op(rest) {
            let prec = precedence(&op);
            if prec < current_prec {
                return Some((expr, rest));
            }
            let (rhs, rest_after_rhs) = parse_binary(rest_after_op, prec)?;
            expr = Expression::Binary(op, Box::new(expr), Box::new(rhs));
            rest = rest_after_rhs;
        } else {
            return Some((expr, rest));
        }
    }
}

fn parse_expression(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    parse_binary(tokens, 0)
        .or(parse_unary_expression(tokens))
        .or(parse_grouping(tokens))
        .or(parse_literal(tokens))
}

pub fn parse(tokens: &[Token]) -> anyhow::Result<Expression> {
    match parse_expression(tokens) {
        None => bail!("Cannot parse expression"),
        Some((expr, rest)) => Ok(expr),
    }
}
