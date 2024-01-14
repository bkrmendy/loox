use std::fmt::Display;

use crate::scan::{Token, TokenLiteral, TokenType};

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Identifier(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(number) => write!(f, "{number}"),
            Literal::String(string) => write!(f, "{string}"),
            Literal::Identifier(name) => write!(f, "{name}"),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct FunctionDeclarationSyntax {
    // fun
    pub name: String,
    // `(` + identifier+ + `)`
    pub params: Vec<String>,
    // `{` + expression* + `}`
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionLiteralSyntax {
    // `(` + identifier+ + `)`
    pub params: Vec<String>,
    // `{` + expression* + `}`
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    // `var` name `=` expression `;`
    VariableDeclaration(String, Box<Expression>),
    // name `=` expression `;`
    VariableAssignment(String, Box<Expression>),
    // see `FunctionSyntax`
    FunctionDeclaration(FunctionDeclarationSyntax),
    // here so that expressions can be typed in the repl
    FreeStandingExpression(Box<Expression>),
    // represent errors
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    // `(` + expression + `)`
    Grouping(Box<Expression>),
    // unary_op + expression
    Unary(UnaryOp, Box<Expression>),
    // literal
    Literal(Literal),
    // expression + binary op + expression
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    // name + `(` + expression* + `)`
    FunctionCall(String, Vec<Expression>),
    // "lambda" function
    FunctionLiteral(FunctionLiteralSyntax),
    // represent errors
    Error(String),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({e})"),
            Expression::Unary(op, e) => write!(f, "{op}{e}"),
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::Binary(op, left, right) => write!(f, "({op} {left} {right})"),
            Expression::Error(e) => write!(f, "[{e}]"),
            Expression::FunctionCall(name, args) => {
                let param_list: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                let param_list = param_list.join(", ");
                write!(f, "{name}({param_list})")
            }
            Expression::FunctionLiteral(_) => write!(f, "#anonymous function"),
        }
    }
}

fn expect_maybe_token(tokens: &[Token], token_type: TokenType) -> (Option<Token>, &[Token]) {
    match tokens.get(0) {
        None => (None, tokens),
        Some(token) => {
            if token.token_type == token_type {
                (Some(token.clone()), &tokens[1..])
            } else {
                (None, tokens)
            }
        }
    }
}

fn expect_token(tokens: &[Token], token_type: TokenType) -> Option<(Token, &[Token])> {
    let token = tokens.get(0)?;
    if token.token_type == token_type {
        Some((token.clone(), &tokens[1..]))
    } else {
        None
    }
}

fn parse_literal(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    let next_token = tokens.get(0)?;
    match next_token {
        Token {
            token_type: TokenType::Number,
            literal: TokenLiteral::NumberLiteral(number),
            ..
        } => Some((Expression::Literal(Literal::Number(*number)), &tokens[1..])),
        Token {
            token_type: TokenType::String,
            lexeme,
            ..
        } => Some((
            Expression::Literal(Literal::String(lexeme.clone())),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::True,
            ..
        } => Some((Expression::Literal(Literal::True), &tokens[1..])),
        Token {
            token_type: TokenType::False,
            ..
        } => Some((Expression::Literal(Literal::False), &tokens[1..])),
        Token {
            token_type: TokenType::Nil,
            ..
        } => Some((Expression::Literal(Literal::Nil), &tokens[1..])),
        Token {
            token_type: TokenType::Identifier,
            lexeme,
            ..
        } => Some((
            Expression::Literal(Literal::Identifier(lexeme.clone())),
            &tokens[1..],
        )),
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
            // TODO: signal the missing error here
            let (_, rest) = expect_maybe_token(rest, TokenType::RightParen);
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
        _ => None, // TODO: add any missing operators
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

fn parse_binary_expr_operand(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    parse_literal(tokens)
        .or(parse_grouping(tokens))
        .or(parse_unary_expression(tokens))
}

fn parse_binary_with_recovery(tokens: &[Token], current_prec: u8) -> (Expression, &[Token]) {
    let next = parse_binary_expr_operand(tokens);

    if next.is_none() {
        return (Expression::Error(String::from("Expected operand")), tokens);
    }

    let (mut expr, mut rest) = next.unwrap();

    loop {
        let maybe_op = parse_binary_op(rest);
        if maybe_op.is_none() {
            return (expr, rest);
        }

        let (op, rest_after_op) = maybe_op.unwrap();
        let prec = precedence(&op);
        if prec < current_prec {
            return (expr, rest);
        }
        let maybe_next_op = parse_binary_contd(rest_after_op, prec);
        if maybe_next_op.is_none() {
            return (Expression::Error(String::from("Expected operand")), rest);
        }
        let (rhs, rest_after_rhs) = maybe_next_op.unwrap();
        expr = Expression::Binary(op, Box::new(expr), Box::new(rhs));
        rest = rest_after_rhs;
    }
}

fn parse_binary_contd(tokens: &[Token], current_prec: u8) -> Option<(Expression, &[Token])> {
    let (expr, rest) = parse_binary_expr_operand(tokens)?;
    let maybe_op = parse_binary_op(rest);
    if maybe_op.is_none() {
        return Some((expr, rest));
    }
    Some(parse_binary_with_recovery(tokens, current_prec))
}

fn parse_binary(tokens: &[Token], current_prec: u8) -> Option<(Expression, &[Token])> {
    let (_, rest) = parse_binary_expr_operand(tokens)?;
    let _ = parse_binary_op(rest)?;
    Some(parse_binary_with_recovery(tokens, current_prec))
}

fn parse_expression(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    parse_binary(tokens, 0)
        .or(parse_function_call(tokens))
        .or(parse_unary_expression(tokens))
        .or(parse_grouping(tokens))
        .or(parse_literal(tokens))
}

fn parse_var_declaration(tokens: &[Token]) -> Option<(Statement, &[Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::Var)?;
    let (identifier, tokens) = expect_maybe_token(tokens, TokenType::Identifier);
    // TODO: recovery: pop tokens until a sensibe point
    if identifier.is_none() {
        return Some((
            Statement::Error(String::from("expected identifier")),
            tokens,
        ));
    }
    let identifier = identifier.unwrap();
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Equal);
    let expr = parse_expression(tokens);
    if expr.is_none() {
        // TODO: signal error, but continue
        return Some((
            Statement::Error(String::from("expected expression")),
            tokens,
        ));
    }
    let (expr, tokens) = expr.unwrap();
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    Some((
        Statement::VariableDeclaration(identifier.lexeme, Box::new(expr)),
        tokens,
    ))
}

fn parse_variable_assignment(tokens: &[Token]) -> Option<(Statement, &[Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Equal)?;
    let (expr, tokens) = parse_expression(tokens)?; // TODO; recovery
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    Some((
        Statement::VariableAssignment(identifier.lexeme.clone(), Box::new(expr)),
        tokens,
    ))
}

fn parse_function_call(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::LeftParen)?;
    let mut args: Vec<Expression> = Vec::new();
    let mut tokens = tokens;
    while let Some((expr, tokens_after_expression)) = parse_expression(tokens) {
        args.push(expr);
        let (_, tokens_after_comma) = expect_maybe_token(tokens_after_expression, TokenType::Comma);
        tokens = tokens_after_comma
    }
    let (_, tokens) = expect_maybe_token(tokens, TokenType::RightParen);
    // if left_paren.is_none() {
    //     // TODO: signal error, but continue
    //     return Some((
    //         Expression::Error(String::from("expected expression")),
    //         tokens,
    //     ));
    // }
    Some((
        Expression::FunctionCall(identifier.lexeme.clone(), args),
        tokens,
    ))
}
fn parse_function_declaration(tokens: &[Token]) -> Option<(Statement, &[Token])> {
    // TODO: needs synchronization
    let (_, tokens) = expect_token(tokens, TokenType::Fun)?;
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_maybe_token(tokens, TokenType::LeftParen);
    let mut tokens = tokens;
    let mut arg_names: Vec<String> = Vec::new();
    while let Some((t, tokens_after_arg_name)) = expect_token(tokens, TokenType::Identifier) {
        arg_names.push(t.lexeme.clone());
        let (_, tokens_after_comma) = expect_maybe_token(tokens_after_arg_name, TokenType::Comma);
        tokens = tokens_after_comma;
    }

    let (_, tokens) = expect_maybe_token(tokens, TokenType::RightParen);
    let (_, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    let mut tokens = tokens;
    let mut statements: Vec<Statement> = Vec::new();
    while let Some((statement, rest)) = parse_statement(tokens) {
        statements.push(statement);
        tokens = rest;
    }

    let (_, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    let function_definition = FunctionDeclarationSyntax {
        name: identifier.lexeme.clone(),
        params: arg_names,
        body: statements,
    };
    Some((Statement::FunctionDeclaration(function_definition), tokens))
}

fn parse_expression_statement(tokens: &[Token]) -> Option<(Statement, &[Token])> {
    let (expr, tokens) = parse_expression(tokens)?;
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    Some((Statement::FreeStandingExpression(Box::new(expr)), tokens))
}

fn parse_statement(tokens: &[Token]) -> Option<(Statement, &[Token])> {
    parse_var_declaration(tokens)
        .or(parse_variable_assignment(tokens))
        .or(parse_expression_statement(tokens))
        .or(parse_function_declaration(tokens))
}

pub fn parse(tokens: &[Token]) -> anyhow::Result<Vec<Statement>> {
    let mut result: Vec<Statement> = vec![];
    let mut current_tokens = tokens;
    loop {
        match parse_statement(current_tokens) {
            None => break,
            Some((statement, rest)) => {
                result.push(statement);
                current_tokens = rest;
            }
        }
    }
    Ok(result)
}
