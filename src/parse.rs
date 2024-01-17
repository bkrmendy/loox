use std::{collections::BTreeMap, fmt::Display};

use crate::{
    eval::EnvPtr,
    scan::{Token, TokenLiteral, TokenType},
};

#[derive(Debug, Clone)]
pub enum Literal {
    Object(BTreeMap<String, Box<Expression>>),
    Number(f64),
    String(String),
    Identifier(String),
    True,
    False,
    Nil,
    Unit,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Object(object) => {
                let innards: String = object
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{{{innards}}}")
            }
            Literal::Number(number) => write!(f, "{number}"),
            Literal::String(string) => write!(f, "{string}"),
            Literal::Identifier(name) => write!(f, "{name}"),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
            Literal::Unit => write!(f, "()"),
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
    pub enclosing_env: EnvPtr,
}

#[derive(Debug, Clone)]
pub enum Statement {
    // `var` name `=` expression `;`
    VariableDeclaration(String, Box<Expression>),
    // name `=` expression `;`
    VariableAssignment(String, Box<Expression>),
    PropertyAssignment(String, String, Box<Expression>),
    // see `FunctionSyntax`
    FunctionDeclaration(FunctionDeclarationSyntax),
    // here so that expressions can be typed in the repl
    FreeStandingExpression(Box<Expression>),
    // represent errors
    If(Box<Expression>, Vec<Statement>, Option<Vec<Statement>>),
    While(Box<Expression>, Vec<Statement>),
    Error,
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
    // object.prop
    PropertyAccess(String, String),
    // represent errors
    Error,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({e})"),
            Expression::Unary(op, e) => write!(f, "{op}{e}"),
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::Binary(op, left, right) => write!(f, "({op} {left} {right})"),
            Expression::Error => write!(f, "[error]"),
            Expression::FunctionCall(name, args) => {
                let param_list: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                let param_list = param_list.join(", ");
                write!(f, "{name}({param_list})")
            }
            Expression::FunctionLiteral(_) => write!(f, "#anonymous function"),
            Expression::PropertyAccess(name, prop) => write!(f, "{name}.{prop}"),
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

fn synchronize(tokens: &[Token]) -> &[Token] {
    let mut tokens = tokens;
    loop {
        match tokens.get(0) {
            None => return tokens,
            Some(Token {
                token_type: TokenType::Eof,
                ..
            }) => return &tokens[1..],
            Some(Token {
                token_type: TokenType::Fun,
                ..
            }) => return &tokens[1..],
            Some(Token {
                token_type: TokenType::Var,
                ..
            }) => return &tokens[1..],
            Some(Token {
                token_type: TokenType::IF,
                ..
            }) => return &tokens[1..],
            Some(Token {
                token_type: TokenType::While,
                ..
            }) => return &tokens[1..],
            _ => (),
        }
        tokens = &tokens[1..];
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

fn parse_unit(tokens: &[Token]) -> Option<(Expression, &[Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::LeftParen)?;
    let (_, tokens) = expect_token(tokens, TokenType::RightParen)?;
    Some((Expression::Literal(Literal::Unit), tokens))
}

fn parse_object<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::LeftBrace)?;
    let mut tokens = tokens;
    let mut k_v_pairs: Vec<(String, Box<Expression>)> = Vec::new();
    while let Some((
        Token {
            token_type: TokenType::Identifier,
            lexeme,
            ..
        },
        rest,
    )) = expect_token(tokens, TokenType::Identifier)
    {
        let prop_name = lexeme.clone();
        let (maybe_colon, rest) = expect_maybe_token(rest, TokenType::Colon);
        if maybe_colon.is_none() {
            errors.push(ParseError {
                message: String::from("Expected colon"),
            })
        }
        let maybe_expr = parse_expression(errors, rest);
        if maybe_expr.is_none() {
            errors.push(ParseError {
                message: String::from("Expected expression"),
            });
            let rest: &[Token] = synchronize(rest);
            return Some((Expression::Error, rest));
        }
        let (expr, rest) = maybe_expr.unwrap();
        k_v_pairs.push((prop_name, Box::new(expr)));
        let (_, rest) = expect_maybe_token(rest, TokenType::Comma);
        tokens = rest
    }

    let (_, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);

    let object: BTreeMap<String, Box<Expression>> = BTreeMap::from_iter(k_v_pairs);
    Some((Expression::Literal(Literal::Object(object)), tokens))
}

fn parse_property_access<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    let (object_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Dot)?;
    let maybe_prop = expect_token(tokens, TokenType::Identifier);
    if maybe_prop.is_none() {
        errors.push(ParseError {
            message: String::from("Expected identifier"),
        });
        let tokens: &[Token] = synchronize(tokens);
        return Some((Expression::Error, tokens));
    }
    let (prop, tokens) = maybe_prop.unwrap();

    Some((
        Expression::PropertyAccess(object_name.lexeme.clone(), prop.lexeme.clone()),
        tokens,
    ))
}

fn parse_grouping<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::LeftParen,
            ..
        } => {
            let (expr, rest) = parse_expression(errors, &tokens[1..])?;
            // TODO: signal the missing error here
            let (_, rest) = expect_maybe_token(rest, TokenType::RightParen);
            Some((Expression::Grouping(Box::new(expr)), rest))
        }
        _ => None,
    }
}

fn parse_unary_expression<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::Minus,
            ..
        } => {
            let (expr, rest) = parse_expression(errors, &tokens[1..])?;
            Some((Expression::Unary(UnaryOp::Minus, Box::new(expr)), rest))
        }
        Token {
            token_type: TokenType::Bang,
            ..
        } => {
            let (expr, rest) = parse_expression(errors, &tokens[1..])?;
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

fn parse_binary_expr_operand<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    parse_function_call(errors, tokens)
        .or(parse_grouping(errors, tokens))
        .or(parse_unary_expression(errors, tokens))
        .or(parse_property_access(errors, tokens))
        .or(parse_literal(tokens))
}

fn parse_binary_with_recovery<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
    current_prec: u8,
) -> (Expression, &'b [Token]) {
    let next = parse_binary_expr_operand(errors, tokens);

    if next.is_none() {
        errors.push(ParseError {
            message: String::from("Expected operand"),
        });
        return (Expression::Error, tokens);
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
        let maybe_next_op = parse_binary_contd(errors, rest_after_op, prec);
        if maybe_next_op.is_none() {
            errors.push(ParseError {
                message: String::from("Expected operand"),
            });
            let rest = synchronize(rest);
            return (Expression::Error, rest);
        }
        let (rhs, rest_after_rhs) = maybe_next_op.unwrap();
        expr = Expression::Binary(op, Box::new(expr), Box::new(rhs));
        rest = rest_after_rhs;
    }
}

fn parse_binary_contd<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
    current_prec: u8,
) -> Option<(Expression, &'b [Token])> {
    let (expr, rest) = parse_binary_expr_operand(errors, tokens)?;
    let maybe_op = parse_binary_op(rest);
    if maybe_op.is_none() {
        return Some((expr, rest));
    }
    Some(parse_binary_with_recovery(errors, tokens, current_prec))
}

fn parse_binary<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
    current_prec: u8,
) -> Option<(Expression, &'b [Token])> {
    let (_, rest) = parse_binary_expr_operand(errors, tokens)?;
    let _ = parse_binary_op(rest)?;
    Some(parse_binary_with_recovery(errors, tokens, current_prec))
}

fn parse_expression<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    parse_binary(errors, tokens, 0)
        .or(parse_property_access(errors, tokens))
        .or(parse_function_call(errors, tokens))
        .or(parse_unary_expression(errors, tokens))
        .or(parse_grouping(errors, tokens))
        .or(parse_literal(tokens))
        .or(parse_object(errors, tokens))
        .or(parse_unit(tokens))
}

fn parse_var_declaration<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::Var)?;
    let (identifier, tokens) = expect_maybe_token(tokens, TokenType::Identifier);
    if identifier.is_none() {
        errors.push(ParseError {
            message: String::from("expected identifier"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let identifier = identifier.unwrap();
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Equal);
    let expr = parse_expression(errors, tokens);
    if expr.is_none() {
        errors.push(ParseError {
            message: String::from("expected identifier"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (expr, tokens) = expr.unwrap();
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    Some((
        Statement::VariableDeclaration(identifier.lexeme, Box::new(expr)),
        tokens,
    ))
}

fn parse_variable_assignment<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Equal)?;
    let maybe_expr = parse_expression(errors, tokens);
    if maybe_expr.is_none() {
        errors.push(ParseError {
            message: String::from("expected expression"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (expr, tokens) = maybe_expr.unwrap();
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    if maybe_semicolon.is_none() {
        errors.push(ParseError {
            message: String::from("expected semicolon"),
        });
    }
    Some((
        Statement::VariableAssignment(identifier.lexeme.clone(), Box::new(expr)),
        tokens,
    ))
}

fn parse_property_assignment<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (object_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Dot)?;
    let (prop_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Equal)?;
    let maybe_expr = parse_expression(errors, tokens);
    if maybe_expr.is_none() {
        errors.push(ParseError {
            message: String::from("expected expression"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (expr, tokens) = maybe_expr.unwrap();
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    if maybe_semicolon.is_none() {
        errors.push(ParseError {
            message: String::from("expected semicolon"),
        });
    }
    Some((
        Statement::PropertyAssignment(
            object_name.lexeme.clone(),
            prop_name.lexeme.clone(),
            Box::new(expr),
        ),
        tokens,
    ))
}

fn parse_function_call<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Expression, &'b [Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::LeftParen)?;
    let mut args: Vec<Expression> = Vec::new();
    let mut tokens = tokens;
    while let Some((expr, tokens_after_expression)) = parse_expression(errors, tokens) {
        args.push(expr);
        let (_, tokens_after_comma) = expect_maybe_token(tokens_after_expression, TokenType::Comma);
        tokens = tokens_after_comma
    }
    let (left_paren, tokens) = expect_maybe_token(tokens, TokenType::RightParen);
    if left_paren.is_none() {
        errors.push(ParseError {
            message: String::from("expected expression"),
        });
    }
    Some((
        Expression::FunctionCall(identifier.lexeme.clone(), args),
        tokens,
    ))
}
fn parse_function_declaration<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::Fun)?;
    let maybe_identifier = expect_token(tokens, TokenType::Identifier);
    if maybe_identifier.is_none() {
        errors.push(ParseError {
            message: String::from("expected identifier"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (identifier, tokens) = maybe_identifier.unwrap();
    let (left_paren, tokens) = expect_maybe_token(tokens, TokenType::LeftParen);
    if left_paren.is_none() {
        errors.push(ParseError {
            message: String::from("expected left paren"),
        });
    }
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
    while let Some((statement, rest)) = parse_statement(errors, tokens) {
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

fn parse_expression_statement<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (expr, tokens) = parse_expression(errors, tokens)?;
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    Some((Statement::FreeStandingExpression(Box::new(expr)), tokens))
}

fn parse_if_statement<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::IF)?;
    let maybe_test = parse_expression(errors, tokens);
    if maybe_test.is_none() {
        errors.push(ParseError {
            message: String::from("expected identifier"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (test, tokens) = maybe_test.unwrap();
    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected opening brace"),
        });
    }
    let mut tokens = tokens;
    let mut then_statements: Vec<Statement> = Vec::new();
    while let Some((statement, rest)) = parse_statement(errors, tokens) {
        then_statements.push(statement);
        tokens = rest;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected closing brace"),
        });
    }

    let (maybe_else_token, tokens) = expect_maybe_token(tokens, TokenType::Else);

    if maybe_else_token.is_none() {
        return Some((Statement::If(Box::new(test), then_statements, None), tokens));
    }
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Else);
    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected opening brace"),
        });
    }

    let mut tokens = tokens;
    let mut else_statements: Vec<Statement> = Vec::new();
    while let Some((statement, rest)) = parse_statement(errors, tokens) {
        else_statements.push(statement);
        tokens = rest;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected closing brace"),
        });
    }

    Some((
        Statement::If(Box::new(test), then_statements, Some(else_statements)),
        tokens,
    ))
}

fn parse_while_statement<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    let (_, tokens) = expect_token(tokens, TokenType::While)?;
    let maybe_test = parse_expression(errors, tokens);
    if maybe_test.is_none() {
        errors.push(ParseError {
            message: String::from("expected identifier"),
        });
        let tokens = synchronize(tokens);
        return Some((Statement::Error, tokens));
    }
    let (test, tokens) = maybe_test.unwrap();
    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected opening brace"),
        });
    }
    let mut tokens = tokens;
    let mut then_statements: Vec<Statement> = Vec::new();
    while let Some((statement, rest)) = parse_statement(errors, tokens) {
        then_statements.push(statement);
        tokens = rest;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        errors.push(ParseError {
            message: String::from("expected closing brace"),
        });
    }

    Some((Statement::While(Box::new(test), then_statements), tokens))
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

fn parse_statement<'a: 'b, 'b>(
    errors: &mut Vec<ParseError>,
    tokens: &'b [Token],
) -> Option<(Statement, &'b [Token])> {
    parse_var_declaration(errors, tokens)
        .or(parse_if_statement(errors, tokens))
        .or(parse_while_statement(errors, tokens))
        .or(parse_property_assignment(errors, tokens))
        .or(parse_variable_assignment(errors, tokens))
        .or(parse_expression_statement(errors, tokens))
        .or(parse_function_declaration(errors, tokens))
}

pub fn parse(tokens: &[Token]) -> (Vec<Statement>, Vec<ParseError>) {
    let mut errors: Vec<ParseError> = Vec::new();
    let mut result: Vec<Statement> = vec![];
    let mut current_tokens = tokens;
    loop {
        match parse_statement(&mut errors, current_tokens) {
            None => break,
            Some((statement, rest)) => {
                result.push(statement);
                current_tokens = rest;
            }
        }
    }
    (result, errors)
}

#[cfg(test)]
mod tests {
    use crate::{parse::parse, scan::scan};

    #[test]
    fn test_parse_variable() {
        let src = "var a = 1;";
        let tokens = scan(src).expect("should be able to tokenize source");
        let (ast, _errors) = parse(&tokens);
        insta::assert_debug_snapshot!(ast, @r###"
        [
            VariableDeclaration(
                "a",
                Literal(
                    Number(
                        1.0,
                    ),
                ),
            ),
        ]
        "###);
    }

    #[test]
    fn test_parse_variable_no_ident() {
        let src = "var = 1;";
        let tokens = scan(src).expect("should be able to tokenize source");
        let (ast, errors) = parse(&tokens);
        insta::assert_debug_snapshot!(ast, @r###"
        [
            Error,
        ]
        "###);
        insta::assert_debug_snapshot!(errors, @r###"
        [
            ParseError {
                message: "expected identifier",
            },
        ]
        "###);
    }

    #[test]
    fn test_parse_variable_recovery() {
        let src = r###"
        var = 1;
        var b = 2;
        "###;
        let tokens = scan(src).expect("should be able to tokenize source");
        let (ast, errors) = parse(&tokens);
        insta::assert_debug_snapshot!(ast, @r###"
        [
            Error,
            VariableAssignment(
                "b",
                Literal(
                    Number(
                        2.0,
                    ),
                ),
            ),
        ]
        "###);
        insta::assert_debug_snapshot!(errors, @r###"
        [
            ParseError {
                message: "expected identifier",
            },
        ]
        "###);
    }
}
