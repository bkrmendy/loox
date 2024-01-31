use std::{collections::BTreeMap, fmt::Display};

use crate::{
    eval::EnvPtr,
    scan::{SourceRange, Token, TokenLiteral, TokenType},
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

// TODO: replace .into_ast(SourceRange::new(0, 0)) with actual source locations
// this shouldn't be this hard

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

#[derive(Debug, Clone, Copy)]
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
    pub body: Vec<StatementAst>,
}

#[derive(Debug, Clone)]
pub struct FunctionLiteralSyntax {
    // `(` + identifier+ + `)`
    pub params: Vec<String>,
    // `{` + expression* + `}`
    pub body: Vec<StatementAst>,
    pub enclosing_env: EnvPtr,
}

#[derive(Debug, Clone)]
pub struct StatementAst {
    pub source_location: SourceRange,
    pub statement: Statement,
}

#[derive(Debug, Clone)]
pub enum Statement {
    // `var` name `=` expression `;`
    VariableDeclaration(String, Box<ExpressionAst>),
    // name `=` expression `;`
    VariableAssignment(String, Box<ExpressionAst>),
    PropertyAssignment(String, String, Box<ExpressionAst>),
    // see `FunctionSyntax`
    FunctionDeclaration(FunctionDeclarationSyntax),
    // here so that expressions can be typed in the repl
    FreeStandingExpression(Box<ExpressionAst>),
    // represent errors
    If(
        Box<ExpressionAst>,
        Vec<StatementAst>,
        Option<Vec<StatementAst>>,
    ),
    While(Box<ExpressionAst>, Vec<StatementAst>),
    Error,
}

impl Statement {
    pub fn into_ast(self, source_location: SourceRange) -> StatementAst {
        StatementAst {
            statement: self,
            source_location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionAst {
    pub source_location: SourceRange,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    // `(` + expression + `)`
    Grouping(Box<ExpressionAst>),
    // unary_op + expression
    Unary(UnaryOp, Box<ExpressionAst>),
    // literal
    Literal(Literal),
    // expression + binary op + expression
    Binary(BinaryOp, Box<ExpressionAst>, Box<ExpressionAst>),
    // name + `(` + expression* + `)`
    FunctionCall(String, Vec<ExpressionAst>),
    // "lambda" function
    FunctionLiteral(FunctionLiteralSyntax),
    // object.prop
    PropertyAccess(String, String),
    // represent errors
    Error,
}

impl Expression {
    pub fn into_ast(self, source_location: SourceRange) -> ExpressionAst {
        ExpressionAst {
            source_location,
            expression: self,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Grouping(e) => write!(f, "({})", e.expression),
            Expression::Unary(op, e) => write!(f, "{op}{}", e.expression),
            Expression::Literal(literal) => write!(f, "{literal}"),
            Expression::Binary(op, left, right) => {
                write!(f, "({op} {} {})", left.expression, right.expression)
            }
            Expression::Error => write!(f, "[error]"),
            Expression::FunctionCall(name, args) => {
                let param_list: Vec<String> =
                    args.iter().map(|a| a.expression.to_string()).collect();
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

fn parse_literal(tokens: &[Token]) -> Option<(ExpressionAst, &[Token])> {
    let next_token = tokens.get(0)?;
    match next_token {
        Token {
            token_type: TokenType::Number,
            literal: TokenLiteral::NumberLiteral(number),
            ..
        } => Some((
            Expression::Literal(Literal::Number(*number))
                .into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::String,
            lexeme,
            ..
        } => Some((
            Expression::Literal(Literal::String(lexeme.clone()))
                .into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::True,
            ..
        } => Some((
            Expression::Literal(Literal::True).into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::False,
            ..
        } => Some((
            Expression::Literal(Literal::False).into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::Nil,
            ..
        } => Some((
            Expression::Literal(Literal::Nil).into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        Token {
            token_type: TokenType::Identifier,
            lexeme,
            ..
        } => Some((
            Expression::Literal(Literal::Identifier(lexeme.clone()))
                .into_ast(next_token.source_location.clone()),
            &tokens[1..],
        )),
        _ => None,
    }
}

fn parse_unit(tokens: &[Token]) -> Option<(ExpressionAst, &[Token])> {
    let (left_paren_token, tokens) = expect_token(tokens, TokenType::LeftParen)?;
    let (right_paren_token, tokens) = expect_token(tokens, TokenType::RightParen)?;
    Some((
        Expression::Literal(Literal::Unit).into_ast(SourceRange::new(
            left_paren_token.source_location.start,
            right_paren_token.source_location.end,
        )),
        tokens,
    ))
}

fn parse_object<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    let (left_brace_token, tokens) = expect_token(tokens, TokenType::LeftBrace)?;
    let mut tokens = tokens;
    let mut k_v_pairs: Vec<(String, Box<Expression>)> = Vec::new();
    let mut k_v_pairs_source_end: usize = left_brace_token.source_location.end;
    while let Some((
        Token {
            token_type: TokenType::Identifier,
            lexeme,
            source_location,
            ..
        },
        rest,
    )) = expect_token(tokens, TokenType::Identifier)
    {
        let prop_name = lexeme.clone();
        let (maybe_colon, rest) = expect_maybe_token(rest, TokenType::Colon);
        if maybe_colon.is_none() {
            messages.error("Expected :", &source_location);
        }
        let maybe_expr = parse_expression(messages, rest);
        if maybe_expr.is_none() {
            let location = maybe_colon
                .map(|c| c.source_location)
                .unwrap_or(source_location);
            messages.error("Expected expression", &location);
            let rest: &[Token] = synchronize(rest);
            return Some((Expression::Error.into_ast(location), rest));
        }
        let (expr, rest) = maybe_expr.unwrap();
        k_v_pairs.push((prop_name, Box::new(expr.expression)));
        let (token_comma, rest) = expect_maybe_token(rest, TokenType::Comma);
        tokens = rest;
        k_v_pairs_source_end = token_comma
            .map(|t| t.source_location.end)
            .unwrap_or(expr.source_location.end);
    }

    let (right_brace_token, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if right_brace_token.is_none() {
        messages.error(
            "Expected }",
            &SourceRange::new(k_v_pairs_source_end, k_v_pairs_source_end + 1),
        );
    }

    let object: BTreeMap<String, Box<Expression>> = BTreeMap::from_iter(k_v_pairs);
    let object_end = right_brace_token
        .map(|t| t.source_location.end)
        .unwrap_or(k_v_pairs_source_end);
    Some((
        Expression::Literal(Literal::Object(object)).into_ast(SourceRange::new(
            left_brace_token.source_location.start,
            object_end,
        )),
        tokens,
    ))
}

fn parse_property_access<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    let (object_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (dot_token, tokens) = expect_token(tokens, TokenType::Dot)?;
    let maybe_prop = expect_token(tokens, TokenType::Identifier);
    if maybe_prop.is_none() {
        messages.error("Expected identifier", &dot_token.source_location);
        let tokens: &[Token] = synchronize(tokens);
        return Some((
            Expression::Error.into_ast(dot_token.source_location),
            tokens,
        ));
    }
    let (prop, tokens) = maybe_prop.unwrap();

    Some((
        Expression::PropertyAccess(object_name.lexeme.clone(), prop.lexeme.clone()).into_ast(
            SourceRange::new(object_name.source_location.start, prop.source_location.end),
        ),
        tokens,
    ))
}

fn parse_grouping<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::LeftParen,
            ..
        } => {
            let (expr, rest) = parse_expression(messages, &tokens[1..])?;
            // TODO: signal the missing error here
            let (right_paren, rest) = expect_maybe_token(rest, TokenType::RightParen);
            let source_start = expr.source_location.start;
            let source_end = right_paren
                .map(|r| r.source_location.end)
                .unwrap_or(expr.source_location.end);
            Some((
                Expression::Grouping(Box::new(expr))
                    .into_ast(SourceRange::new(source_start, source_end)),
                rest,
            ))
        }
        _ => None,
    }
}

fn parse_unary_expression<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    if tokens.is_empty() {
        return None;
    }

    match tokens.get(0).unwrap() {
        Token {
            token_type: TokenType::Minus,
            source_location: SourceRange { start, .. },
            ..
        } => {
            let (expr, rest) = parse_expression(messages, &tokens[1..])?;
            let expr_end = expr.source_location.end;
            Some((
                Expression::Unary(UnaryOp::Minus, Box::new(expr))
                    .into_ast(SourceRange::new(*start, expr_end)),
                rest,
            ))
        }
        Token {
            token_type: TokenType::Bang,
            source_location: SourceRange { start, .. },
            ..
        } => {
            let (expr, rest) = parse_expression(messages, &tokens[1..])?;
            let expr_end = expr.source_location.end;
            Some((
                Expression::Unary(UnaryOp::Bang, Box::new(expr))
                    .into_ast(SourceRange::new(*start, expr_end)),
                rest,
            ))
        }
        _ => None,
    }
}

fn parse_binary_op(tokens: &[Token]) -> Option<(BinaryOp, SourceRange, &[Token])> {
    if tokens.is_empty() {
        return None;
    }

    let next_token = tokens.get(0).unwrap();
    let rest = &tokens[1..];
    match next_token.token_type {
        TokenType::Greater => Some((BinaryOp::Gt, next_token.source_location.clone(), rest)),
        TokenType::GreaterEqual => Some((BinaryOp::Gte, next_token.source_location.clone(), rest)),
        TokenType::Less => Some((BinaryOp::Lt, next_token.source_location.clone(), rest)),
        TokenType::LessEqual => Some((BinaryOp::Lte, next_token.source_location.clone(), rest)),
        TokenType::Plus => Some((BinaryOp::Plus, next_token.source_location.clone(), rest)),
        TokenType::Minus => Some((BinaryOp::Minus, next_token.source_location.clone(), rest)),
        TokenType::Star => Some((BinaryOp::Times, next_token.source_location.clone(), rest)),
        TokenType::Slash => Some((BinaryOp::Div, next_token.source_location.clone(), rest)),
        TokenType::EqualEqual => Some((BinaryOp::Equals, next_token.source_location.clone(), rest)),
        TokenType::BangEqual => Some((BinaryOp::NEquals, next_token.source_location.clone(), rest)),
        TokenType::And => Some((BinaryOp::And, next_token.source_location.clone(), rest)),
        TokenType::Or => Some((BinaryOp::Or, next_token.source_location.clone(), rest)),
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
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    parse_function_call(messages, tokens)
        .or(parse_grouping(messages, tokens))
        .or(parse_unary_expression(messages, tokens))
        .or(parse_property_access(messages, tokens))
        .or(parse_literal(tokens))
}

fn parse_binary_with_recovery<'a: 'b, 'b>(
    messages: &mut Messages,
    start: usize,
    tokens: &'b [Token],
    current_prec: u8,
) -> (ExpressionAst, &'b [Token]) {
    let next = parse_binary_expr_operand(messages, tokens);

    if next.is_none() {
        let location = SourceRange::new(start, start + 1);
        messages.error("Expected operand", &location);
        return (Expression::Error.into_ast(location), tokens);
    }

    let (mut expr, mut rest) = next.unwrap();

    loop {
        let maybe_op = parse_binary_op(rest);
        if maybe_op.is_none() {
            return (expr, rest);
        }

        let (op, location, rest_after_op) = maybe_op.unwrap();
        let prec = precedence(&op);
        if prec < current_prec {
            return (expr, rest);
        }
        let maybe_next_op = parse_binary_contd(messages, location.end, rest_after_op, prec);
        if maybe_next_op.is_none() {
            let loc = location.after();
            messages.error("Expected operand", &loc);
            let rest = synchronize(rest);
            return (
                Expression::Error.into_ast(SourceRange::new(start, loc.start)),
                rest,
            );
        }
        let (rhs, rest_after_rhs) = maybe_next_op.unwrap();
        let location = SourceRange::new(expr.source_location.start, rhs.source_location.end);
        expr = Expression::Binary(op, Box::new(expr), Box::new(rhs)).into_ast(location);
        rest = rest_after_rhs;
    }
}

fn parse_binary_contd<'a: 'b, 'b>(
    messages: &mut Messages,
    start: usize,
    tokens: &'b [Token],
    current_prec: u8,
) -> Option<(ExpressionAst, &'b [Token])> {
    let (expr, rest) = parse_binary_expr_operand(messages, tokens)?;
    let maybe_op = parse_binary_op(rest);
    if maybe_op.is_none() {
        return Some((expr, rest));
    }
    Some(parse_binary_with_recovery(
        messages,
        start,
        tokens,
        current_prec,
    ))
}

fn parse_binary<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
    current_prec: u8,
) -> Option<(ExpressionAst, &'b [Token])> {
    let (operand, rest) = parse_binary_expr_operand(messages, tokens)?;
    let _ = parse_binary_op(rest)?;
    Some(parse_binary_with_recovery(
        messages,
        operand.source_location.start,
        tokens,
        current_prec,
    ))
}

fn parse_expression<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    parse_binary(messages, tokens, 0)
        .or(parse_property_access(messages, tokens))
        .or(parse_function_call(messages, tokens))
        .or(parse_unary_expression(messages, tokens))
        .or(parse_grouping(messages, tokens))
        .or(parse_literal(tokens))
        .or(parse_object(messages, tokens))
        .or(parse_unit(tokens))
}

fn parse_var_declaration<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (var_token, tokens) = expect_token(tokens, TokenType::Var)?;
    let (identifier, tokens) = expect_maybe_token(tokens, TokenType::Identifier);
    if identifier.is_none() {
        let location = SourceRange::new(
            var_token.source_location.end,
            var_token.source_location.end + 1,
        );
        messages.error("Expected identifier", &location);
        let tokens = synchronize(tokens);
        return Some((
            StatementAst {
                statement: Statement::Error,
                source_location: location,
            },
            tokens,
        ));
    }
    let identifier = identifier.unwrap();
    let (_, tokens) = expect_maybe_token(tokens, TokenType::Equal);
    let expr = parse_expression(messages, tokens);
    if expr.is_none() {
        let location = SourceRange::new(
            identifier.source_location.end,
            identifier.source_location.end + 1,
        );
        messages.error("Expected identifier", &location);
        let tokens = synchronize(tokens);
        return Some((
            StatementAst {
                statement: Statement::Error,
                source_location: location,
            },
            tokens,
        ));
    }
    let (expr, tokens) = expr.unwrap();
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    if maybe_semicolon.is_none() {
        messages.warning("Expected ;", &expr.source_location);
    }
    let expr_end = expr.source_location.end;
    Some((
        StatementAst {
            statement: Statement::VariableDeclaration(identifier.lexeme, Box::new(expr)),
            source_location: SourceRange::new(var_token.source_location.start, expr_end),
        },
        tokens,
    ))
}

fn parse_variable_assignment<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (equals_token, tokens) = expect_token(tokens, TokenType::Equal)?;
    let maybe_expr = parse_expression(messages, tokens);
    if maybe_expr.is_none() {
        messages.error("Expected expression", &equals_token.source_location);
        let tokens = synchronize(tokens);
        return Some((
            Statement::Error.into_ast(equals_token.source_location),
            tokens,
        ));
    }
    let (expr, tokens) = maybe_expr.unwrap();
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    let expr_end = expr.source_location.end;
    if maybe_semicolon.is_none() {
        messages.warning("Expected ;", &expr.source_location);
    }
    Some((
        Statement::VariableAssignment(identifier.lexeme.clone(), Box::new(expr))
            .into_ast(SourceRange::new(identifier.source_location.start, expr_end)),
        tokens,
    ))
}

fn parse_property_assignment<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (object_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (_, tokens) = expect_token(tokens, TokenType::Dot)?;
    let (prop_name, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (equals_token, tokens) = expect_token(tokens, TokenType::Equal)?;
    let maybe_expr = parse_expression(messages, tokens);
    if maybe_expr.is_none() {
        let location = SourceRange::new(
            object_name.source_location.start,
            equals_token.source_location.end,
        );
        messages.error("Expected expression", &location);
        let tokens = synchronize(tokens);
        return Some((Statement::Error.into_ast(location), tokens));
    }
    let (expr, tokens) = maybe_expr.unwrap();
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    let expr_end = expr.source_location.end;
    if maybe_semicolon.is_none() {
        messages.warning("Expected ;", &expr.source_location);
    }
    Some((
        Statement::PropertyAssignment(
            object_name.lexeme.clone(),
            prop_name.lexeme.clone(),
            Box::new(expr),
        )
        .into_ast(SourceRange::new(
            object_name.source_location.start,
            expr_end,
        )),
        tokens,
    ))
}

fn parse_function_call<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(ExpressionAst, &'b [Token])> {
    let (identifier, tokens) = expect_token(tokens, TokenType::Identifier)?;
    let (left_paren_token, tokens) = expect_token(tokens, TokenType::LeftParen)?;
    let mut args: Vec<ExpressionAst> = Vec::new();
    let mut tokens = tokens;
    let mut args_end = left_paren_token.source_location.end;
    while let Some((expr, tokens_after_expression)) = parse_expression(messages, tokens) {
        let end = expr.source_location.end;
        args.push(expr);
        let (maybe_comma_token, tokens_after_comma) =
            expect_maybe_token(tokens_after_expression, TokenType::Comma);
        tokens = tokens_after_comma;
        args_end = maybe_comma_token
            .map(|t| t.source_location.end)
            .unwrap_or(end);
    }
    let (right_paren, tokens) = expect_maybe_token(tokens, TokenType::RightParen);
    if right_paren.is_none() {
        messages.error("Expected )", &SourceRange::new(args_end, args_end + 1));
    }
    let function_call_end = right_paren
        .map(|t| t.source_location.end)
        .unwrap_or(args_end);
    Some((
        Expression::FunctionCall(identifier.lexeme.clone(), args).into_ast(SourceRange::new(
            identifier.source_location.start,
            function_call_end,
        )),
        tokens,
    ))
}
fn parse_function_declaration<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (fun_token, tokens) = expect_token(tokens, TokenType::Fun)?;
    let maybe_identifier = expect_token(tokens, TokenType::Identifier);
    if maybe_identifier.is_none() {
        messages.error("Expected identifier", &fun_token.source_location);
        let tokens = synchronize(tokens);
        return Some((Statement::Error.into_ast(fun_token.source_location), tokens));
    }
    let (identifier, tokens) = maybe_identifier.unwrap();
    let (left_paren, tokens) = expect_maybe_token(tokens, TokenType::LeftParen);
    if left_paren.is_none() {
        messages.error("Expected (", &identifier.source_location.after());
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
    let mut statements: Vec<StatementAst> = Vec::new();
    while let Some((statement, rest)) = parse_statement(messages, tokens) {
        statements.push(statement);
        tokens = rest;
    }

    let (_, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    let function_definition = FunctionDeclarationSyntax {
        name: identifier.lexeme.clone(),
        params: arg_names,
        body: statements,
    };
    Some((
        Statement::FunctionDeclaration(function_definition)
            .into_ast(SourceRange::new(fun_token.source_location.start, 0)),
        tokens,
    ))
}

fn parse_expression_statement<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (expr, tokens) = parse_expression(messages, tokens)?;
    let (maybe_semicolon, tokens) = expect_maybe_token(tokens, TokenType::Semicolon);
    let end = maybe_semicolon
        .map(|t| t.source_location.end)
        .unwrap_or(expr.source_location.end);
    let start = expr.source_location.start;
    Some((
        Statement::FreeStandingExpression(Box::new(expr)).into_ast(SourceRange::new(start, end)),
        tokens,
    ))
}

fn parse_if_statement<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (if_token, tokens) = expect_token(tokens, TokenType::IF)?;
    let maybe_test = parse_expression(messages, tokens);
    if maybe_test.is_none() {
        messages.error("Expected expression", &if_token.source_location.after());
        let tokens = synchronize(tokens);
        return Some((
            Statement::Error.into_ast(if_token.source_location.after()),
            tokens,
        ));
    }
    let (test, tokens) = maybe_test.unwrap();
    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        messages.error("Expected {", &test.source_location.after());
    }
    let mut tokens = tokens;
    let mut then_statements: Vec<StatementAst> = Vec::new();
    let mut then_end = maybe_left_brace
        .map(|t| t.source_location.end)
        .unwrap_or(test.source_location.end);
    while let Some((statement, rest)) = parse_statement(messages, tokens) {
        let end = statement.source_location.end;
        then_statements.push(statement);
        tokens = rest;
        then_end = end;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        messages.error("Expected }", &SourceRange::new(then_end, then_end + 1));
    }

    let (maybe_else_token, tokens) = expect_maybe_token(tokens, TokenType::Else);

    if maybe_else_token.is_none() {
        let end = maybe_right_brace
            .map(|t| t.source_location.end)
            .unwrap_or(then_end);
        return Some((
            Statement::If(Box::new(test), then_statements, None)
                .into_ast(SourceRange::new(if_token.source_location.start, end)),
            tokens,
        ));
    }

    let else_token = maybe_else_token.unwrap();

    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        messages.error("Expected {", &else_token.source_location.after());
    }

    let mut tokens = tokens;
    let mut else_statements: Vec<StatementAst> = Vec::new();
    let mut else_end = maybe_left_brace
        .map(|t| t.source_location.end)
        .unwrap_or(else_token.source_location.end);
    while let Some((statement, rest)) = parse_statement(messages, tokens) {
        let end = statement.source_location.end;
        else_statements.push(statement);
        tokens = rest;
        else_end = end;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        messages.error("Expected }", &SourceRange::new(else_end, else_end + 1));
    }

    let if_end = maybe_right_brace
        .map(|t| t.source_location.end)
        .unwrap_or(else_end);

    Some((
        Statement::If(Box::new(test), then_statements, Some(else_statements))
            .into_ast(SourceRange::new(if_token.source_location.start, if_end)),
        tokens,
    ))
}

fn parse_while_statement<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    let (while_token, tokens) = expect_token(tokens, TokenType::While)?;
    let maybe_test = parse_expression(messages, tokens);
    if maybe_test.is_none() {
        messages.error("Expected identifier", &while_token.source_location.after());
        let tokens = synchronize(tokens);
        return Some((
            Statement::Error.into_ast(while_token.source_location),
            tokens,
        ));
    }
    let (test, tokens) = maybe_test.unwrap();
    let (maybe_left_brace, tokens) = expect_maybe_token(tokens, TokenType::LeftBrace);
    if maybe_left_brace.is_none() {
        messages.error("Expected {", &test.source_location.after());
    }
    let mut tokens = tokens;
    let mut then_statements: Vec<StatementAst> = Vec::new();
    let mut then_end = maybe_left_brace
        .map(|t| t.source_location.end)
        .unwrap_or(test.source_location.end);

    while let Some((statement, rest)) = parse_statement(messages, tokens) {
        let end = statement.source_location.end;
        then_statements.push(statement);
        tokens = rest;
        then_end = end;
    }
    let (maybe_right_brace, tokens) = expect_maybe_token(tokens, TokenType::RightBrace);
    if maybe_right_brace.is_none() {
        messages.error("Expected }", &SourceRange::new(then_end, then_end + 1));
    }

    let while_end = maybe_right_brace
        .map(|t| t.source_location.end)
        .unwrap_or(then_end);
    Some((
        Statement::While(Box::new(test), then_statements).into_ast(SourceRange::new(
            while_token.source_location.start,
            while_end,
        )),
        tokens,
    ))
}

#[derive(Debug)]
pub enum MessageLevel {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct Message {
    pub level: MessageLevel,
    pub location: SourceRange,
    pub message: String,
}

#[derive(Debug)]
pub struct Messages {
    pub warnings: Vec<Message>,
    pub errors: Vec<Message>,
}

impl Messages {
    pub fn new() -> Self {
        Messages {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn warning(&mut self, message: &str, location: &SourceRange) {
        self.warnings.push(Message {
            level: MessageLevel::Warning,
            location: SourceRange::new(location.start, location.end),
            message: String::from(message),
        });
    }

    pub fn error(&mut self, message: &str, location: &SourceRange) {
        self.errors.push(Message {
            level: MessageLevel::Error,
            location: SourceRange::new(location.start, location.end),
            message: String::from(message),
        });
    }
}

fn parse_statement<'a: 'b, 'b>(
    messages: &mut Messages,
    tokens: &'b [Token],
) -> Option<(StatementAst, &'b [Token])> {
    parse_var_declaration(messages, tokens)
        .or(parse_if_statement(messages, tokens))
        .or(parse_while_statement(messages, tokens))
        .or(parse_property_assignment(messages, tokens))
        .or(parse_variable_assignment(messages, tokens))
        .or(parse_expression_statement(messages, tokens))
        .or(parse_function_declaration(messages, tokens))
}

pub fn parse(tokens: &[Token]) -> (Vec<StatementAst>, Messages) {
    let mut messages = Messages::new();
    let mut result: Vec<StatementAst> = vec![];
    let mut current_tokens = tokens;
    loop {
        match parse_statement(&mut messages, current_tokens) {
            None => break,
            Some((statement, rest)) => {
                result.push(statement);
                current_tokens = rest;
            }
        }
    }
    (result, messages)
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
            StatementAst {
                source_location: SourceRange {
                    start: 0,
                    end: 9,
                },
                statement: VariableDeclaration(
                    "a",
                    ExpressionAst {
                        source_location: SourceRange {
                            start: 8,
                            end: 9,
                        },
                        expression: Literal(
                            Number(
                                1.0,
                            ),
                        ),
                    },
                ),
            },
        ]
        "###);
    }

    #[test]
    fn test_parse_variable_no_ident() {
        let src = "var = 1;";
        let tokens = scan(src).expect("should be able to tokenize source");
        let (ast, messages) = parse(&tokens);
        insta::assert_debug_snapshot!(ast, @r###"
        [
            StatementAst {
                source_location: SourceRange {
                    start: 3,
                    end: 4,
                },
                statement: Error,
            },
        ]
        "###);
        insta::assert_debug_snapshot!(messages, @r###"
        Messages {
            warnings: [],
            errors: [
                Message {
                    level: Error,
                    location: SourceRange {
                        start: 3,
                        end: 4,
                    },
                    message: "Expected identifier",
                },
            ],
        }
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
            StatementAst {
                source_location: SourceRange {
                    start: 12,
                    end: 13,
                },
                statement: Error,
            },
            StatementAst {
                source_location: SourceRange {
                    start: 30,
                    end: 35,
                },
                statement: VariableAssignment(
                    "b",
                    ExpressionAst {
                        source_location: SourceRange {
                            start: 34,
                            end: 35,
                        },
                        expression: Literal(
                            Number(
                                2.0,
                            ),
                        ),
                    },
                ),
            },
        ]
        "###);
        insta::assert_debug_snapshot!(errors, @r###"
        Messages {
            warnings: [],
            errors: [
                Message {
                    level: Error,
                    location: SourceRange {
                        start: 12,
                        end: 13,
                    },
                    message: "Expected identifier",
                },
            ],
        }
        "###);
    }
}
