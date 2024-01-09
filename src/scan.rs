use crate::utils::error;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Else,
    False,
    Fun,
    For,
    IF,
    Nil,
    Or,
    Print,
    Return,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug)]
pub enum TokenLiteral {
    Placeholder,
    NumberLiteral(f64),
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub line: usize,
}

enum ScanResult {
    TokenScanned(Token),
    StringScanned(Token, usize),
    NewLine,
    Whitespace,
    UnexpectedCharacter(char),
    CommentedLine,
}

const KEYWORDS: [(&str, TokenType); 13] = [
    ("and", TokenType::And),
    ("or", TokenType::Or),
    ("true", TokenType::True),
    ("false", TokenType::False),
    ("for", TokenType::For),
    ("while", TokenType::While),
    ("if", TokenType::IF),
    ("else", TokenType::Else),
    ("fun", TokenType::Fun),
    ("nil", TokenType::Nil),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("var", TokenType::Var),
];

fn scan_keyword(source: &str, line: usize) -> Option<(Token, &str)> {
    let mk_tok = |t: TokenType, lexeme: &str| Token {
        token_type: t,
        lexeme: String::from(lexeme),
        literal: TokenLiteral::Placeholder,
        line,
    };

    for (keyword, keyword_type) in KEYWORDS {
        if let Some(rest) = source.strip_prefix(keyword) {
            return Some((mk_tok(keyword_type, keyword), rest));
        }
    }

    None
}

fn scan_string_literal(source: &str, line: usize) -> Option<(usize, Token, &str)> {
    if let Some(rest) = source.strip_prefix('"') {
        let literal: String = rest.chars().take_while(|&c| c != '"').collect();
        let literal_len = literal.len();
        let newlines_count = literal.match_indices('\n').count();
        let end_line = line + newlines_count;
        let token = Token {
            token_type: TokenType::String,
            lexeme: literal,
            literal: TokenLiteral::Placeholder,
            line,
        };
        let rest = &source[literal_len + 1..];
        return Some((end_line, token, rest));
    }

    None
}

fn get_while(source: &str, f: fn(char) -> bool) -> (String, &str) {
    let literal: String = source.chars().take_while(|&c| f(c)).collect();
    let literal_len = literal.len();
    let rest = &source[literal_len..];
    (literal, rest)
}

fn scan_identifier(source: &str, line: usize) -> Option<(Token, &str)> {
    let next_char = source.chars().next().unwrap();
    if !next_char.is_alphabetic() {
        return None;
    }
    let (literal, rest) = get_while(source, |c| c.is_alphanumeric());
    let token = Token {
        token_type: TokenType::Identifier,
        lexeme: literal,
        literal: TokenLiteral::Placeholder,
        line,
    };
    Some((token, rest))
}

fn mk_number_token(lexeme: String, line: usize) -> Token {
    let value = lexeme.parse::<f64>().unwrap();
    Token {
        token_type: TokenType::Number,
        lexeme,
        literal: TokenLiteral::NumberLiteral(value),
        line,
    }
}

fn scan_number(source: &str, line: usize) -> Option<(Token, &str)> {
    let next_char = source.chars().next().unwrap();
    if !next_char.is_numeric() {
        return None;
    }
    let (whole_part, rest) = get_while(source, |c| c.is_alphanumeric());
    if let Some(stripped) = rest.strip_prefix('.') {
        let (fractional_part, rest) = get_while(stripped, |c| c.is_alphanumeric());
        if fractional_part.is_empty() {
            return None;
        }
        let lexeme = format!("{}.{}", whole_part, fractional_part);
        let token = mk_number_token(lexeme, line);
        Some((token, rest))
    } else {
        let token = mk_number_token(whole_part, line);
        Some((token, rest))
    }
}

fn scan_token(source: &str, line: usize) -> (ScanResult, &str) {
    assert!(!source.is_empty(), "Source should not be empty");
    let next_char = source.chars().next().unwrap();

    let mk_single_char_tok = |t: TokenType| Token {
        token_type: t,
        lexeme: String::from(next_char),
        literal: TokenLiteral::Placeholder,
        line,
    };

    let mk_tok = |t: TokenType, lexeme: &str| Token {
        token_type: t,
        lexeme: String::from(lexeme),
        literal: TokenLiteral::Placeholder,
        line,
    };

    let match_char = |c: char| {
        source
            .chars()
            .nth(1)
            .and_then(|n| if n == c { Some(c) } else { None })
    };

    let simple_token = match next_char {
        '(' => Some(mk_single_char_tok(TokenType::LeftParen)),
        ')' => Some(mk_single_char_tok(TokenType::RightParen)),
        '{' => Some(mk_single_char_tok(TokenType::LeftBrace)),
        '}' => Some(mk_single_char_tok(TokenType::RightBrace)),
        ',' => Some(mk_single_char_tok(TokenType::Comma)),
        '.' => Some(mk_single_char_tok(TokenType::Dot)),
        '-' => Some(mk_single_char_tok(TokenType::Minus)),
        '+' => Some(mk_single_char_tok(TokenType::Plus)),
        ';' => Some(mk_single_char_tok(TokenType::Semicolon)),
        '*' => Some(mk_single_char_tok(TokenType::Star)),
        '!' => match_char('=').map_or(Some(mk_single_char_tok(TokenType::Bang)), |_| {
            Some(mk_tok(TokenType::BangEqual, "!="))
        }),
        '=' => match_char('=').map_or(Some(mk_single_char_tok(TokenType::Equal)), |_| {
            Some(mk_tok(TokenType::EqualEqual, "=="))
        }),
        '<' => match_char('=').map_or(Some(mk_single_char_tok(TokenType::Less)), |_| {
            Some(mk_tok(TokenType::LessEqual, "<="))
        }),
        '>' => match_char('=').map_or(Some(mk_single_char_tok(TokenType::Greater)), |_| {
            Some(mk_tok(TokenType::GreaterEqual, ">="))
        }),
        _ => None,
    };

    if let Some(simple_token) = simple_token {
        let lexeme_length = simple_token.lexeme.len();
        return (
            ScanResult::TokenScanned(simple_token),
            &source[lexeme_length..],
        );
    }

    let whitespace_token = match next_char {
        ' ' => Some((ScanResult::Whitespace, &source[1..])),
        '\r' => Some((ScanResult::Whitespace, &source[1..])),
        '\t' => Some((ScanResult::Whitespace, &source[1..])),
        '\n' => Some((ScanResult::NewLine, &source[1..])),
        _ => None,
    };

    if let Some((ws, rest)) = whitespace_token {
        return (ws, rest);
    }

    if let Some(rest) = source.strip_prefix("//") {
        let (_, rest_of_line) = get_while(rest, |c| c.is_alphabetic());
        return (ScanResult::CommentedLine, rest_of_line);
    }

    if let Some(rest) = source.strip_prefix('/') {
        let token = mk_single_char_tok(TokenType::Slash);
        return (ScanResult::TokenScanned(token), rest);
    }

    if let Some((t, rest)) = scan_keyword(source, line) {
        return (ScanResult::TokenScanned(t), rest);
    }

    if let Some((next_line, token, rest)) = scan_string_literal(source, line) {
        return (ScanResult::StringScanned(token, next_line), rest);
    }

    if let Some((token, rest)) = scan_number(source, line) {
        return (ScanResult::TokenScanned(token), rest);
    }

    if let Some((token, rest)) = scan_identifier(source, line) {
        return (ScanResult::TokenScanned(token), rest);
    }

    (ScanResult::UnexpectedCharacter(next_char), &source[1..])
}

pub fn scan(mut source: &str) -> anyhow::Result<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut line: usize = 0;

    while !source.is_empty() {
        let (token, rest_of_source) = scan_token(source, line);
        match token {
            ScanResult::TokenScanned(t) => tokens.push(t),
            ScanResult::StringScanned(string, next_line) => {
                line = next_line;
                tokens.push(string)
            }
            ScanResult::NewLine => line += 1,
            ScanResult::Whitespace => {}
            ScanResult::CommentedLine => {}
            ScanResult::UnexpectedCharacter(c) => error(line, format!("Unexpected character {c}")),
        }
        source = rest_of_source;
    }

    tokens.push(Token {
        token_type: TokenType::Eof,
        lexeme: String::from(""),
        literal: TokenLiteral::Placeholder,
        line,
    });

    Ok(tokens)
}
