#[derive(Debug, PartialEq, Clone)]
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
    Colon,
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
    Return,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone)]
pub enum TokenLiteral {
    Placeholder,
    NumberLiteral(f64),
}

#[derive(Debug, Clone)]
pub struct SourceRange {
    pub start: usize, // inclusie
    pub end: usize,   // exclusive
}

impl SourceRange {
    pub fn new(start: usize, end: usize) -> Self {
        SourceRange { start, end }
    }

    pub fn after(&self) -> SourceRange {
        SourceRange {
            start: self.end,
            end: self.end + 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub source_location: SourceRange,
}

enum ScanResult {
    TokenScanned(Token),
    NewLine,
    Whitespace,
    UnexpectedCharacter(char),
    CommentedLine,
}

const KEYWORDS: [(&str, TokenType); 12] = [
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
    ("return", TokenType::Return),
    ("var", TokenType::Var),
];

fn scan_keyword(source: &str, source_offset: usize) -> Option<(Token, usize, &str)> {
    let mk_tok = |t: TokenType, lexeme: &str| Token {
        token_type: t,
        lexeme: String::from(lexeme),
        literal: TokenLiteral::Placeholder,
        source_location: SourceRange {
            start: source_offset,
            end: source_offset + lexeme.len(),
        },
    };

    for (keyword, keyword_type) in KEYWORDS {
        if let Some(rest) = source.strip_prefix(keyword) {
            return Some((
                mk_tok(keyword_type, keyword),
                source_offset + keyword.len(),
                rest,
            ));
        }
    }

    None
}

fn scan_string_literal(source: &str, source_offset: usize) -> Option<(Token, usize, &str)> {
    if let Some(rest) = source.strip_prefix('"') {
        let literal: String = rest.chars().take_while(|&c| c != '"').collect();
        let literal_len = literal.len() + 1;
        let next_offset = source_offset + 1 + literal_len;
        let token = Token {
            token_type: TokenType::String,
            lexeme: literal,
            literal: TokenLiteral::Placeholder,
            source_location: SourceRange {
                start: source_offset,
                end: next_offset,
            },
        };
        let rest = &source[literal_len + 1..];
        return Some((token, next_offset, rest));
    }

    None
}

fn get_while(source: &str, f: fn(char) -> bool) -> (String, &str) {
    let literal: String = source.chars().take_while(|&c| f(c)).collect();
    let literal_len = literal.len();
    let rest = &source[literal_len..];
    (literal, rest)
}

fn scan_identifier(source: &str, source_offset: usize) -> Option<(Token, usize, &str)> {
    let next_char = source.chars().next().unwrap();
    if !next_char.is_alphabetic() {
        return None;
    }
    let (literal, rest) = get_while(source, |c| c.is_alphanumeric() || c == '_');
    let next_offset = source_offset + literal.len();
    let token = Token {
        token_type: TokenType::Identifier,
        lexeme: literal,
        literal: TokenLiteral::Placeholder,
        source_location: SourceRange {
            start: source_offset,
            end: next_offset,
        },
    };
    Some((token, next_offset, rest))
}

fn mk_number_token(lexeme: String, source_offset: usize) -> (Token, usize) {
    let value = lexeme.parse::<f64>().unwrap();
    let next_offset = source_offset + lexeme.len();
    (
        Token {
            token_type: TokenType::Number,
            lexeme,
            literal: TokenLiteral::NumberLiteral(value),
            source_location: SourceRange {
                start: source_offset,
                end: next_offset,
            },
        },
        next_offset,
    )
}

fn scan_number(source: &str, source_offset: usize) -> Option<(Token, usize, &str)> {
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
        let (token, next_offset) = mk_number_token(lexeme, source_offset);
        Some((token, next_offset, rest))
    } else {
        let (token, next_offset) = mk_number_token(whole_part, source_offset);
        Some((token, next_offset, rest))
    }
}

fn scan_token(source: &str, source_offset: usize) -> (ScanResult, usize, &str) {
    assert!(!source.is_empty(), "Source should not be empty");
    let next_char = source.chars().next().unwrap();

    let mk_single_char_tok = |t: TokenType| Token {
        token_type: t,
        lexeme: String::from(next_char),
        literal: TokenLiteral::Placeholder,
        source_location: SourceRange {
            start: source_offset,
            end: source_offset + 1,
        },
    };

    let mk_tok = |t: TokenType, lexeme: &str| Token {
        token_type: t,
        lexeme: String::from(lexeme),
        literal: TokenLiteral::Placeholder,
        source_location: SourceRange {
            start: source_offset,
            end: source_offset + lexeme.len(),
        },
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
        ':' => Some(mk_single_char_tok(TokenType::Colon)),
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
            source_offset + lexeme_length,
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
        return (ws, source_offset + 1, rest);
    }

    if let Some(rest) = source.strip_prefix("//") {
        let (line, rest_of_line) = get_while(rest, |c| c.is_alphabetic());
        let next_offset = source_offset + 2 + line.len();
        return (ScanResult::CommentedLine, next_offset, rest_of_line);
    }

    if let Some(rest) = source.strip_prefix('/') {
        let token = mk_single_char_tok(TokenType::Slash);
        return (ScanResult::TokenScanned(token), source_offset + 1, rest);
    }

    if let Some((t, next_offset, rest)) = scan_keyword(source, source_offset) {
        return (ScanResult::TokenScanned(t), next_offset, rest);
    }

    if let Some((token, next_offset, rest)) = scan_string_literal(source, source_offset) {
        return (ScanResult::TokenScanned(token), next_offset, rest);
    }

    if let Some((token, next_offset, rest)) = scan_number(source, source_offset) {
        return (ScanResult::TokenScanned(token), next_offset, rest);
    }

    if let Some((token, next_offset, rest)) = scan_identifier(source, source_offset) {
        return (ScanResult::TokenScanned(token), next_offset, rest);
    }

    (
        ScanResult::UnexpectedCharacter(next_char),
        source_offset + 1,
        &source[1..],
    )
}

pub fn scan(mut source: &str) -> anyhow::Result<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut source_offset: usize = 0;

    while !source.is_empty() {
        let (token, next_offset, rest_of_source) = scan_token(source, source_offset);
        match token {
            ScanResult::TokenScanned(t) => tokens.push(t),
            ScanResult::NewLine => {}
            ScanResult::Whitespace => {}
            ScanResult::CommentedLine => {}
            ScanResult::UnexpectedCharacter(_) => {}
        }
        source_offset = next_offset;
        source = rest_of_source;
    }

    tokens.push(Token {
        token_type: TokenType::Eof,
        lexeme: String::from(""),
        literal: TokenLiteral::Placeholder,
        source_location: SourceRange {
            start: source_offset,
            end: source_offset + 1,
        },
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::scan;

    #[test]
    fn test_scan_var_with_string() {
        let src = r###"var a = "hello"; "###;
        let tokens = scan(src).expect("should be able to scan source");
        insta::assert_debug_snapshot!(tokens, @r###"
        [
            Token {
                token_type: Var,
                lexeme: "var",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 0,
                    end: 3,
                },
            },
            Token {
                token_type: Identifier,
                lexeme: "a",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 4,
                    end: 5,
                },
            },
            Token {
                token_type: Equal,
                lexeme: "=",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 6,
                    end: 7,
                },
            },
            Token {
                token_type: String,
                lexeme: "hello",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 8,
                    end: 15,
                },
            },
            Token {
                token_type: Semicolon,
                lexeme: ";",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 15,
                    end: 16,
                },
            },
            Token {
                token_type: Eof,
                lexeme: "",
                literal: Placeholder,
                source_location: SourceRange {
                    start: 17,
                    end: 18,
                },
            },
        ]
        "###);
    }
}
