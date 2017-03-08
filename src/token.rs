use lexer::is_digit;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

impl TokenType {
    pub fn from_str<'a>(s: &'a str) -> TokenType {
        match s {
            "=" => TokenType::ASSIGN,
            "+" => TokenType::PLUS,
            "," => TokenType::COMMA,
            ";" => TokenType::SEMICOLON,
            "(" => TokenType::LPAREN,
            ")" => TokenType::RPAREN,
            "{" => TokenType::LBRACE,
            "}" => TokenType::RBRACE,
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            "" => TokenType::EOF,
            n if is_digit(&n.to_string()) => TokenType::INT(n.to_string()),
            id => TokenType::IDENT(id.to_string()),
        }
    }

    pub fn to_str<'a>(&'a self) -> String {
         (match *self {
             TokenType::EOF => "",
             TokenType::ASSIGN => "=",
             TokenType::PLUS => "+",
             TokenType::COMMA => ",",
             TokenType::SEMICOLON => ";",
             TokenType::LPAREN => "(",
             TokenType::RPAREN => ")",
             TokenType::LBRACE => "{",
             TokenType::RBRACE => "}",
             TokenType::FUNCTION => "fn",
             TokenType::LET => "let",
             TokenType::INT(ref x) => x,
             TokenType::IDENT(ref x) => x,
             _ => "ILLEGAL",
         }).to_string()
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn new(t: TokenType) -> Token {
    Token {
        token_type: t.clone(),
        literal: t.to_str(),
    }
}
