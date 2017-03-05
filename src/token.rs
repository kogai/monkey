use lexer::is_digit;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
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
            n if is_digit(&n.to_string()) => TokenType::INT,
            _ => TokenType::IDENT,
        }
    }
    
    pub fn to_str(&self) -> &'static str {
         match *self {
             TokenType::EOF => "",
             TokenType::IDENT => "IDENT",
             TokenType::INT => "INT",
             TokenType::ASSIGN => "=",
             TokenType::PLUS => "+",
             TokenType::COMMA => ",",
             TokenType::SEMICOLON => ";",
             TokenType::LPAREN => "(",
             TokenType::RPAREN => ")",
             TokenType::LBRACE => "{",
             TokenType::RBRACE => "}",
             TokenType::FUNCTION => "FUNCTION",
             TokenType::LET => "LET",
             _ => "ILLEGAL",
         }
    }
}

pub struct Token<'a> {
    pub token_type: TokenType,
    pub literal: &'a str,
}

pub fn new<'a>(t: TokenType) -> Token<'a> {
    Token {
        literal: t.to_str(),
        token_type: t,
    }
}
