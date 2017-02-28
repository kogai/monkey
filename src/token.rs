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
    pub fn from_str(s: &'static str) -> TokenType {
        match s {
            "=" => TokenType::ASSIGN,
            "+" => TokenType::PLUS,
            "," => TokenType::COMMA,
            ";" => TokenType::SEMICOLON,
            "(" => TokenType::LPAREN,
            ")" => TokenType::RPAREN,
            "{" => TokenType::LBRACE,
            "}" => TokenType::RBRACE,
            _ => TokenType::EOF,
        }
    }
    
    pub fn to_str(&self) -> &'static str {
         match *self {
             TokenType::EOF => "EOF",
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
