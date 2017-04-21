use utils::{is_digit, is_letter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    STRING(String),
    ASSIGN,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    LT,
    GT,
    BANG,
    COMMA,
    COLON,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOTEQ,
}

impl TokenType {
    pub fn from_string(s: &String) -> TokenType {
        match s.as_str() {
            "+" => TokenType::PLUS,
            "-" => TokenType::MINUS,
            "*" => TokenType::MULTIPLY,
            "/" => TokenType::DIVIDE,
            "<" => TokenType::LT,
            ">" => TokenType::GT,
            "," => TokenType::COMMA,
            ";" => TokenType::SEMICOLON,
            ":" => TokenType::COLON,
            "(" => TokenType::LPAREN,
            ")" => TokenType::RPAREN,
            "{" => TokenType::LBRACE,
            "}" => TokenType::RBRACE,
            "[" => TokenType::LBRACKET,
            "]" => TokenType::RBRACKET,
            "let" => TokenType::LET,
            "fn" => TokenType::FUNCTION,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            "" => TokenType::EOF,
            "=" => TokenType::ASSIGN,
            "!" => TokenType::BANG,
            "==" => TokenType::EQ,
            "!=" => TokenType::NOTEQ,
            _ if is_digit(s) => TokenType::INT(s.clone()),
            _ if is_letter(s) => TokenType::IDENT(s.clone()),
            _ => TokenType::ILLEGAL,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line_num: u8,
    pub column_num: u8,
}

impl Token {
    pub fn new(s: String, is_string: bool, line_num: u8, column_num: u8) -> Self {
        if is_string {
            return Token::new_string(s, line_num, column_num);
        }
        Token {
            token_type: TokenType::from_string(&s),
            literal: s,
            line_num: line_num,
            column_num: column_num,
        }
    }

    fn new_string(s: String, line_num: u8, column_num: u8) -> Self {
        Token {
            token_type: TokenType::STRING(s.clone()),
            literal: s,
            line_num: line_num,
            column_num: column_num,
        }
    }
}

