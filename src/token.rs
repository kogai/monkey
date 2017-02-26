pub type TokenType = &'static str;

pub struct Token {
  pub token_type: TokenType,
  pub literal: String,
}

pub const ILLEGAL: &'static str = "ILLEGAL";
pub const EOF: &'static str = "EOF";
pub const IDENT: &'static str = "IDENT";
pub const INT: &'static str = "INT";
pub const ASSIGN: &'static str = "=";
pub const PLUS: &'static str = "+";
pub const COMMA: &'static str = ",";
pub const SEMICOLON: &'static str = ";";
pub const LPAREN: &'static str = "(";
pub const RPAREN: &'static str = ")";
pub const LBRACE: &'static str = "{";
pub const RBRACE: &'static str = "}";
pub const FUNCTION: &'static str = "FUNCTION";
pub const LET: &'static str = "LET";

pub fn hello() -> TokenType {
  "こんにちは!!!"
}

// var keywords = map[string]TokenType{
// 	"fn":  FUNCTION,
// 	"let": LET,
// }

// func LookUpIdent(ident string) TokenType {
// 	if tok, ok := keywords[ident]; ok {
// 		return tok
// 	}
// 	return IDENT
// }
