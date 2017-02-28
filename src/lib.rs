pub mod token;
pub mod lexer;

// use token;

// pub struct Lexer {
//     input: String,
//     position: usize,
//     read_position: usize,
//     ch: String,
// }

// impl Lexer {
//     fn read_char(&mut self) {
//         if self.read_position >= self.input.len() {
//             self.ch = "0".to_string();
//         } else {
//             // self.ch = self.input[self.position..].chars().next().unwrap()
//             self.ch = self.input[self.position..].to_string()
//         }

//         self.position = self.read_position;
//         self.read_position += 1;
//     }

//     fn next_token(&mut self) -> token::Token {
//         let emp = "".to_string();
//         let tok = match self.ch.to_string().as_str() {
//             "=" => new_token(token::ASSIGN, &self.ch),
//             ";" => new_token(token::SEMICOLON, &self.ch),
//             "(" => new_token(token::LPAREN, &self.ch),
//             ")" => new_token(token::RPAREN, &self.ch),
//             "," => new_token(token::COMMA, &self.ch),
//             "+" => new_token(token::PLUS, &self.ch),
//             "{" => new_token(token::LBRACE, &self.ch),
//             "}" => new_token(token::RBRACE, &self.ch),
//             "0" => new_token(token::EOF, &emp),
//             _ => new_token(token::ILLEGAL, &emp),
//         };
//         self.read_char();
//         tok
//     }
// }

// fn new_token<'a>(t: token::TokenType, ch: &'a String) -> token::Token<'a> {
//     token::Token {
//         token_type: t,
//         literal: ch,
//     }
// }

// pub fn new_lexer(input: String) -> Lexer {
//     let mut l = Lexer {
//         input: input,
//         ch: "".to_string(),
//         position: 0,
//         read_position: 0,
//     };
//     l.read_char();
//     l
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_should_able_to_be_analysis_simple_token() {
//         // let input = "=+,;(){}";
//         let input = "=";
//         let expects = [(token::ASSIGN, "="),
//                        //  (token::PLUS, "+"),
//                        //  (token::COMMA, ","),
//                        //  (token::SEMICOLON, ";"),
//                        //  (token::LPAREN, "("),
//                        //  (token::RPAREN, ")"),
//                        //  (token::LBRACE, "{"),
//                        //  (token::RBRACE, "}"),
//                        (token::EOF, "")];

//         let mut lex = new_lexer(input.to_string());
//         for expect in &expects {
//             let tok = lex.next_token();
//             assert_eq!(tok.token_type, expect.0);
//             assert_eq!(tok.literal, expect.1);
//         }
//     }
// }

// pub type TokenType = &'static str;

// pub struct Token<'a> {
//     pub token_type: TokenType,
//     pub literal: &'a String,
// }

// pub const ILLEGAL: &'static str = "ILLEGAL";
// pub const EOF: &'static str = "EOF";
// pub const IDENT: &'static str = "IDENT";
// pub const INT: &'static str = "INT";
// pub const ASSIGN: &'static str = "=";
// pub const PLUS: &'static str = "+";
// pub const COMMA: &'static str = ",";
// pub const SEMICOLON: &'static str = ";";
// pub const LPAREN: &'static str = "(";
// pub const RPAREN: &'static str = ")";
// pub const LBRACE: &'static str = "{";
// pub const RBRACE: &'static str = "}";
// pub const FUNCTION: &'static str = "FUNCTION";
// pub const LET: &'static str = "LET";

// pub fn hello() -> TokenType {
//     "こんにちは!!!"
// }

// // var keywords = map[string]TokenType{
// // 	"fn":  FUNCTION,
// // 	"let": LET,
// // }

// // func LookUpIdent(ident string) TokenType {
// // 	if tok, ok := keywords[ident]; ok {
// // 		return tok
// // 	}
// // 	return IDENT
// // }

