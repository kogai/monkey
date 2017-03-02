use token;

pub struct Lexer<'a> {
    pub input: &'a str,
    pub current_char: String,
    pub position: u16,
    pub read_position: u16,
}

impl<'a> Lexer<'a> {
    fn read_char(&mut self) {
        let position = self.position as usize;

        self.current_char = match self.input.chars().nth(position) {
            Some(x) => x.to_string(),
            None => "".to_string(),
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token<'b>(&mut self) -> token::Token<'b> {
        let t = token::new(token::TokenType::from_str(self.current_char.as_str()));
        self.read_char();
        return t;
    }
}

pub fn new(input: &'static str) -> Lexer {
    let mut l = Lexer {
        input: input,
        current_char: "".to_string(),
        position: 0,
        read_position: 1,
    };
    l.read_char();
    l
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_analysis_simple_token() {
        let mut l = new("=+,;(){}");
        let expects = [
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::PLUS, "+"),
            (token::TokenType::COMMA, ","),
            (token::TokenType::SEMICOLON, ";"),
            (token::TokenType::LPAREN, "("),
            (token::TokenType::RPAREN, ")"),
            (token::TokenType::LBRACE, "{"),
            (token::TokenType::RBRACE, "}"),
            (token::TokenType::EOF, ""),
        ];

        for expect in &expects {
            let t = l.next_token();
            assert_eq!(t.token_type, expect.0);
            assert_eq!(t.literal, expect.1);
        }
    }
}