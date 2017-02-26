use token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: String,
}

impl Lexer {
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = "0".to_string();
        } else {
            // self.ch = self.input[self.position..].chars().next().unwrap()
            self.ch = self.input[self.position..].to_string()
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> token::Token {
        let tok = match self.ch.to_string().as_str() {
            "=" => new_token(token::ASSIGN, self.ch),
            ";" => new_token(token::SEMICOLON, self.ch),
            "(" => new_token(token::LPAREN, self.ch),
            ")" => new_token(token::RPAREN, self.ch),
            "," => new_token(token::COMMA, self.ch),
            "+" => new_token(token::PLUS, self.ch),
            "{" => new_token(token::LBRACE, self.ch),
            "}" => new_token(token::RBRACE, self.ch),
            "0" => new_token(token::EOF, "".to_string()),
            _ => new_token(token::ILLEGAL, "".to_string()),
        };
        self.read_char();
        tok
    }
}

fn new_token(t: token::TokenType, ch: String) -> token::Token {
    token::Token {
        token_type: t,
        literal: ch.to_string(),
    }
}

pub fn new_lexer(input: String) -> Lexer {
    let mut l = Lexer {
        input: input,
        ch: "".to_string(),
        position: 0,
        read_position: 0,
    };
    l.read_char();
    l
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_able_to_be_analysis_simple_token() {
        // let input = "=+,;(){}";
        let input = "=";
        let expects = [(token::ASSIGN, "="),
                       //  (token::PLUS, "+"),
                       //  (token::COMMA, ","),
                       //  (token::SEMICOLON, ";"),
                       //  (token::LPAREN, "("),
                       //  (token::RPAREN, ")"),
                       //  (token::LBRACE, "{"),
                       //  (token::RBRACE, "}"),
                       (token::EOF, "")];

        let mut lex = new_lexer(input.to_string());
        for expect in &expects {
            let tok = lex.next_token();
            assert_eq!(tok.token_type, expect.0);
            assert_eq!(tok.literal, expect.1);
        }
    }
}
