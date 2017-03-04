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
        self.skip_white_space();
        let t = token::new(
            token::TokenType::from_str(
                self.current_char.as_str()
            )
        );
        self.read_char();
        return t;
    }
    pub fn skip_white_space(&mut self) {
        let is_whitespace = match self.current_char.chars().last() {
            Some(x) => x.is_whitespace(),
            None => false,
        };
        if is_whitespace {
            self.read_char();
            self.skip_white_space()
        }
    }
}

fn is_letter(s: &str) -> bool {
    "a" <= s && s <= "z" || "A" <= s && s <= "Z" || "_" == s
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
    fn it_should_detect_character() {
        assert!(is_letter("a"));
        assert!(is_letter("Z"));
        assert!(is_letter("_"));
        assert!(!is_letter("-"));
        assert!(!is_letter("あ"));
        assert!(!is_letter(" "));
        assert!(!is_letter("漢"));
    }

    fn it_analysis_simple_token() {
        let mut l = new("
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
            x + y;
            };

            let result = add(five, ten);
        ");
        let expects = [
            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT, "five"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::INT, "5"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT, "ten"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::INT, "10"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT, "add"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::FUNCTION, "fn"),
            (token::TokenType::LPAREN, "("),
            (token::TokenType::IDENT, "x"),
            (token::TokenType::COMMA, ","),
            (token::TokenType::IDENT, "y"),
            (token::TokenType::RPAREN, ")"),
            (token::TokenType::LBRACE, "{"),
            (token::TokenType::IDENT, "x"),
            (token::TokenType::PLUS, "+"),
            (token::TokenType::IDENT, "y"),
            (token::TokenType::SEMICOLON, ";"),
            (token::TokenType::RBRACE, "}"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT, "result"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::IDENT, "add"),
            (token::TokenType::LPAREN, "("),
            (token::TokenType::IDENT, "five"),
            (token::TokenType::COMMA, ","),
            (token::TokenType::IDENT, "ten"),
            (token::TokenType::RPAREN, ")"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::EOF, "")
        ];

        for expect in expects.iter() {
            let t = l.next_token();
            assert_eq!(t.token_type, expect.0);
            assert_eq!(t.literal, expect.1);
        }
    }
}