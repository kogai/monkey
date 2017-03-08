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

    pub fn next_token(&mut self) -> token::Token {
        self.skip_white_space();
        let current_char = &self.current_char.clone();

        let seed = match current_char {
            x if is_letter(x) => self.read_identifier(),
            x if is_digit(x) => self.read_digit(),
            x => {
                self.read_char();
                x.clone()
            },
        };
        token::new(
            token::TokenType::from_str(
                seed.as_str()
            )
        )
    }

    pub fn read_identifier(&mut self) -> String {
        let start = (self.position - 1) as usize;

        while is_letter(&self.current_char) {
            self.read_char();
        }

        let input_chars = self.input.chars().collect::<Vec<char>>();
        let end = (self.position - 1) as usize;
        let splited = &input_chars[start..end].iter().fold("".to_string(), |acc, &s| { format!("{}{}", acc, s.to_string()) });
        (*splited).to_string()
    }

    pub fn read_digit(&mut self) -> String {
        let start = (self.position - 1) as usize;

        while is_digit(&self.current_char) {
            self.read_char();
        }

        let input_chars = self.input.chars().collect::<Vec<char>>();
        let end = (self.position - 1) as usize;
        let splited = &input_chars[start..end].iter().fold("".to_string(), |acc, &s| { format!("{}{}", acc, s.to_string()) });
        (*splited).to_string()
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

fn is_letter(s: &String) -> bool {
    &"a".to_string() <= s && s <= &"z".to_string() || &"A".to_string() <= s && s <= &"Z".to_string() || &"_".to_string() == s
}

pub fn is_digit(s: &String) -> bool {
    let c = s.chars().nth(0);
    match c {
        Some(n) => n.is_digit(10),
        None => false,
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
    fn it_should_detect_character() {
        assert!(is_letter(&"a".to_string()));
        assert!(is_letter(&"Z".to_string()));
        assert!(is_letter(&"_".to_string()));
        assert!(!is_letter(&"-".to_string()));
        assert!(!is_letter(&"あ".to_string()));
        assert!(!is_letter(&" ".to_string()));
        assert!(!is_letter(&"漢".to_string()));
    }

    #[test]
    fn it_should_detect_digit() {
        assert!(!is_digit(&"a".to_string()));
        assert!(!is_digit(&"Z".to_string()));
        assert!(!is_digit(&"_".to_string()));
        assert!(is_digit(&"0".to_string()));
        assert!(is_digit(&"9".to_string()));
    }

    #[test]
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
            (token::TokenType::IDENT("five".to_string()), "five"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::INT("5".to_string()), "5"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT("ten".to_string()), "ten"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::INT("10".to_string()), "10"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT("add".to_string()), "add"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::FUNCTION, "fn"),
            (token::TokenType::LPAREN, "("),
            (token::TokenType::IDENT("x".to_string()), "x"),
            (token::TokenType::COMMA, ","),
            (token::TokenType::IDENT("y".to_string()), "y"),
            (token::TokenType::RPAREN, ")"),
            (token::TokenType::LBRACE, "{"),
            (token::TokenType::IDENT("x".to_string()), "x"),
            (token::TokenType::PLUS, "+"),
            (token::TokenType::IDENT("y".to_string()), "y"),
            (token::TokenType::SEMICOLON, ";"),
            (token::TokenType::RBRACE, "}"),
            (token::TokenType::SEMICOLON, ";"),

            (token::TokenType::LET, "let"),
            (token::TokenType::IDENT("result".to_string()), "result"),
            (token::TokenType::ASSIGN, "="),
            (token::TokenType::IDENT("add".to_string()), "add"),
            (token::TokenType::LPAREN, "("),
            (token::TokenType::IDENT("five".to_string()), "five"),
            (token::TokenType::COMMA, ","),
            (token::TokenType::IDENT("ten".to_string()), "ten"),
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