use lexer::token;
use utils::{is_digit, is_letter, EMPTY_STR};

#[derive(Debug, Clone)]
pub struct Lexer {
    pub input: String,
    pub current_char: String,
    pub position: i32,
    pub read_position: i32,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input,
            current_char: EMPTY_STR.to_string(),
            position: 0,
            read_position: 1,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        let position = self.position as usize;

        self.current_char = match self.input.chars().nth(position) {
            Some(x) => x.to_string(),
            None => EMPTY_STR.to_string(),
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peak_char(&self) -> String {
        match self.input.chars().nth((self.read_position - 1) as usize) {
            Some(x) => x.to_string(),
            None => EMPTY_STR.to_string(),
        }
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_white_space();
        let current_char = &self.current_char.clone();
        let mut is_string = false;
        let seed = match current_char {
            x if is_letter(x) => self.read_identifier(),
            x if is_digit(x) => self.read_digit(),
            x if x == "=" && self.peak_char() == "=" => {
                self.read_char();
                self.read_char();
                format!("{}{}", x, "=")
            },
            x if x == "!" && self.peak_char() == "=" => {
                self.read_char();
                self.read_char();
                format!("{}{}", x, "=")
            },
            x if x == "\"" => {
                is_string = true;
                self.read_string()
            },
            x => {
                self.read_char();
                x.clone()
            },
        };

        token::Token::new(seed, is_string)
    }

    fn read_string(&mut self) -> String {
        self.read_char();
        let start = (self.position - 1) as usize;

        while self.current_char != "\"" {
            self.read_char();
        }

        let input_chars = self.input.chars().collect::<Vec<char>>();
        let end = (self.position - 1) as usize;
        self.read_char();

        (&input_chars[start..end])
            .iter()
            .fold("".to_string(), |acc, &s| { format!("{}{}", acc, s.to_string()) })
    }

    fn read_identifier(&mut self) -> String {
        let start = (self.position - 1) as usize;

        while is_letter(&self.current_char) {
            self.read_char();
        }

        let input_chars = self.input.chars().collect::<Vec<char>>();
        let end = (self.position - 1) as usize;
        let splited = &input_chars[start..end].iter().fold("".to_string(), |acc, &s| { format!("{}{}", acc, s.to_string()) });
        (*splited).to_string()
    }

    fn read_digit(&mut self) -> String {
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

#[cfg(test)]
mod tests {
    use super::*;
    use self::token::TokenType::*;

    #[test]
    fn it_should_analysis_arithmetic() {
        let mut l = Lexer::new("!-*/<>".to_string());
        let expects = [
            (BANG, "!"),
            (MINUS, "-"),
            (MULTIPLY, "*"),
            (DIVIDE, "/"),
            (LT, "<"),
            (GT, ">"),
            (EOF, "")
        ];

        for expect in expects.iter() {
            let t = l.next_token();
            assert_eq!(t.token_type, expect.0);
            assert_eq!(t.literal, expect.1);
        }
    }

    #[test]
    fn it_should_analysis_control_syntax() {
        let mut l = Lexer::new("
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        ".to_string());
        let expects = vec![
            (IF, "if"),
            (LPAREN, "("),
            (INT("5".to_string()), "5"),
            (LT, "<"),
            (INT("10".to_string()), "10"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (TRUE, "true"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (ELSE, "else"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (FALSE, "false"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (EOF, "")
        ];

        for (token_type, literal) in expects {
            let t = l.next_token();
            assert_eq!(t.token_type, token_type);
            assert_eq!(t.literal, literal);
        }
    }

    #[test]
    fn it_should_analysis_comparison_operator() {
        let mut l = Lexer::new("
            if (10 == 10) 
            if (5 != 10) 
        ".to_string());

        let expects = vec![
            (IF, "if"),
            (LPAREN, "("),
            (INT("10".to_string()), "10"),
            (EQ, "=="),
            (INT("10".to_string()), "10"),
            (RPAREN, ")"),

            (IF, "if"),
            (LPAREN, "("),
            (INT("5".to_string()), "5"),
            (NOTEQ, "!="),
            (INT("10".to_string()), "10"),
            (RPAREN, ")"),

            (EOF, "")
        ];

        for (token_type, literal) in expects {
            let t = l.next_token();
            assert_eq!(t.token_type, token_type);
            assert_eq!(t.literal, literal);
        }
    }

    #[test]
    fn it_analysis_simple_token() {
        let mut l = Lexer::new(r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
            x + y;
            };

            let result = add(five, ten);
            "foobar";
            "foo bar";
            [1, 2];
            { "foo": "bar" };
        "#.to_string());

        let expects = vec![
            (LET, "let"),
            (IDENT("five".to_string()), "five"),
            (ASSIGN, "="),
            (INT("5".to_string()), "5"),
            (SEMICOLON, ";"),

            (LET, "let"),
            (IDENT("ten".to_string()), "ten"),
            (ASSIGN, "="),
            (INT("10".to_string()), "10"),
            (SEMICOLON, ";"),

            (LET, "let"),
            (IDENT("add".to_string()), "add"),
            (ASSIGN, "="),
            (FUNCTION, "fn"),
            (LPAREN, "("),
            (IDENT("x".to_string()), "x"),
            (COMMA, ","),
            (IDENT("y".to_string()), "y"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (IDENT("x".to_string()), "x"),
            (PLUS, "+"),
            (IDENT("y".to_string()), "y"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (SEMICOLON, ";"),

            (LET, "let"),
            (IDENT("result".to_string()), "result"),
            (ASSIGN, "="),
            (IDENT("add".to_string()), "add"),
            (LPAREN, "("),
            (IDENT("five".to_string()), "five"),
            (COMMA, ","),
            (IDENT("ten".to_string()), "ten"),
            (RPAREN, ")"),
            (SEMICOLON, ";"),

            (STRING("foobar".to_string()), "foobar"),
            (SEMICOLON, ";"),
            (STRING("foo bar".to_string()), "foo bar"),
            (SEMICOLON, ";"),

            (LBRACKET, "["),
            (INT("1".to_string()), "1"),
            (COMMA, ","),
            (INT("2".to_string()), "2"),
            (RBRACKET, "]"),
            (SEMICOLON, ";"),

            (LBRACE, "{"),
            (STRING("foo".to_string()), "foo"),
            (COLON, ":"),
            (STRING("bar".to_string()), "bar"),
            (RBRACE, "}"),
            (SEMICOLON, ";"),

            (EOF, "")
        ];

        for (token_type, literal) in expects {
            let t = l.next_token();
            assert_eq!(t.token_type, token_type);
            assert_eq!(t.literal, literal);
        }
    }
}