use lexer::token;
use utils::{is_digit, is_letter, EMPTY_STR};

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    current_char: String,
    position: i32,
    read_position: i32,
    line: u8,
    column: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input,
            current_char: EMPTY_STR.to_string(),
            position: 0,
            read_position: 1,
            line: 1,
            column: 0,
        };
        l.read_char();
        l
    }

    fn get_char(&self, position: i32) -> String {
        match self.input.chars().nth(position as usize) {
            Some(x) => x.to_string(),
            None => EMPTY_STR.to_string(),
        }
    }

    fn read_char(&mut self) {
        self.current_char = self.get_char(self.position);
        self.position = self.read_position;
        self.column = self.column + 1;
        self.read_position += 1;
    }

    fn peak_char(&self) -> String {
        self.get_char(self.read_position - 1)
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_white_space();
        let current_char = &self.current_char.clone();
        let position = self.column;

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

        token::Token::new(seed, is_string, self.line, position)
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
        let (is_whitespace, is_newline) = match self.current_char.chars().last() {
            Some(x) => {
                (x.is_whitespace(), x == '\n')
            },
            _ => (false, false),
        };
        if is_newline {
            self.column = 0;
            self.line = self.line + 1;
        };
        if is_whitespace {
            self.read_char();
            self.skip_white_space()
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::token::TokenType::*;

    #[test]
    fn it_should_analysis_arithmetic() {
        let mut l = Lexer::new("!-*/<>".to_string());
        let expects = vec![
            (BANG, "!", 1),
            (MINUS, "-", 2),
            (MULTIPLY, "*", 3),
            (DIVIDE, "/", 4),
            (LT, "<", 5),
            (GT, ">", 6),
            (EOF, "", 7)
        ];

        for (token_type, literal, column_num) in expects {
            let t = l.next_token();
            assert_eq!(t.token_type, token_type);
            assert_eq!(t.literal, literal);
            assert_eq!(t.column_num, column_num);
        }
    }

    #[test]
    fn it_should_count_mutiple_line() {
        let mut l = Lexer::new("
        let x = 0;
        let y = 1;
        ".to_string());
        let expects = vec![
            (2, 9), (2,13), (2, 15), (2, 17), (2, 18),
            (3, 9), (3,13), (3, 15), (3, 17), (3, 18)
        ];

        for (line_num, column_num) in expects {
            let t = l.next_token();
            assert_eq!(t.line_num, line_num);
            assert_eq!(t.column_num, column_num);
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
            (IF, "if", 2),
            (LPAREN, "(", 2),
            (INT("10".to_string()), "10", 2),
            (EQ, "==", 2),
            (INT("10".to_string()), "10", 2),
            (RPAREN, ")", 2),

            (IF, "if", 3),
            (LPAREN, "(", 3),
            (INT("5".to_string()), "5", 3),
            (NOTEQ, "!=", 3),
            (INT("10".to_string()), "10", 3),
            (RPAREN, ")", 3),

            (EOF, "", 4)
        ];

        for (token_type, literal, line_num) in expects {
            let t = l.next_token();
            assert_eq!(t.token_type, token_type);
            assert_eq!(t.line_num, line_num);
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