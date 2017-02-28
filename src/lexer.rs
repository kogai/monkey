pub struct Lexer {
    pub input: &'static str,
    pub current_char: String,
    pub position: u16,
    pub read_position: u16,
}

impl Lexer {
}

pub fn new(input: &'static str) -> Lexer {
    Lexer {
        input: input,
        current_char: "".to_string(),
        position: 0,
        read_position: 0,
    }
}