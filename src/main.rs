extern crate monkey;

use monkey::{lexer, token};

fn main() {
    let l = lexer::new("=+,;(){}");
    let c = token::TokenType::from_str(";");
    let t = token::new(c);

    for c in l.input.chars() {
        print!("{}", c);
    }

    // print!("{}", l.input.chars());
    println!("\n{}", t.literal);
}