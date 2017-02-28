extern crate monkey;

use monkey::{lexer, token};

fn main() {
    let l = lexer::new("=+,;(){}");
    // let c = token::TokenType::from_str("=");

    for c in l.input.chars() {
        print!("{}", token::TokenType::from_str(c).to_str());
    }
    // print!("{}", l.input.chars());
    // println!("\n{}", c.to_str());
}