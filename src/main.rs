extern crate monkey;

use monkey::token::{hello, EOF};

fn main() {
    // let n = 5;
    println!("Hello in Japanese: {}", hello());
    println!("Hello in Japanese: {}", EOF);

    // let l = new(";.".to_string());
    // println!("Hello in Japanese: {}", l.input);
    // println!("Hello in Japanese: {}", l.ch);
    // println!("Hello in Japanese: {}", l.position);
    // println!("Hello in Japanese: {}", l.read_position);
}