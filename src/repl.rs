use std::io;

use lexer;
use token::TokenType;

pub fn run() {
    let prompt = ">>";
    let mut scan = String::new();

    println!("read print eval loop is started");
    println!("{}", prompt);

    loop {
        io::stdin()
            .read_line(&mut scan)
            .expect("Failed to read line");

        let mut lex = lexer::new(scan.clone());

        while lex.next() {
            let t = lex.next_token();
            if t.token_type == TokenType::EOF {
                break;
            }
            println!("{:?}", t.token_type);
        }
        print!("{}", prompt);
    }
}

