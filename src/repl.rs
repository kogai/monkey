use std::io::{self, Write};

use lexer::lexer;
use parser::parser;
use parser::ast::Node;
use evaluator::evaluator::eval;
use evaluator::object::Enviroment;

pub fn run() {
    let prompt = ">>";
    let mut scan = String::new();
    let mut env = Enviroment::new();

    print!("read print eval loop is started {}", prompt);
    io::stdout().flush().unwrap();

    loop {
        io::stdin().read_line(&mut scan).expect("Failed to read line");

        let lex = lexer::Lexer::new(scan.clone());
        let mut p = parser::Parser::new(lex);
        let program = p.parse_program();

        if p.errors.len() > 0 {
            for error in p.errors {
                println!("{}", error);
            }
            continue;
        }

        let evaluated = eval(program.to_enum().to_ast(), &mut env);
        println!("{:?}", evaluated.inspect());
        scan = "".to_string();
        print!("{}", prompt);
        io::stdout().flush().unwrap();
    }
}

