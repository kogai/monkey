mod lexer;
mod parser;
mod evaluator;

mod utils;
mod buildin;
mod repl;

fn main() {
    repl::run();
}
