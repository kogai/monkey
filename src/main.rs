mod token;
mod lexer;
mod ast;
mod parser;
mod buildin;
mod object;
mod evaluator;
mod repl;

fn main() {
    repl::run();
}
