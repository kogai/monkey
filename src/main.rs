mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;
mod repl;

fn main() {
    repl::run();
}
