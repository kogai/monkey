mod repl;
mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;

fn main() {
    repl::run();
}
