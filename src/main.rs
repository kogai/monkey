mod repl;
mod token;
mod lexer;
mod ast;
mod parser;
mod object;

fn main() {
    repl::run();
}
