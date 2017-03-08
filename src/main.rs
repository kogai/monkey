mod token;
mod lexer;

fn main() {
    let mut l = lexer::new("
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
        x + y;
        };

        let result = add(five, ten);
    ");
    println!("{}", l.next_token().literal);
}
