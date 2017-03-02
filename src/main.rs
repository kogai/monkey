extern crate monkey;

fn main() {
    let ss = "abcdeあいう覇権アニメ🍣";
    for i in 0..ss.len() {
        print!("{}-", ss.chars().nth(i).unwrap());
    }
}