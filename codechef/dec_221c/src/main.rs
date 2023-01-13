use std::io;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn main() {
    println!("CodeChef Dec 2022");
}
