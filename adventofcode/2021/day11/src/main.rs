use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<Vec<i32>> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()
        .iter()
        .map(|s| s.chars().map(|c| c.to_string().parse().unwrap()).collect())
        .collect()
}

struct Point {
    x: String,
    y: String,
}

impl Point {
    fn new(x: String, y: String) -> Self {
        Point { x, y }
    }
}

const ADJACENCY: [Point; 8] = [
    Point::new(-1, -1),
    Point::new( 0, -1),
    Point::new( 1, -1),
    Point::new(-1,  0),
    Point::new( 1,  0),
    Point::new(-1,  1),
    Point::new( 0,  1),
    Point::new( 1,  1),
]

fn part1(_lines: Vec<String>) -> String {
    return "part1".into();
}

fn part2(_lines: Vec<String>) -> String {
    return "part2".into();
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let s1 = Instant::now();
    println!("{}", part1(lines.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(lines.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
