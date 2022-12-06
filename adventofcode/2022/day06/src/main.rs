use itertools::Itertools;
use std::io;
use std::time::Instant;

fn read_input() -> String {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()
        .first()
        .unwrap()
        .to_string()
}

fn sub(line: String, len: usize) -> usize {
    if line[0..len].chars().unique().collect::<Vec<_>>().len() == len {
        return len;
    }

    let mut idx: usize = 1;
    while idx < line.len() {
        if line[idx..(idx + len)]
            .chars()
            .unique()
            .collect::<Vec<_>>()
            .len()
            == len
        {
            return idx + len;
        }
        idx += 1;
    }
    idx
}

fn part1(line: String) -> usize {
    sub(line, 4)
}

fn part2(line: String) -> usize {
    sub(line, 14)
}

fn main() {
    let s = Instant::now();
    let line = read_input();
    let s1 = Instant::now();
    println!("{}", part1(line.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(line.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
