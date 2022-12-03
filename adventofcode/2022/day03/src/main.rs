use std::io;
use std::time::Instant;

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn find_common_item(items: &String) -> String {
    let mut cs = String::new();
    let mid = items.len() / 2;
    let (left, right) = items.split_at(mid);
    for c in left.chars() {
        if right.contains(c) && !cs.contains(c) {
            cs.push(c);
        }
    }
    cs
}

fn priority(c: char) -> u32 {
    if c.is_lowercase() {
        return (c as u32 - 'a' as u32) + 1;
    }
    (c as u32 - 'A' as u32) + 27
}

fn part1(lines: Vec<String>) -> u32 {
    lines
        .iter()
        .map(find_common_item)
        .fold(0, |tot, cs| tot + cs.chars().map(priority).sum::<u32>())
}

fn find_common_item_batch(batch: &[String]) -> String {
    let first = &batch[0];
    let second = &batch[1];
    let third = &batch[2];
    first
        .chars()
        .find(|&c| second.contains(c) && third.contains(c))
        .unwrap()
        .to_string()
}

fn part2(lines: Vec<String>) -> u32 {
    const BATCH: usize = 3;
    lines
        .chunks(BATCH)
        .map(find_common_item_batch)
        .fold(0, |tot, cs| tot + cs.chars().map(priority).sum::<u32>())
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let s1 = Instant::now();
    println!("{}", part1(lines.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(lines));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Solved part1 in {:?}, part2 in {:?}, total with IO in {:?}",
        e1, e2, e
    );
}
