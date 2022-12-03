use std::io;
use std::time::Instant;

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn find_common_item(items: String) -> String {
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

    return (c as u32 - 'A' as u32) + 27;
}

fn part1(lines: Vec<String>) -> u32 {
    let mut tot: u32 = 0;
    for line in lines {
        let cs = find_common_item(line);
        let ct = cs.chars().map(|c| priority(c)).sum::<u32>();
        tot = tot + ct;
    }
    tot
}

fn find_common_item_batch(batch: Vec<String>) -> String {
    let mut cs = String::new();
    let first = &batch[0];
    let second = &batch[1];
    let third = &batch[2];
    for c in first.chars() {
        if second.contains(c) && third.contains(c) && !cs.contains(c) {
            cs.push(c);
        }
    }
    cs
}

fn part2(lines: Vec<String>) -> u32 {
    const BATCH: usize = 3;
    let mut tot: u32 = 0;
    for batch in lines.chunks(BATCH) {
        let cs = find_common_item_batch(batch.to_vec());
        let ct = cs.chars().map(|c| priority(c)).sum::<u32>();
        tot = tot + ct;
    }
    tot
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
        "Solved part1 in {:?}, part2 in {:?}, total with IO in {:?}",
        e1, e2, e
    );
}
