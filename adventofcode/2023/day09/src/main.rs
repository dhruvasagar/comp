use std::io;
use std::time::Instant;

fn read_input() -> Vec<Vec<i32>> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|x| x.split_whitespace().map(|x| x.parse().unwrap()).collect())
        .collect()
}

fn diffs(nums: Vec<i32>) -> Vec<i32> {
    let mut res: Vec<i32> = vec![];
    for i in 1..nums.len() {
        res.push(nums[i] - nums[i - 1]);
    }
    res
}

fn next(nums: Vec<i32>) -> i32 {
    let ndiffs = diffs(nums.clone());
    if ndiffs.iter().all(|&x| x == 0) {
        return nums[0];
    }

    nums[nums.len() - 1] + next(ndiffs)
}

fn part1(nums: Vec<Vec<i32>>) -> i32 {
    nums.iter().map(|x| next(x.clone())).sum()
}

fn part2(nums: Vec<Vec<i32>>) -> i32 {
    nums.iter()
        .map(|x| next(x.into_iter().rev().map(|&i| i).collect()))
        .sum()
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
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
