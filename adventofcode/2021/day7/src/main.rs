use std::io;
use std::time::Instant;

fn read_input() -> Vec<i32> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()[0]
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn median(nums: Vec<i32>) -> i32 {
    let size = nums.len();
    if size % 2 != 0 {
        nums[size / 2]
    } else {
        (nums[(size - 1) / 2] + nums[size / 2]) / 2
    }
}

fn part1(nums: Vec<i32>) -> String {
    let med = median(nums.clone());
    let md = nums.iter().fold(0, |acc, num| acc + (num - med).abs());
    format!("{}", md)
}

fn sigma(n: i32) -> i32 {
    (n + 1) * n / 2
}

fn part2(nums: Vec<i32>) -> String {
    let mut md = 0;
    let size = nums.len();
    let (min, max) = (nums[0], nums[size - 1]);
    for i in min..max {
        let d = nums.iter().fold(0, |acc, num| acc + sigma((num - i).abs()));
        if i == 0 || md > d {
            md = d;
        }
    }
    format!("{}", md)
}

fn main() {
    let s = Instant::now();
    let mut nums = read_input();
    nums.sort();
    let s1 = Instant::now();
    println!("{}", part1(nums.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(nums.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
