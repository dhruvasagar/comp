use std::io;

#[allow(dead_code)]
fn read_input() -> Vec<i32> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let lines = stdin
        .lock()
        .lines()
        .map(|x| x.unwrap().parse::<i32>().unwrap())
        .collect();
    lines
}

fn part1(lines: Vec<i32>) -> i32 {
    let mut res: i32 = 0;
    let mut fd: i32 = lines[0];
    for i in 1..lines.len() {
        let d: i32 = lines[i as usize];
        if d > fd {
            res = res + 1;
        }
        fd = d;
    }
    res
}

fn part2(lines: Vec<i32>) -> i32 {
    let window: i32 = 3;
    let num_windows = (lines.len() as i32) - window + 1;
    let mut window_sums: Vec<i32> = vec![0; num_windows as usize];
    for i in 0..num_windows {
        let mut ctr: i32 = 0;
        for j in 0..=2 {
            ctr = ctr + lines[(i + j) as usize];
        }
        window_sums[i as usize] = ctr;
    }
    part1(window_sums)
}

pub fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
