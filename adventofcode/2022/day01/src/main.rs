use std::cmp::Reverse;
use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<i32> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let mut lines: Vec<i32> = stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|s| s.parse().unwrap_or(-1))
        .collect();
    // for input set to ensure we count the last group
    lines.push(-1);
    lines
}

fn count_calaries(cgroups: Vec<i32>) -> Vec<i32> {
    let mut gsum: i32 = 0;
    let mut sall: Vec<i32> = vec![];
    for cal in cgroups {
        if cal == -1 {
            sall.push(gsum);
            gsum = 0;
        } else {
            gsum = gsum + cal;
        }
    }
    return sall;
}

fn part1(all_cals: Vec<i32>) -> i32 {
    return *all_cals.iter().max().unwrap();
}

fn part2(all_cals: Vec<i32>) -> i32 {
    let mut cals = all_cals.clone();
    cals.sort_by_key(|w| Reverse(*w));
    return cals[0..3].iter().sum();
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let all_cals = count_calaries(lines);
    let s1 = Instant::now();
    println!("{}", part1(all_cals.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(all_cals.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
