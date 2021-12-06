use std::io;

#[allow(dead_code)]
fn read_input() -> Vec<i32> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()[0]
        .split(',')
        .map(|t| t.parse().unwrap())
        .collect()
}

fn simulate(fishes: Vec<i32>) -> (i64, i64) {
    let mut timers = vec![0; 9];
    for f in fishes {
        timers[f as usize] += 1;
    }
    let mut part1 = 0;
    for day in 0..256 {
        if day == 80 {
            part1 = timers.iter().sum();
        }
        let zero = timers[0];
        for i in 0..8 {
            timers[i] = timers[i + 1];
        }
        timers[8] = zero; // each fish at 0 timer creates a new fish
        timers[6] += zero; // the old fish timer now resets to 6
    }
    (part1, timers.iter().sum())
}

fn main() {
    let fishes = read_input();
    let (part1, part2) = simulate(fishes);
    println!("{}", part1);
    println!("{}", part2);
}
