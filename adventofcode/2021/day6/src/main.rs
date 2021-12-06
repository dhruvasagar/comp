use std::io;
use std::time::Instant;

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
        timers.rotate_left(1);
        timers[6] += timers[8]; // the old fish timer now resets to 6
    }
    (part1, timers.iter().sum())
}

fn main() {
    let start = Instant::now();
    let fishes = read_input();
    let startsim = Instant::now();
    let (part1, part2) = simulate(fishes);
    let endsim = startsim.elapsed();
    let end = start.elapsed();

    println!("{}", part1);
    println!("{}", part2);
    println!(
        "Time for simulation: {:?}, Total time with IO: {:?}",
        endsim, end
    );
}
