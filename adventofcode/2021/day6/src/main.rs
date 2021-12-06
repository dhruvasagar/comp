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

#[derive(Clone, Debug)]
struct LanternFish {
    timer: i32,
}

fn parse_input(lines: Vec<i32>) -> Vec<LanternFish> {
    let mut fishes: Vec<LanternFish> = vec![];
    for timer in lines {
        let fish = LanternFish { timer };
        fishes.push(fish);
    }
    fishes
}

fn simulate(fishes: Vec<LanternFish>) -> (i64, i64) {
    let mut timers = vec![0; 9];
    for f in fishes {
        timers[f.timer as usize] += 1;
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
        timers[8] = zero;
        timers[6] += zero; // each fish at 0 timer creates a new fish
    }
    (part1, timers.iter().sum())
}

fn main() {
    let fishes = parse_input(read_input());
    let (part1, part2) = simulate(fishes);
    println!("{}", part1);
    println!("{}", part2);
}
