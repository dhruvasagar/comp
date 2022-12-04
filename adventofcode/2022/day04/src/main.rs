use std::io;
use std::str::FromStr;
use std::time::Instant;

#[derive(Clone, Debug)]
struct Range {
    low: u32,
    high: u32,
}

impl Range {
    fn contains(&self, r: &Range) -> bool {
        (self.low <= r.low && self.high >= r.high) || (r.low <= self.low && r.high >= self.high)
    }

    fn overlap(&self, r: &Range) -> bool {
        (self.low <= r.low && self.high >= r.low) || (self.low >= r.low && self.low <= r.high)
    }
}

impl FromStr for Range {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lr: Vec<&str> = s.split("-").collect();
        Ok(Range {
            low: lr[0].parse::<u32>().unwrap(),
            high: lr[1].parse::<u32>().unwrap(),
        })
    }
}

fn read_input() -> Vec<Vec<Range>> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|l| {
            l.split(",")
                .map(|r| r.parse().unwrap())
                .collect::<Vec<Range>>()
        })
        .collect()
}

fn part1(ranges: Vec<Vec<Range>>) -> u32 {
    ranges
        .iter()
        .fold(0, |c, rs| if rs[0].contains(&rs[1]) { c + 1 } else { c })
}

fn part2(ranges: Vec<Vec<Range>>) -> u32 {
    ranges
        .iter()
        .fold(0, |c, rs| if rs[0].overlap(&rs[1]) { c + 1 } else { c })
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
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
