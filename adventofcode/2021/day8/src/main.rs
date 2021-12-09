use std::collections::HashMap;
use std::io;
use std::iter::Iterator;
use std::str::FromStr;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
struct Segment {
    label: char,
    on: bool,
}

impl FromStr for Segment {
    type Err = ();
    fn from_str(s: &str) -> Result<Segment, ()> {
        Ok(Segment {
            label: s.chars().nth(0).unwrap(),
            on: true,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
struct Digit {
    label: String,
    segments: Vec<Segment>,
}

impl FromStr for Digit {
    type Err = ();
    fn from_str(s: &str) -> Result<Digit, ()> {
        let mut segments: Vec<Segment> = vec![];
        let labels: [char; 7] = ['a', 'b', 'c', 'd', 'e', 'f', 'g'];
        for i in 0..7 {
            let l = labels[i];
            segments.push(Segment {
                label: labels[i],
                on: s.contains(l),
            });
        }
        let mut label: Vec<char> = s.to_string().chars().collect();
        label.sort();
        Ok(Digit {
            segments,
            label: label.iter().collect(),
        })
    }
}

impl Digit {
    fn sig_count(&self) -> i32 {
        self.segments
            .iter()
            .fold(0, |acc, s| if s.on { acc + 1 } else { acc })
    }

    // fn on_sigs(&self) -> Vec<Segment> {
    //     self.segments
    //         .iter()
    //         .filter_map(|&s| if s.on { Some(s) } else { None })
    //         .collect()
    // }

    fn contains(&self, d: &Digit) -> bool {
        d.segments.iter().all(|&s| self.label.contains(s.label))
    }

    fn ocount(&self, d: Digit) -> i32 {
        d.segments.iter().fold(0, |acc, &s| {
            if self.label.contains(s.label) {
                acc + 1
            } else {
                acc
            }
        })
    }
}

fn part1(digs: Vec<(Vec<Digit>, Vec<Digit>)>) -> i32 {
    let usizes = vec![2, 4, 3, 7];
    digs.iter().fold(0, |acc, dig| {
        acc + dig.1.iter().fold(0, |acc, dd| {
            if usizes.contains(&dd.sig_count()) {
                acc + 1
            } else {
                acc
            }
        })
    })
}

// 0 -> "abcef", 6
// 1 -> "cf", 2      [unique]
// 2 -> "acdef", 5
// 3 -> "acdfg", 5
// 4 -> "bcdf", 4    [unique]
// 5 -> "abdfg", 5
// 6 -> "abdefg", 6
// 7 -> "acf", 3     [unique]
// 8 -> "abcdefg", 7 [unique]
// 9 -> "abcdfg", 6
fn part2(adigs: Vec<(Vec<Digit>, Vec<Digit>)>) -> i32 {
    let mut res = 0;
    for sig_dig in adigs {
        let (sigs, digs) = sig_dig;
        let mut smap: HashMap<i32, Vec<Digit>> = HashMap::new();
        for s in sigs {
            if let Some(d) = smap.get_mut(&s.sig_count()) {
                d.push(s);
            } else {
                smap.insert(s.sig_count(), vec![s]);
            }
        }

        let mut nmap: HashMap<String, i32> = HashMap::new();
        let one = &smap.get(&2).unwrap()[0].clone();
        nmap.insert(one.label.clone(), 1);
        let seven = smap.get(&3).unwrap()[0].clone();
        nmap.insert(seven.label, 7);
        let four = smap.get(&4).unwrap()[0].clone();
        nmap.insert(four.label.clone(), 4);
        let eight = smap.get(&7).unwrap()[0].clone();
        nmap.insert(eight.label, 8);

        let three = smap
            .get(&5)
            .unwrap()
            .iter()
            .find(|&s| s.contains(one))
            .unwrap();
        nmap.insert(three.label.clone(), 3);
        smap.get_mut(&5).unwrap().iter().filter_map(|&d| {
            if d.label == three.label {
                None
            } else {
                Some(d)
            }
        });

        let two = smap
            .get(&5)
            .unwrap()
            .iter()
            .find(|&&s| s.ocount(four.clone()) == 2)
            .unwrap();
        nmap.insert(two.label, 2);

        println!("{:?}", smap);
    }
    res
}

fn parse_input(lines: Vec<String>) -> Vec<(Vec<Digit>, Vec<Digit>)> {
    let mut adigs: Vec<(Vec<Digit>, Vec<Digit>)> = vec![];
    for line in lines {
        let sigs_n_digs: Vec<&str> = line.split(" | ").collect();
        let sigs: Vec<Digit> = sigs_n_digs[0]
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();
        let digs: Vec<Digit> = sigs_n_digs[1]
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();
        adigs.push((sigs, digs));
    }
    adigs
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let digs = parse_input(lines.clone());
    let s1 = Instant::now();
    println!("{}", part1(digs));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    let digs = parse_input(lines.clone());
    println!("{}", part2(digs));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
