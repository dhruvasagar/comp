use std::collections::HashMap;
use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn step(poly: String, rules: HashMap<String, String>, n: i64) -> i64 {
    let mut hc: HashMap<String, i64> = HashMap::new();
    for i in 0..(poly.len() - 1) {
        let pair: String = poly[i..=(i + 1)].to_string();
        *hc.entry(pair).or_insert(0) += 1;
    }
    for _ in 0..n {
        let mut nhc: HashMap<String, i64> = HashMap::new();
        for (pair, cnt) in hc.iter() {
            let np1 = format!("{}{}", pair.chars().nth(0).unwrap(), rules[pair]);
            let np2 = format!("{}{}", rules[pair], pair.chars().nth(1).unwrap());
            *nhc.entry(np1).or_insert(0) += cnt;
            *nhc.entry(np2).or_insert(0) += cnt;
        }
        hc = nhc.clone();
    }
    let mut lc: HashMap<String, i64> = HashMap::new();
    for (pair, cnt) in hc.iter() {
        *lc.entry(pair.chars().nth(0).unwrap().to_string())
            .or_insert(0) += cnt;
    }
    *lc.entry(poly.chars().nth(poly.len() - 1).unwrap().to_string())
        .or_insert(0) += 1;

    lc.values().max().unwrap() - lc.values().min().unwrap()
}

fn parse_input(lines: Vec<String>) -> (String, HashMap<String, String>) {
    let poly = lines[0].clone();
    let rlines = lines[2..].to_vec();
    let mut map: HashMap<String, String> = HashMap::new();
    for line in rlines {
        let rule: Vec<String> = line.split(" -> ").map(|s| s.to_string()).collect();
        map.insert(rule[0].clone(), rule[1].clone());
    }
    (poly, map)
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let (poly, rules) = parse_input(lines);
    let s1 = Instant::now();
    println!("{}", step(poly.clone(), rules.clone(), 10));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", step(poly.clone(), rules.clone(), 40));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
