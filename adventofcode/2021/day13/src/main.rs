use std::collections::HashSet;
use std::io;
use std::str::FromStr;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl FromStr for Point {
    type Err = ();
    fn from_str(s: &str) -> Result<Point, ()> {
        let cs: Vec<i32> = s.split(',').map(|c| c.parse().unwrap()).collect();
        Ok(Point { x: cs[0], y: cs[1] })
    }
}

impl Point {
    fn fold(&self, f: Fold) -> Self {
        let p = f.0;
        let x = if p.x == 0 {
            self.x
        } else {
            p.x - (p.x - self.x).abs()
        };
        let y = if p.y == 0 {
            self.y
        } else {
            p.y - (p.y - self.y).abs()
        };
        Point { x, y }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Fold(Point);

impl FromStr for Fold {
    type Err = ();
    fn from_str(s: &str) -> Result<Fold, ()> {
        let cs: Vec<String> = s
            .split_whitespace()
            .last()
            .unwrap()
            .split('=')
            .map(|s| s.to_string())
            .collect();
        match cs[0].as_str() {
            "x" => Ok(Fold(Point {
                x: cs[1].parse().unwrap_or(0),
                y: 0,
            })),
            "y" => Ok(Fold(Point {
                x: 0,
                y: cs[1].parse().unwrap_or(0),
            })),
            _ => Err(()),
        }
    }
}

fn part1(coords: Vec<Point>, insts: Vec<Fold>) -> String {
    let f = insts[0];
    let ps: Vec<Point> = coords.iter().map(|p| p.fold(f)).collect();
    let mut h: HashSet<Point> = HashSet::new();
    for p in ps {
        h.insert(p);
    }
    format!("{}", h.len())
}

fn display(coords: Vec<Point>) -> String {
    let mut xmax = 0;
    let mut ymax = 0;
    let mut h: HashSet<Point> = HashSet::new();
    for p in coords {
        xmax = xmax.max(p.x);
        ymax = ymax.max(p.y);
        h.insert(p);
    }
    let mut s = String::new();
    for y in 0..=ymax {
        for x in 0..=xmax {
            let p = Point { x, y };
            if h.contains(&p) {
                s.push('#');
            } else {
                s.push(' ');
            }
        }
        s.push('\n');
    }
    s
}

fn part2(coords: Vec<Point>, insts: Vec<Fold>) -> String {
    let mut ps = coords.clone();
    for inst in insts {
        ps = ps.iter().map(|p| p.fold(inst)).collect();
    }
    display(ps)
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let mut idx = 0;
    for i in 0..lines.len() {
        if lines[i] == "" {
            idx = i;
            break;
        }
    }
    let coords: Vec<Point> = lines[0..idx].iter().map(|l| l.parse().unwrap()).collect();
    let insts: Vec<Fold> = lines[(idx + 1)..lines.len()]
        .iter()
        .map(|l| l.parse().unwrap())
        .collect();

    let s1 = Instant::now();
    println!("{}", part1(coords.clone(), insts.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(coords.clone(), insts.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
