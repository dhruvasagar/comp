use std::collections::HashMap;
use std::io;
use std::num::ParseIntError;
use std::str::FromStr;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl FromStr for Point {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<i32> = s.split(',').map(|x| x.parse().unwrap()).collect();
        Ok(Point {
            x: coords[0],
            y: coords[1],
        })
    }
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Clone, Debug, Default)]
struct Segment {
    start: Point,
    end: Point,
    points: Vec<Point>,
}

impl FromStr for Segment {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let points: Vec<Point> = s.split(" -> ").map(|x| x.parse().unwrap()).collect();
        Ok(Segment::new(points[0].clone(), points[1].clone()))
    }
}

impl Segment {
    fn new(start: Point, end: Point) -> Self {
        let s = start.clone();
        let e = end.clone();
        let points: Vec<Point> = vec![];
        let mut seg = Self { start, end, points };
        let len = if seg.is_v() {
            (s.y - e.y).abs()
        } else {
            (s.x - e.x).abs()
        };
        for i in 0..=len {
            let x = if s.x == e.x {
                s.x
            } else if s.x < e.x {
                s.x + i
            } else {
                s.x - i
            };
            let y = if s.y == e.y {
                s.y
            } else if s.y < e.y {
                s.y + i
            } else {
                s.y - i
            };
            let p = Point::new(x, y);
            seg.points.push(p);
        }
        seg
    }
    fn is_h(&self) -> bool {
        self.start.y == self.end.y
    }
    fn is_v(&self) -> bool {
        self.start.x == self.end.x
    }
    fn is_hv(&self) -> bool {
        self.is_h() || self.is_v()
    }
    fn is_diag(&self) -> bool {
        let slope = (self.start.y - self.end.y) / (self.start.x - self.end.x);
        slope.abs() == 1
    }
}

fn parse_input(lines: Vec<String>, diag: bool) -> Vec<Segment> {
    let mut res: Vec<Segment> = vec![];
    for line in lines {
        let seg: Segment = line.parse().unwrap();
        if seg.is_hv() || (diag && seg.is_diag()) {
            res.push(seg);
        }
    }
    res
}

fn count_intersections(segs: Vec<Segment>) -> i32 {
    let mut mem: HashMap<Point, i32> = HashMap::new();
    for seg in segs {
        for p in seg.points {
            match mem.get_mut(&p) {
                Some(n) => *n += 1,
                None => {
                    mem.insert(p, 1);
                }
            };
        }
    }
    mem.values()
        .filter(|&n| *n > 1)
        .map(|n| *n)
        .collect::<Vec<i32>>()
        .len() as i32
}

fn part1(lines: Vec<String>) -> String {
    let segs = parse_input(lines, false);
    format!("{}", count_intersections(segs))
}

fn part2(lines: Vec<String>) -> String {
    let segs = parse_input(lines, true);
    format!("{}", count_intersections(segs))
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
