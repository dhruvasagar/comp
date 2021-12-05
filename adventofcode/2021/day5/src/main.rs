use std::collections::HashMap;
use std::io;

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

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Clone, Debug, Default)]
struct Segment {
    start: Point,
    end: Point,

    ipoints: Vec<Point>,
}

impl Segment {
    fn new(start: Point, end: Point) -> Self {
        let s = start.clone();
        let e = end.clone();
        let ipoints: Vec<Point> = vec![];
        let mut seg = Self {
            start,
            end,
            ipoints,
        };
        let mut i: i32 = 0;
        let len = if seg.is_h() {
            (s.x - e.x).abs()
        } else if seg.is_v() {
            (s.y - e.y).abs()
        } else {
            (s.x - e.x).abs()
        };
        while i <= len {
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
            seg.ipoints.push(p);
            i += 1;
        }
        seg
    }
    fn is_h(&self) -> bool {
        self.start.y == self.end.y
    }
    fn is_v(&self) -> bool {
        self.start.x == self.end.x
    }
    fn is_diag(&self) -> bool {
        let slope = (self.start.y - self.end.y) / (self.start.x - self.end.x);
        slope.abs() == 1
    }
    fn is_hv(&self) -> bool {
        self.is_h() || self.is_v()
    }
}

fn parse_input(lines: Vec<String>, diag: bool) -> Vec<Segment> {
    let mut res: Vec<Segment> = vec![];
    for line in lines {
        let points: Vec<&str> = line.split(" -> ").collect();
        let starti: Vec<i32> = points[0].split(',').map(|x| x.parse().unwrap()).collect();
        let endi: Vec<i32> = points[1].split(',').map(|x| x.parse().unwrap()).collect();
        let start = Point::new(starti[0], starti[1]);
        let end = Point::new(endi[0], endi[1]);
        let seg = Segment::new(start, end);
        if seg.is_hv() || (diag && seg.is_diag()) {
            res.push(seg);
        }
    }
    res
}

fn part1(lines: Vec<String>) -> String {
    let segs = parse_input(lines, false);
    let mut mem: HashMap<Point, i32> = HashMap::new();
    for seg in segs {
        for p in seg.ipoints {
            match mem.get_mut(&p) {
                Some(n) => *n += 1,
                None => {
                    mem.insert(p, 1);
                }
            };
        }
    }
    let res = mem
        .values()
        .filter(|&n| *n > 1)
        .map(|n| *n)
        .collect::<Vec<i32>>()
        .len();
    format!("{}", res)
}

fn part2(lines: Vec<String>) -> String {
    let segs = parse_input(lines, true);
    let mut mem: HashMap<Point, i32> = HashMap::new();
    for seg in segs {
        for p in seg.ipoints {
            match mem.get_mut(&p) {
                Some(n) => *n += 1,
                None => {
                    mem.insert(p, 1);
                }
            };
        }
    }
    let res = mem
        .values()
        .filter(|&n| *n > 1)
        .map(|n| *n)
        .collect::<Vec<i32>>()
        .len();
    format!("{}", res)
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
