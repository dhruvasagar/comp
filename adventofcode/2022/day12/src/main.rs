use std::collections::{HashSet, VecDeque};
use std::io;
use std::str::FromStr;
use std::time::Instant;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Clone, Debug)]
struct Grid {
    start: Point,
    end: Point,

    points: Vec<Vec<char>>,
    pub starts: Vec<Point>,
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut start = Point::new(0, 0);
        let mut end = Point::new(0, 0);
        let mut starts = vec![];
        for (y, l) in s.split("\n").enumerate() {
            for (x, c) in l.to_string().chars().enumerate() {
                if c == 'S' {
                    start = Point::new(x as i32, y as i32);
                    starts.push(start)
                }
                if c == 'a' {
                    starts.push(Point::new(x as i32, y as i32));
                }

                if c == 'E' {
                    end = Point::new(x as i32, y as i32);
                }
            }
        }
        let points: Vec<Vec<char>> = s
            .split("\n")
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.chars().collect())
            .collect();
        Ok(Grid {
            start,
            end,
            points,
            starts,
        })
    }
}

impl Grid {
    fn bounds(&self) -> (i32, i32) {
        (self.points[0].len() as i32, self.points.len() as i32)
    }

    fn inside(&self, p: Point) -> bool {
        let (xsize, ysize) = self.bounds();
        p.x >= 0 && p.x < xsize && p.y >= 0 && p.y < ysize
    }

    pub fn get(&self, p: Point) -> i32 {
        if p == self.start {
            return 'a' as i32;
        }

        if p == self.end {
            return 'z' as i32;
        }

        self.points[p.y as usize][p.x as usize] as i32
    }

    #[allow(dead_code)]
    pub fn char(&self, p: Point) -> char {
        self.points[p.y as usize][p.x as usize]
    }

    fn neighs(&self, p: Point) -> Vec<Point> {
        let ps: Vec<Point> = vec![
            Point::new(p.x, p.y - 1),
            Point::new(p.x - 1, p.y),
            Point::new(p.x + 1, p.y),
            Point::new(p.x, p.y + 1),
        ];
        ps.iter()
            .filter_map(|&p| if self.inside(p) { Some(p) } else { None })
            .collect()
    }

    fn dist(&self, p1: Point, p2: Point) -> i32 {
        self.get(p2) - self.get(p1)
    }

    fn shortest(&self, start: Point, end: Point) -> i32 {
        let mut q: VecDeque<(Point, i32)> = VecDeque::new();
        q.push_back((start.clone(), 0));

        let mut visited: HashSet<Point> = HashSet::new();
        visited.insert(start);

        while let Some((p, steps)) = q.pop_front() {
            if p == end {
                return steps;
            }

            for n in self.neighs(p) {
                if !visited.contains(&n) && self.dist(p, n) <= 1 {
                    visited.insert(n);
                    q.push_back((n, steps + 1));
                }
            }
        }

        i32::MAX
    }
}

fn read_input() -> String {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let mut s = String::new();
    stdin.lock().read_to_string(&mut s).unwrap();
    s
}

fn part1(grid: &Grid) -> i32 {
    grid.shortest(grid.start, grid.end)
}

fn part2(grid: &Grid) -> i32 {
    let mut dist: i32 = i32::MAX;
    for start in grid.starts.clone() {
        let d = grid.shortest(start, grid.end);
        if dist > d {
            dist = d;
        }
    }
    dist
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let grid: Grid = lines.parse().unwrap();
    let s1 = Instant::now();
    println!("{}", part1(&grid));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(&grid));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
