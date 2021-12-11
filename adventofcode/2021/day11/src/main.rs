use std::collections::HashMap;
use std::fmt;
use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<Vec<i32>> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()
        .iter()
        .map(|s| s.chars().map(|c| c.to_string().parse().unwrap()).collect())
        .collect()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }

    fn neighs(&self) -> Vec<Point> {
        vec![
            Point::new(self.x - 1, self.y - 1),
            Point::new(self.x - 1, self.y),
            Point::new(self.x - 1, self.y + 1),
            Point::new(self.x, self.y - 1),
            Point::new(self.x, self.y + 1),
            Point::new(self.x + 1, self.y - 1),
            Point::new(self.x + 1, self.y),
            Point::new(self.x + 1, self.y + 1),
        ]
    }
}

#[derive(Clone, Debug)]
struct Grid {
    xmax: i32,
    ymax: i32,
    cells: Vec<Vec<i32>>,
}

impl Grid {
    fn inside(&self, p: Point) -> bool {
        p.x >= 0 && p.x < self.xmax && p.y >= 0 && p.y < self.ymax
    }

    fn flash(&mut self, memo: &mut HashMap<Point, bool>, p: Point) -> i32 {
        self.cells[p.y as usize][p.x as usize] = 0;
        memo.insert(p.clone(), true);

        let mut fc = 1;
        for np in p.neighs() {
            if memo.contains_key(&np) || !self.inside(np.clone()) {
                continue;
            }
            let o = self
                .cells
                .get_mut(np.y as usize)
                .unwrap()
                .get_mut(np.x as usize)
                .unwrap();
            if *o == 9 {
                fc += self.flash(memo, np)
            } else {
                *o += 1
            }
        }
        fc
    }

    fn step(&mut self) -> i32 {
        let mut fc = 0;
        let memo: &mut HashMap<Point, bool> = &mut HashMap::new();
        for y in 0..self.ymax {
            for x in 0..self.xmax {
                let p = Point::new(x, y);
                if memo.contains_key(&p) {
                    continue;
                }
                let o = self
                    .cells
                    .get_mut(p.y as usize)
                    .unwrap()
                    .get_mut(p.x as usize)
                    .unwrap();
                if *o == 9 {
                    fc += self.flash(memo, p)
                } else {
                    *o += 1
                }
            }
        }
        fc
    }

    fn all_flashed(&self) -> bool {
        self.cells
            .iter()
            .fold(0, |acc, row| acc + row.iter().sum::<i32>())
            == 0
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for row in self.cells.clone() {
            s = format!(
                "{}\n{}",
                s,
                row.iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<String>>()
                    .join("")
            );
        }
        write!(f, "{}", s)
    }
}

fn part1(grid: &mut Grid) -> i32 {
    let mut fc = 0;
    for _ in 0..100 {
        fc += grid.step();
    }
    fc
}

fn part2(grid: &mut Grid) -> i32 {
    let mut i = 101;
    loop {
        grid.step();
        if grid.all_flashed() {
            return i;
        }
        i += 1;
    }
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let grid = &mut Grid {
        cells: lines.clone(),
        ymax: lines.len() as i32,
        xmax: lines[0].len() as i32,
    };
    let s1 = Instant::now();
    println!("{}", part1(grid));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(grid));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
