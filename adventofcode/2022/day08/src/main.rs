use std::io;
use std::str::FromStr;
use std::time::Instant;

#[derive(Copy, Clone, Debug)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone)]
struct Grid {
    cells: Vec<Vec<u32>>,
}

impl FromStr for Grid {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Grid {
            cells: s
                .trim()
                .split("\n")
                .map(|s| s.chars().map(|c| c.to_digit(10).unwrap()).collect())
                .collect(),
        })
    }
}

impl Grid {
    fn visible(&self, p: Point) -> bool {
        if p.x == self.cells.len() - 1 || p.y == self.cells[0].len() - 1 {
            return true;
        }

        let height = self.cells[p.y][p.x];
        (0..p.x).all(|xi| self.cells[p.y][xi] < height)
            || (0..p.y).all(|yi| self.cells[yi][p.x] < height)
            || ((p.x + 1)..self.cells.len()).all(|xi| self.cells[p.y][xi] < height)
            || ((p.y + 1)..self.cells[0].len()).all(|yi| self.cells[yi][p.x] < height)
    }

    fn scenic_score(&self, p: Point) -> u32 {
        let height = self.cells[p.y][p.x];
        let size = self.cells.len();
        let mut l: u32 = 0;
        for xi in (0..p.x).rev() {
            l += 1;
            if self.cells[p.y][xi] >= height {
                break;
            }
        }

        let mut t: u32 = 0;
        for yi in (0..p.y).rev() {
            t += 1;
            if self.cells[yi][p.x] >= height {
                break;
            }
        }

        let mut r: u32 = 0;
        for xi in (p.x + 1)..size {
            r += 1;
            if self.cells[p.y][xi] >= height {
                break;
            }
        }

        let mut b: u32 = 0;
        for yi in (p.y + 1)..size {
            b += 1;
            if self.cells[yi][p.x] >= height {
                break;
            }
        }

        l * t * r * b
    }

    pub fn visible_count(&self) -> u32 {
        let mut cnt: u32 = 0;
        let size = self.cells.len();
        for x in 0..size {
            for y in 0..size {
                let p = Point { x, y };
                if self.visible(p) {
                    cnt += 1;
                }
            }
        }
        cnt
    }

    pub fn best_scenic_score(&self) -> u32 {
        let size = self.cells.len();
        (0..size)
            .map(|x| {
                (0..size)
                    .map(|y| self.scenic_score(Point { x, y }))
                    .max()
                    .unwrap()
            })
            .max()
            .unwrap()
    }
}

fn read_input() -> String {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let mut input: String = String::new();
    stdin.lock().read_to_string(&mut input).unwrap();
    input
}

fn part1(grid: &Grid) -> u32 {
    grid.visible_count()
}

fn part2(grid: &Grid) -> u32 {
    grid.best_scenic_score()
}

fn main() {
    let s = Instant::now();
    let input = read_input();
    let grid: Grid = input.parse().unwrap();
    let s1 = Instant::now();
    println!("{}", part1(&grid));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(&grid));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
