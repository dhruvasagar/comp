use std::collections::{HashMap, VecDeque};
use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Grid {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let cells: Vec<Vec<i32>> = stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .collect::<Vec<String>>()
        .iter()
        .map(|l| l.chars().map(|c| c.to_string().parse().unwrap()).collect())
        .collect();
    Grid {
        cells: cells.clone(),
        size: cells.len() as i32,
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Clone, Debug)]
struct Grid {
    cells: Vec<Vec<i32>>,
    size: i32,
}

impl Grid {
    fn inside(&self, p: Point) -> bool {
        p.x >= 0 && p.x < self.size && p.y >= 0 && p.y < self.size
    }

    fn get(&self, p: Point) -> i32 {
        self.cells[p.y as usize][p.x as usize]
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

    #[allow(dead_code)]
    fn display(&self) {
        for y in 0..self.size {
            for x in 0..self.size {
                let p = Point { x, y };
                print!("{} ", self.get(p));
            }
            println!("");
        }
    }

    fn shortest(&self, start: Point, end: Point) -> i32 {
        let mut dist: HashMap<Point, i32> = HashMap::new();
        for y in 0..self.size {
            for x in 0..self.size {
                let p = Point::new(x, y);
                dist.insert(p, i32::MAX);
            }
        }
        dist.insert(start, 0); // Starting node value is not counted

        let mut q: VecDeque<Point> = VecDeque::new();
        q.push_back(start);

        while q.len() > 0 {
            let p = q.pop_front().unwrap();
            for n in self.neighs(p) {
                let dn = *dist.get(&n).unwrap();
                let dp = *dist.get(&p).unwrap() + self.get(n);
                if dn > dp {
                    dist.insert(n, dp);
                    if !q.contains(&n) {
                        q.push_back(n);
                    }
                }
            }
        }
        *dist.get(&end).unwrap()
    }
}

fn part1(grid: Grid) -> String {
    let res = grid.shortest(Point::new(0, 0), Point::new(grid.size - 1, grid.size - 1));
    format!("{}", res)
}

fn part2(grid: Grid) -> String {
    let fsize = 5 * grid.size;
    let mut ngrid: Grid = Grid {
        cells: vec![vec![0; fsize as usize]; fsize as usize],
        size: fsize,
    };
    for y in 0..fsize {
        for x in 0..fsize {
            let tx = x / grid.size;
            let ty = y / grid.size;
            let sp = Point::new(x % grid.size, y % grid.size);
            let mut nv = grid.get(sp) + tx + ty;
            if nv > 9 {
                nv %= 9;
            }
            ngrid.cells[y as usize][x as usize] = nv;
        }
    }
    let res = ngrid.shortest(Point::new(0, 0), Point::new(fsize - 1, fsize - 1));
    format!("{}", res)
}

fn main() {
    let s = Instant::now();
    let grid = read_input();
    // println!("{:?}", grid);
    let s1 = Instant::now();
    println!("{}", part1(grid.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(grid.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
