use std::collections::{HashMap, VecDeque};
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
        .map(|x| {
            x.chars()
                .collect::<Vec<char>>()
                .iter()
                .map(|c| c.to_string().parse().unwrap())
                .collect()
        })
        .collect()
}

const DIRECTIONS: [(i32, i32); 4] = [(0, -1), (-1, 0), (1, 0), (0, 1)];

fn inside(grid: Vec<Vec<i32>>, point: (i32, i32)) -> bool {
    let (x, y) = point;
    let xmax = grid.len() as i32;
    let ymax = grid[0].len() as i32;
    x >= 0 && x < xmax && y >= 0 && y < ymax
}

fn neighbours(grid: Vec<Vec<i32>>, point: (i32, i32)) -> Vec<(i32, i32)> {
    let (x, y) = point;
    let mut ns: Vec<(i32, i32)> = vec![];
    for (dx, dy) in DIRECTIONS.iter() {
        let np = (x + dx, y + dy);
        ns.push(np);
    }
    ns.iter()
        .filter_map(|&pp| {
            if inside(grid.clone(), pp) {
                Some(pp)
            } else {
                None
            }
        })
        .collect()
}

fn low_points(grid: Vec<Vec<i32>>) -> Vec<(i32, i32)> {
    let mut lps: Vec<(i32, i32)> = vec![];
    for i in 0..grid.len() {
        let row = grid[i].clone();
        for j in 0..row.len() {
            let p = (i as i32, j as i32);
            let ns: Vec<(i32, i32)> = neighbours(grid.clone(), p);
            let low = ns
                .iter()
                .all(|&(nx, ny)| grid[i][j] < grid[nx as usize][ny as usize]);
            if low {
                lps.push(p)
            }
        }
    }
    lps
}

fn part1(grid: Vec<Vec<i32>>, lps: Vec<(i32, i32)>) -> i32 {
    let mut res = 0;
    for (x, y) in lps {
        res += grid[x as usize][y as usize] + 1;
    }
    res
}

fn basin_size_rec(
    grid: Vec<Vec<i32>>,
    vis: &mut HashMap<(i32, i32), bool>,
    point: (i32, i32),
) -> i32 {
    vis.insert(point, true);
    let (x, y) = point;
    let val = grid[x as usize][y as usize];
    let ns: Vec<(i32, i32)> = neighbours(grid.clone(), point)
        .iter()
        .filter_map(|&(nx, ny)| {
            if !vis.contains_key(&(nx, ny))
                && grid[nx as usize][ny as usize] > val
                && grid[nx as usize][ny as usize] != 9
            {
                vis.insert((nx, ny), true);
                Some((nx, ny))
            } else {
                None
            }
        })
        .collect();
    ns.len() as i32
        + ns.iter()
            .fold(0, |acc, &np| acc + basin_size_rec(grid.clone(), vis, np))
}

fn basin_size(grid: Vec<Vec<i32>>, vis: &mut HashMap<(i32, i32), bool>, point: (i32, i32)) -> i32 {
    let mut q: VecDeque<(i32, i32)> = VecDeque::new();
    q.push_back(point);
    let mut size = 0;
    while q.len() > 0 {
        let p = q.pop_front().unwrap();
        if vis.contains_key(&p) {
            continue;
        }
        size += 1;
        let (x, y) = p;
        vis.insert(p, true);
        let val = grid[x as usize][y as usize];
        for (nx, ny) in neighbours(grid.clone(), p) {
            let nh = grid[nx as usize][ny as usize];
            if nh > val && nh != 9 {
                q.push_back((nx, ny));
            }
        }
    }
    size
}

fn part2(grid: Vec<Vec<i32>>, lps: Vec<(i32, i32)>) -> i32 {
    let mut vls: Vec<i32> = vec![];
    let vis = &mut HashMap::new();
    for lp in lps {
        vls.push(basin_size(grid.clone(), vis, lp));
    }
    vls.sort_by(|a, b| b.cmp(a));
    vls[0] * vls[1] * vls[2]
}

fn main() {
    let s = Instant::now();
    let grid = read_input();
    let s1 = Instant::now();
    let lps = low_points(grid.clone());
    println!("{}", part1(grid.clone(), lps.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(grid.clone(), lps.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
