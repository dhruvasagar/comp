use std::collections::HashMap;
use std::io;
use std::time::Instant;

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Point {
    x: i16,
    y: i16,
}

impl Point {
    fn down(&self) -> Point {
        Point {
            x: self.x,
            y: self.y + 1,
        }
    }
    fn left(&self) -> Point {
        Point {
            x: self.x - 1,
            y: self.y,
        }
    }
    fn right(&self) -> Point {
        Point {
            x: self.x + 1,
            y: self.y,
        }
    }
}

fn parse_paths(lines: Vec<String>) -> HashMap<Point, char> {
    let mut paths: HashMap<Point, char> = HashMap::new();
    for line in lines {
        let coords: Vec<&str> = line.split(" -> ").collect();
        let mut points: Vec<Point> = vec![];
        for coord in coords {
            let xy: Vec<i16> = coord.split(",").map(|p| p.parse().unwrap()).collect();
            let p = Point { x: xy[0], y: xy[1] };
            points.push(p);
        }

        for i in 0..(points.len() - 1) {
            let p1 = points.get(i).unwrap();
            let p2 = points.get(i + 1).unwrap();
            if p1.x == p2.x {
                if p1.y < p2.y {
                    for yi in p1.y..(p2.y + 1) {
                        let p = Point { x: p1.x, y: yi };
                        paths.insert(p, '#');
                    }
                } else {
                    for yi in p2.y..(p1.y + 1) {
                        let p = Point { x: p1.x, y: yi };
                        paths.insert(p, '#');
                    }
                }
            } else {
                if p1.x < p2.x {
                    for xi in p1.x..(p2.x + 1) {
                        let p = Point { x: xi, y: p1.y };
                        paths.insert(p, '#');
                    }
                } else {
                    for xi in p2.x..(p1.x + 1) {
                        let p = Point { x: xi, y: p1.y };
                        paths.insert(p, '#');
                    }
                }
            }
        }
    }
    paths
}

fn falling_forever(ymax: i16, p: &Point) -> bool {
    p.y > ymax
}

fn is_not_wall(paths: &HashMap<Point, char>, p: &Point) -> bool {
    !paths.contains_key(p)
}

fn count_resting(paths: &HashMap<Point, char>) -> u32 {
    let mut cnt: u32 = 0;
    for (_, &c) in paths {
        if c == 'o' {
            cnt += 1;
        }
    }
    cnt
}

fn simulate_sand(paths: &mut HashMap<Point, char>, ymax: i16) {
    let mut ymin = 0;
    let mut p = Point { x: 500, y: 0 };
    loop {
        if falling_forever(ymax, &p) {
            return;
        }

        let np = p.down();
        if is_not_wall(paths, &np) {
            p = np;
        } else {
            if is_not_wall(paths, &np.left()) {
                p = np.left();
            } else if is_not_wall(paths, &np.right()) {
                p = np.right();
            } else {
                paths.insert(p.clone(), 'o');
                if ymin > p.y {
                    ymin = p.y;
                }
                p = Point {
                    x: 500,
                    y: ymin - 1,
                };
            }
        }
    }
}

fn part1(paths: &mut HashMap<Point, char>) -> u32 {
    let mut ymax: i16 = 0;
    for (p, _) in paths.iter() {
        if ymax < p.y {
            ymax = p.y
        }
    }
    simulate_sand(paths, ymax);
    count_resting(paths)
}

fn is_not_wall_or_floor(paths: &HashMap<Point, char>, fy: i16, p: &Point) -> bool {
    if p.y == fy {
        return false;
    }

    is_not_wall(paths, p)
}

fn simulate_sand_with_floor(paths: &mut HashMap<Point, char>, fy: i16) {
    let mut p = Point { x: 500, y: 0 };
    let top = Point { x: 500, y: 0 };
    let mut ymin = 0;
    loop {
        if paths.contains_key(&top) && 'o' == *paths.get(&top).unwrap() {
            return;
        }

        let np = p.down();
        if is_not_wall_or_floor(paths, fy, &np) {
            p = np;
        } else {
            if is_not_wall_or_floor(paths, fy, &np.left()) {
                p = np.left();
            } else if is_not_wall_or_floor(paths, fy, &np.right()) {
                p = np.right();
            } else {
                paths.insert(p.clone(), 'o');
                if ymin > p.y {
                    ymin = p.y;
                }
                p = Point {
                    x: 500,
                    y: ymin - 1,
                };
            }
        }
    }
}

fn part2(paths: &mut HashMap<Point, char>) -> u32 {
    let mut ymax: i16 = 0;
    for (p, _) in paths.iter() {
        if ymax < p.y {
            ymax = p.y
        }
    }
    simulate_sand_with_floor(paths, ymax + 2);
    count_resting(paths)
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let paths = parse_paths(lines.clone());
    let s1 = Instant::now();
    println!("{}", part1(&mut paths.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(&mut paths.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
