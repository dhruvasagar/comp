use std::cell::RefCell;
use std::collections::HashSet;
use std::io;
use std::rc::Rc;
use std::str::FromStr;
use std::time::Instant;

#[derive(Clone, Debug)]
enum Direction {
    Right,
    Up,
    Left,
    Down,
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "R" => Ok(Direction::Right),
            "U" => Ok(Direction::Up),
            "L" => Ok(Direction::Left),
            "D" => Ok(Direction::Down),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
struct Move {
    direction: Direction,
    count: u32,
}

impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mstr, countstr) = s.split_once(" ").unwrap();
        Ok(Move {
            direction: mstr.parse().unwrap(),
            count: countstr.parse().unwrap(),
        })
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn dist(&self, p1: &Point) -> u32 {
        let xdiff = self.x.abs_diff(p1.x);
        let ydiff = self.y.abs_diff(p1.y);
        if xdiff > ydiff {
            return xdiff;
        }
        ydiff
    }
}

#[derive(Clone, Debug)]
struct Rope {
    knots: Rc<Vec<RefCell<Point>>>,
}

impl Rope {
    fn len(&self) -> usize {
        self.knots.len()
    }

    pub fn apply_move(&mut self, mv: Move, hs: &mut HashSet<Point>) {
        let len = self.len();
        let knotsclone = Rc::clone(&self.knots);
        for _ in 0..mv.count {
            for idx in 0..len {
                let mut tail = knotsclone.get(idx).unwrap().borrow_mut();
                if idx == 0 {
                    match mv.direction {
                        Direction::Left => tail.x -= 1,
                        Direction::Right => tail.x += 1,
                        Direction::Up => tail.y -= 1,
                        Direction::Down => tail.y += 1,
                    };
                } else {
                    let head = knotsclone.get(idx - 1).unwrap().borrow();
                    if head.dist(&tail.clone()) > 1 {
                        if head.x > tail.x {
                            tail.x += 1;
                        } else if head.x < tail.x {
                            tail.x -= 1;
                        }
                        if head.y < tail.y {
                            tail.y -= 1;
                        } else if head.y > tail.y {
                            tail.y += 1;
                        }
                    }
                }

                if idx == len - 1 {
                    hs.insert(tail.clone());
                }
            }
        }
    }

    pub fn simulate_moves(&mut self, moves: Vec<Move>) -> usize {
        let mut set: HashSet<Point> = HashSet::new();
        set.insert(Point::default());
        for mv in moves {
            self.apply_move(mv, &mut set);
        }
        // print_set(set.clone());
        set.len()
    }
}

#[allow(dead_code)]
fn print_set(hs: HashSet<Point>) {
    let mut minx: i32 = 0;
    let mut maxx: i32 = 0;
    let mut miny: i32 = 0;
    let mut maxy: i32 = 0;
    for p in hs.iter() {
        if minx > p.x {
            minx = p.x;
        }
        if maxx < p.x {
            maxx = p.x;
        }
        if miny > p.y {
            miny = p.y;
        }
        if maxy < p.y {
            maxy = p.y;
        }
    }
    let mut s = String::new();
    for y in miny..(maxy + 1) {
        for x in minx..(maxx + 1) {
            let p = Point { x, y };
            if p == Point::default() {
                s += "s";
            } else if hs.contains(&p) {
                s += "#";
            } else {
                s += ".";
            }
        }
        s += "\n";
    }
    println!("{}", s);
}

fn read_input() -> Vec<Move> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|l| l.parse().unwrap())
        .collect()
}

fn part1(moves: Vec<Move>) -> usize {
    let rope = &mut Rope {
        knots: Rc::new(vec![
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
        ]),
    };
    rope.simulate_moves(moves)
}

fn part2(moves: Vec<Move>) -> usize {
    let rope = &mut Rope {
        knots: Rc::new(vec![
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
            RefCell::new(Point::default()),
        ]),
    };
    rope.simulate_moves(moves)
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let s1 = Instant::now();
    println!("{}", part1(lines.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(lines.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
