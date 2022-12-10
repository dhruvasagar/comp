use std::io;
use std::str::FromStr;
use std::time::Instant;

#[derive(Clone, Debug)]
enum Instruction {
    Noop,
    Addx(i32),
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "noop" => Ok(Instruction::Noop),
            _ => {
                let (_, val) = s.split_once(" ").unwrap();
                Ok(Instruction::Addx(val.parse::<i32>().unwrap()))
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Program {
    x: i32,
    instructions: Vec<Instruction>,
}

impl Program {
    pub fn run(&mut self) -> (Vec<i32>, Vec<i32>) {
        let mut ccnt: i32 = 1;
        let mut xs: Vec<i32> = vec![];
        let mut ss: Vec<i32> = vec![];
        for inst in self.instructions.iter() {
            match inst {
                Instruction::Noop => {
                    xs.push(self.x);
                    ss.push(self.x * ccnt);
                    ccnt += 1;
                }
                Instruction::Addx(v) => {
                    xs.push(self.x);
                    ss.push(self.x * ccnt);
                    ccnt += 1;

                    xs.push(self.x);
                    ss.push(self.x * ccnt);
                    self.x += v;
                    ccnt += 1;
                }
            };
        }
        (xs, ss)
    }
}

fn read_input() -> Vec<Instruction> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|l| l.parse().unwrap())
        .collect()
}

fn run_program(instructions: Vec<Instruction>) -> (Vec<i32>, Vec<i32>) {
    let p = &mut Program { x: 1, instructions };
    p.run()
}

fn part1(ss: Vec<i32>) -> i32 {
    let indexes: Vec<usize> = vec![19, 59, 99, 139, 179, 219];
    let mut res: i32 = 0;
    for i in indexes {
        res += ss[i];
    }
    res
}

fn part2(xs: Vec<i32>) -> String {
    let mut s = String::new();
    for i in 0..6 {
        for j in 0..40 {
            let idx = i * 40 + j;
            let x = xs[idx];
            if i32::abs(x - j as i32) < 2 {
                s += "█";
            } else {
                s += " ";
            }
        }
        s += "\n";
    }
    s
}

fn main() {
    let s = Instant::now();
    let instructions = read_input();
    let (xs, ss) = run_program(instructions);
    let s1 = Instant::now();
    println!("{}", part1(ss));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(xs));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
