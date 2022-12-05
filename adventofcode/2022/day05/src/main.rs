use regex::Regex;
use std::io;
use std::time::Instant;

#[derive(Clone, Debug, Default)]
struct Stack {
    crates: String,
}

impl Stack {
    fn add(&mut self, cr: String) {
        self.crates = format!("{}{}", self.crates, cr);
    }

    fn ins(&mut self, c: char) {
        self.crates = format!("{}{}", c, self.crates);
    }

    fn remove(&mut self, n: usize) -> String {
        let removed = self.crates[(self.crates.len() - n)..].to_string();
        self.crates = self.crates[0..(self.crates.len() - n)].to_string();
        removed
    }
}

#[derive(Clone, Debug, Default, Copy)]
struct Move {
    count: u8,
    from: usize,
    to: usize,
}

impl Move {
    pub fn apply(self, stacks: &mut Vec<Stack>) {
        let removed: String;
        {
            let fstack = stacks.get_mut(self.from).unwrap();
            removed = fstack.remove(self.count as usize);
        }
        let tstack = stacks.get_mut(self.to).unwrap();
        tstack.add(removed.chars().rev().collect::<String>());
    }

    pub fn apply2(&self, stacks: &mut Vec<Stack>) {
        let removed: String;
        {
            let fstack = stacks.get_mut(self.from).unwrap();
            removed = fstack.remove(self.count as usize);
        }
        let tstack = stacks.get_mut(self.to).unwrap();
        tstack.add(removed);
    }
}

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn parse_stacks(lines: Vec<String>) -> (Vec<Stack>, Vec<Move>) {
    let mut sindex = 0;
    for (index, line) in lines.iter().enumerate() {
        if line.trim().starts_with("1") {
            sindex = index;
            break;
        }
    }
    let scount = lines[sindex]
        .split_whitespace()
        .collect::<Vec<&str>>()
        .len();

    let mut stacks = vec![];
    for i in 0..scount {
        let mut stack = Stack::default();
        for d in lines[0..sindex].iter() {
            let n = d.chars().nth(i * 4 + 1).unwrap();

            if n != ' ' {
                stack.ins(n);
            }
        }
        stacks.push(stack);
    }

    let mut moves = vec![];
    let move_regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    for line in lines[(sindex + 2)..].iter() {
        let caps = move_regex.captures(line).unwrap();
        moves.push(Move {
            count: caps[1].parse().unwrap(),
            from: caps[2].parse::<usize>().unwrap() - 1,
            to: caps[3].parse::<usize>().unwrap() - 1,
        });
    }

    (stacks, moves)
}

fn part1(stacks: &mut Vec<Stack>, moves: Vec<Move>) -> String {
    for mv in moves.iter() {
        mv.apply(stacks);
    }
    stacks
        .iter()
        .map(|s| format!("{}", s.crates.chars().last().unwrap()))
        .collect::<String>()
}

fn part2(stacks: &mut Vec<Stack>, moves: Vec<Move>) -> String {
    for mv in moves.iter() {
        mv.apply2(stacks);
    }
    stacks
        .iter()
        .map(|s| format!("{}", s.crates.chars().last().unwrap()))
        .collect::<String>()
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let (stacks, moves) = parse_stacks(lines);
    let s1 = Instant::now();
    println!("{}", part1(&mut stacks.clone(), moves.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(&mut stacks.clone(), moves));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
