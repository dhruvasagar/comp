use std::io;
use std::str::FromStr;
use std::time::Instant;

#[derive(Clone, Debug)]
pub enum Move {
    Rock,    // A, X
    Paper,   // B, Y
    Scissor, // C, Z
}

impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(Move::Rock),
            "B" | "Y" => Ok(Move::Paper),
            "C" | "Z" => Ok(Move::Scissor),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Outcome {
    Lose, // X
    Draw, // Y
    Win,  // Z
}

impl FromStr for Outcome {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Outcome::Lose),
            "Y" => Ok(Outcome::Draw),
            "Z" => Ok(Outcome::Win),
            _ => Err(()),
        }
    }
}

fn move_score(mv: &Move) -> i32 {
    match mv {
        Move::Rock => 1,
        Move::Paper => 2,
        Move::Scissor => 3,
    }
}

fn outcome_score(outcome: Outcome) -> i32 {
    match outcome {
        Outcome::Lose => 0,
        Outcome::Draw => 3,
        Outcome::Win => 6,
    }
}

fn outcome(mv1: &Move, mv2: &Move) -> Outcome {
    match mv1 {
        Move::Rock => match mv2 {
            Move::Rock => Outcome::Draw,
            Move::Scissor => Outcome::Lose,
            Move::Paper => Outcome::Win,
        },
        Move::Paper => match mv2 {
            Move::Paper => Outcome::Draw,
            Move::Rock => Outcome::Lose,
            Move::Scissor => Outcome::Win,
        },
        Move::Scissor => match mv2 {
            Move::Scissor => Outcome::Draw,
            Move::Paper => Outcome::Lose,
            Move::Rock => Outcome::Win,
        },
    }
}

fn get_move(mv: &Move, outcome: &Outcome) -> Move {
    match mv {
        Move::Rock => match outcome {
            Outcome::Draw => Move::Rock,
            Outcome::Win => Move::Paper,
            Outcome::Lose => Move::Scissor,
        },
        Move::Paper => match outcome {
            Outcome::Draw => Move::Paper,
            Outcome::Win => Move::Scissor,
            Outcome::Lose => Move::Rock,
        },
        Move::Scissor => match outcome {
            Outcome::Draw => Move::Scissor,
            Outcome::Win => Move::Rock,
            Outcome::Lose => Move::Paper,
        },
    }
}

fn round_score(mv1: &Move, mv2: &Move) -> i32 {
    let outcome = outcome(mv1, mv2);
    move_score(mv2) + outcome_score(outcome)
}

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn part1(lines: Vec<String>) -> i32 {
    lines
        .iter()
        .map(|l| l.split(" ").map(|m| m.parse().unwrap()).collect())
        .fold(0, |score, round: Vec<Move>| {
            score + round_score(round.first().unwrap(), round.last().unwrap())
        })
}

fn part2(lines: Vec<String>) -> i32 {
    let mut score: i32 = 0;
    for line in lines {
        let mo: Vec<&str> = line.split(" ").collect();
        let mv1: Move = mo.first().unwrap().parse().unwrap();
        let out: Outcome = mo.last().unwrap().parse().unwrap();
        score = score + round_score(&mv1, &get_move(&mv1, &out));
    }
    score
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let s1 = Instant::now();
    println!("{}", part1(lines.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(lines.clone()));
    let e = s.elapsed();
    let e2 = s2.elapsed();
    println!(
        "Time for part1: {:?}, for part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
