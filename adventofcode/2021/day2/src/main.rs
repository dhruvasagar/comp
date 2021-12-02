use anyhow::{anyhow, Error, Result};
use std::io;
use std::str::FromStr;

#[derive(Debug, Default)]
struct Sub {
    x: i32,
    y: i32,
}

impl Sub {
    fn forward(&mut self, n: i32) {
        self.x = self.x + n;
    }
    fn down(&mut self, n: i32) {
        self.y = self.y - n;
    }
    fn up(&mut self, n: i32) {
        self.y = self.y + n;
    }
    fn pos(&self) -> i32 {
        self.x * self.y * -1
    }
}

pub enum Command {
    Forward,
    Down,
    Up,
}

impl FromStr for Command {
    type Err = Error;

    fn from_str(s: &str) -> Result<Command> {
        match s {
            "forward" => Ok(Command::Forward),
            "down" => Ok(Command::Down),
            "up" => Ok(Command::Up),
            _ => Err(anyhow!("Invalid Command")),
        }
    }
}

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let lines = stdin.lock().lines().map(|x| x.unwrap()).collect();
    lines
}

fn part1(lines: Vec<String>) -> String {
    let mut sub = Sub::default();
    for line in lines {
        let cmdargs: Vec<&str> = line.split(' ').collect();
        let command = cmdargs[0];
        let n: i32 = cmdargs[1].parse().unwrap();
        match Command::from_str(command).unwrap() {
            Command::Forward => sub.forward(n),
            Command::Down => sub.down(n),
            Command::Up => sub.up(n),
        }
    }
    format!("{}", sub.pos())
}

#[derive(Debug, Default)]
struct Sub2 {
    x: i32,
    y: i32,
    aim: i32,
}

impl Sub2 {
    fn forward(&mut self, n: i32) {
        self.x = self.x + n;
        self.y = self.y - self.aim * n;
    }
    fn down(&mut self, n: i32) {
        self.aim = self.aim + n;
    }
    fn up(&mut self, n: i32) {
        self.aim = self.aim - n;
    }
    fn pos(&self) -> i32 {
        self.x * self.y * -1
    }
}

fn part2(lines: Vec<String>) -> String {
    let mut sub2 = Sub2::default();
    for line in lines {
        let cmdargs: Vec<&str> = line.split(' ').collect();
        let command = cmdargs[0];
        let n: i32 = cmdargs[1].parse().unwrap();
        match Command::from_str(command).unwrap() {
            Command::Forward => sub2.forward(n),
            Command::Down => sub2.down(n),
            Command::Up => sub2.up(n),
        }
    }
    format!("{}", sub2.pos())
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
