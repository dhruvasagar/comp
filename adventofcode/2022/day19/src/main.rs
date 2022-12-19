use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashSet, VecDeque};
use std::io;
use std::str::FromStr;
use std::time::Instant;

lazy_static! {
    static ref BLUEPRINT_REGEX: Regex = Regex::new(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();
}

fn read_input() -> Vec<Blueprint> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin
        .lock()
        .lines()
        .map(|x| x.unwrap())
        .map(|l| l.parse().unwrap())
        .collect()
}

#[derive(Clone, Debug)]
struct Blueprint {
    id: u32,
    ore_robot_cost: u32,
    clay_robot_cost: u32,
    obsidian_robot_cost: (u32, u32),
    geode_robot_cost: (u32, u32),
}

impl FromStr for Blueprint {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let caps = BLUEPRINT_REGEX.captures(s).unwrap();
        let id: u32 = caps[1].parse().unwrap();
        let ore_robot_cost = caps[2].parse().unwrap();
        let clay_robot_cost = caps[3].parse().unwrap();
        let obsidian_robot_cost = (caps[4].parse().unwrap(), caps[5].parse().unwrap());
        let geode_robot_cost = (caps[6].parse().unwrap(), caps[7].parse().unwrap());

        Ok(Blueprint {
            id,
            ore_robot_cost,
            clay_robot_cost,
            obsidian_robot_cost,
            geode_robot_cost,
        })
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
struct State {
    ore: u32,
    ore_robots: u32,
    clay: u32,
    clay_robots: u32,
    obsidian: u32,
    obsidian_robots: u32,
    geode: u32,
    geode_robots: u32,
    time: u32,
}

impl State {
    fn next(&mut self) {
        self.ore += self.ore_robots;
        self.clay += self.clay_robots;
        self.obsidian += self.obsidian_robots;
        self.geode += self.geode_robots;
        self.time += 1
    }
}

impl Blueprint {
    fn play(&self, time: u32) -> u32 {
        let mut geodes = 0;
        let mut queue = VecDeque::new();
        let mut start = State::default();
        start.ore_robots = 1;
        queue.push_back(start);

        let mut vis: HashSet<State> = HashSet::new();

        let max_ore_cost = *[
            self.ore_robot_cost,
            self.clay_robot_cost,
            self.obsidian_robot_cost.0,
            self.geode_robot_cost.0,
        ]
        .iter()
        .max()
        .unwrap();

        while let Some(mut state) = queue.pop_front() {
            // println!("state: {:?}", state);
            geodes = geodes.max(state.geode);

            if (geodes > 0 && state.geode < geodes - 1) || vis.contains(&state) {
                continue;
            }
            vis.insert(state);

            if state.time == time {
                continue;
            }

            if state.ore >= self.geode_robot_cost.0 && state.obsidian >= self.geode_robot_cost.1 {
                let mut next_state = state;
                next_state.ore -= self.geode_robot_cost.0;
                next_state.obsidian -= self.geode_robot_cost.1;
                next_state.next();
                next_state.geode_robots += 1;
                queue.push_back(next_state);
            } else {
                if state.ore >= self.ore_robot_cost && state.ore_robots < max_ore_cost {
                    let mut next_state = state;
                    next_state.ore -= self.ore_robot_cost;
                    next_state.next();
                    next_state.ore_robots += 1;
                    queue.push_back(next_state);
                }
                if state.ore >= self.clay_robot_cost {
                    let mut next_state = state;
                    next_state.ore -= self.clay_robot_cost;
                    next_state.next();
                    next_state.clay_robots += 1;
                    queue.push_back(next_state);
                }
                if state.ore >= self.obsidian_robot_cost.0
                    && state.clay >= self.obsidian_robot_cost.1
                {
                    let mut next_state = state;
                    next_state.ore -= self.obsidian_robot_cost.0;
                    next_state.clay -= self.obsidian_robot_cost.1;
                    next_state.next();
                    next_state.obsidian_robots += 1;
                    queue.push_back(next_state);
                }
                state.next();
                queue.push_back(state);
            }
        }
        geodes
    }

    fn quality_level(&self) -> u32 {
        self.id * self.play(24)
    }
}

fn part1(bps: Vec<Blueprint>) -> u32 {
    let mut tot: u32 = 0;
    for bp in bps {
        tot += bp.quality_level();
    }
    tot
}

fn part2(bps: Vec<Blueprint>) -> u32 {
    let mut tot: u32 = 1;
    for bp in bps[0..3].iter() {
        tot *= bp.play(32);
    }
    tot
}

fn main() {
    let s = Instant::now();
    let bps = read_input();
    let s1 = Instant::now();
    println!("{}", part1(bps.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(bps.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
