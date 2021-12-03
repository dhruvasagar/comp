use std::io;

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    let lines = stdin.lock().lines().map(|x| x.unwrap()).collect();
    lines
}

fn to_dec(bits: Vec<i32>) -> i32 {
    let size = bits.len() as u32;
    let base: i32 = 2;
    let mut res: i32 = 0;
    for i in 0..size {
        res = res + bits[i as usize] * base.pow(size - i - 1);
    }
    res
}

fn rate(bits: Vec<Vec<i32>>, major: bool) -> Vec<i32> {
    let mut gamma: Vec<i32> = vec![];
    let bsize: i32 = bits[0].len() as i32;
    for i in 0..bsize {
        let mut n0: i32 = 0;
        let mut n1: i32 = 0;
        for bbit in bits.clone() {
            if bbit[i as usize] == 0 {
                n0 = n0 + 1;
            } else {
                n1 = n1 + 1
            }
        }
        if n0 > n1 {
            gamma.push(if major { 0 } else { 1 });
        } else {
            gamma.push(if major { 1 } else { 0 });
        }
    }
    gamma
}

fn part1(lines: Vec<String>) -> String {
    let mut bits: Vec<Vec<i32>> = vec![];
    for line in lines {
        let bbits: Vec<i32> = line
            .chars()
            .map(|c| c.to_string().parse().unwrap())
            .collect();
        bits.push(bbits);
    }
    let gamma = rate(bits.clone(), true);
    let epsilon = rate(bits.clone(), false);
    format!("{}", to_dec(gamma) * to_dec(epsilon))
}

fn filter(bits: Vec<Vec<i32>>, major: bool) -> Vec<Vec<i32>> {
    let mut gamma_bits = bits.clone();
    let bsize: i32 = bits[0].len() as i32;
    for i in 0..bsize {
        let mut n0: i32 = 0;
        let mut n1: i32 = 0;
        let mut idx0: Vec<i32> = vec![];
        let mut idx1: Vec<i32> = vec![];
        for j in 0..gamma_bits.len() {
            let bbit = gamma_bits[j as usize].clone();
            if bbit[i as usize] == 0 {
                idx0.push(j as i32);
                n0 = n0 + 1;
            } else {
                idx1.push(j as i32);
                n1 = n1 + 1
            }
        }
        let mut tmp_bits = vec![];
        let idxx = if n0 > n1 {
            if major {
                idx0
            } else {
                idx1
            }
        } else {
            if major {
                idx1
            } else {
                idx0
            }
        };
        for idx in idxx.clone() {
            tmp_bits.push(gamma_bits[idx as usize].clone());
        }
        gamma_bits = tmp_bits;
        if gamma_bits.len() == 1 {
            break;
        }
    }
    gamma_bits
}

fn part2(lines: Vec<String>) -> String {
    let mut bits: Vec<Vec<i32>> = vec![];
    for line in lines {
        let bbits: Vec<i32> = line
            .chars()
            .map(|c| c.to_string().parse().unwrap())
            .collect();
        bits.push(bbits);
    }
    let mut gamma_bits = bits.clone();
    let mut epsilon_bits = bits.clone();
    loop {
        if gamma_bits.len() > 1 {
            gamma_bits = filter(gamma_bits.clone(), true);
        }
        if epsilon_bits.len() > 1 {
            epsilon_bits = filter(epsilon_bits.clone(), false);
        }
        if gamma_bits.len() == 1 && epsilon_bits.len() == 1 {
            break;
        }
    }
    format!(
        "{}",
        to_dec(gamma_bits[0].clone()) * to_dec(epsilon_bits[0].clone())
    )
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
