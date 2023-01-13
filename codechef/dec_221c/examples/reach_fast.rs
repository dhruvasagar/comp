#[derive(Debug)]
struct Case {
    x: u16,
    y: u16,
    k: u16,
}

#[derive(Debug)]
struct Input {
    cases: Vec<Case>,
}

fn parse_case(line: &&str) -> Case {
    let cs = line.split_whitespace().collect::<Vec<&str>>();
    Case {
        x: cs[0].parse().unwrap(),
        y: cs[1].parse().unwrap(),
        k: cs[2].parse().unwrap(),
    }
}

fn parse_input(lines: Vec<&str>) -> Input {
    Input {
        cases: lines[1..].iter().map(parse_case).collect()
    }
}

fn abs_diff(a: u16, b: u16) -> u16 {
    if a > b {
        return a - b
    }
    b - a
}

fn solve(lines: Vec<&str>) {
    let input = parse_input(lines);
    for case in input.cases {
        let sc = abs_diff(case.x, case.y);
        let mut st = sc / case.k;
        if sc % case.k != 0 {
            st += 1
        }
        println!("{}", st);
    }
}

fn main() {
    let lines = include_str!("./reach_fast_input").split("\n").collect::<Vec<&str>>().split_last().unwrap().1.to_vec();
    solve(lines);
}
