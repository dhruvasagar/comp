#[derive(Debug)]
struct Case {
    n: u32,
    k: u32,
}

#[derive(Debug)]
struct Input {
    cases: Vec<Case>,
}

fn parse_input(lines: Vec<&str>) -> Input {
    let t: usize = lines[0].parse().unwrap();
    let mut cases: Vec<Case> = vec![];
    for i in 1..=t {
        let nk: Vec<u32> = lines[i].split_whitespace().map(|n| n.parse().unwrap()).collect();
        let case = Case {
            n: nk[0],
            k: nk[1],
        };
        cases.push(case);
    }
    Input { cases }
}

fn solve_case(case: Case) {
    let nsa = case.n * (case.n + 1) / 2;
    if case.k < nsa {
        println!("-1");
        return;
    }

    let mut r = vec![1; (case.n - 1) as usize];
    r.push(case.k - nsa + 1);
    println!("{}", r.iter().map(|&n| format!("{}", n)).collect::<Vec<String>>().join(" "));
}

fn solve(lines: Vec<&str>) {
    let input = parse_input(lines);
    for case in input.cases {
        solve_case(case);
    }
}

fn main() {
    let lines = include_str!("./gcd_subarray_input").split("\n").collect::<Vec<&str>>().split_last().unwrap().1.to_vec();
    solve(lines);
}
