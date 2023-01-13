#[derive(Debug)]
struct Case {
    nums: Vec<u32>
}

#[derive(Debug)]
struct Input {
    cases: Vec<Case>,
}

fn parse_input(lines: Vec<&str>) -> Input {
    let t: usize = lines[0].parse().unwrap();
    let mut cases: Vec<Case> = vec![];
    for i in (1..t*2).step_by(2) {
        let case = Case {
            nums: lines[i+1].split_whitespace().map(|n| n.parse().unwrap()).collect(),
        };
        cases.push(case);
    }
    Input { cases }
}

fn gcd(a: u32, b: u32) -> u32 {
    if a == 0 {
        return b;
    }

    gcd(b % a, a)
}

fn solve_case(nums: Vec<u32>) -> Vec<u32> {
    let mut res: Vec<u32> = vec![];
    let mut gcda = *nums.first().unwrap();
    for &n in nums.iter() {
        gcda = gcda.min(gcd(gcda, n));
    }
    for &n in nums.iter() {
        res.push(n / gcda);
    }
    res
}

fn solve(lines: Vec<&str>) {
    let input = parse_input(lines);
    for case in input.cases {
        for n in solve_case(case.nums) {
            print!("{} ", n);
        }
        println!();
    }
}

fn main() {
    let lines = include_str!("./divisible_input").split("\n").collect::<Vec<&str>>().split_last().unwrap().1.to_vec();
    solve(lines);
}
