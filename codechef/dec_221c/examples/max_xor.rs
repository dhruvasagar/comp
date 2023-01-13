#[derive(Debug)]
struct Case {
    a: Vec<char>,
    b: Vec<char>,
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
            a: lines[i].chars().collect(),
            b: lines[i+1].chars().collect(),
        };
        cases.push(case);
    }
    Input { cases }
}

pub fn solve(lines: Vec<&str>) {
    let input = parse_input(lines);
    for case in input.cases {
        let sz = case.a.clone().len();
        let mut zc = 0;
        let mut oc = 0;
        for i in 0..sz {
            if case.a[i] == '1' {
                oc += 1;
            } else {
                zc += 1;
            }
            if case.b[i]== '1' {
                oc += 1;
            } else {
                zc += 1;
            }
        }
        if zc == oc {
            println!("{}", "1".repeat(sz));
        } else if oc > zc {
            println!("{}{}", "1".repeat(zc), "0".repeat(sz - zc));
        } else {
            println!("{}{}", "1".repeat(oc), "0".repeat(sz - oc));
        }
    }
}

fn main() {
    let lines = include_str!("./max_xor_input").split("\n").collect::<Vec<&str>>().split_last().unwrap().1.to_vec();
    solve(lines);
}
