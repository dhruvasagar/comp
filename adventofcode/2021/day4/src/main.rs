use std::io;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Clone, Debug, Default)]
struct Cell {
    num: i32,
    marked: bool,
}

#[derive(Clone, Debug, Default)]
struct Board {
    id: i32,
    cells: Vec<Vec<Cell>>,
}

impl Board {
    fn has_won(&self) -> bool {
        let bsize = self.cells.len();
        let mut rowwon = true;
        let mut colwon = true;
        for i in 0..bsize {
            rowwon = true;
            colwon = true;
            for j in 0..bsize {
                let rcell = self.cells[i][j].clone();
                let ccell = self.cells[j][i].clone();
                rowwon &= rcell.marked;
                colwon &= ccell.marked;
            }
            if rowwon || colwon {
                return true;
            }
        }
        rowwon || colwon
    }

    fn score(&self, num: i32) -> i32 {
        let bsize = self.cells.len();
        let mut res: i32 = 0;
        for i in 0..bsize {
            for j in 0..bsize {
                let cell = self.cells[i][j].clone();
                if !cell.marked {
                    res += cell.num;
                }
            }
        }
        res * num
    }

    fn play(&mut self, num: i32) {
        let bsize = self.cells.len();
        for i in 0..bsize {
            for j in 0..bsize {
                let cell = self.cells.get_mut(i).unwrap().get_mut(j).unwrap();
                if cell.num == num {
                    cell.marked = true;
                    return;
                }
            }
        }
    }
}

fn parse_input(lines: Vec<String>) -> Vec<Board> {
    let mut boards: Vec<Board> = vec![];
    let mut idx = 0;
    while idx < lines.len() {
        let mut board = Board::default();
        board.id = (idx as i32) / 6; // 6 due to the empty line
        while idx < lines.len() {
            let line = lines[idx].clone();
            if line.is_empty() {
                break;
            }
            let row: Vec<i32> = line
                .split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect();
            let mut cell_row: Vec<Cell> = vec![];
            for num in row {
                let cell = Cell { num, marked: false };
                cell_row.push(cell);
            }
            board.cells.push(cell_row);
            idx += 1;
        }
        boards.push(board);
        idx += 1;
    }
    boards
}

fn part1(lines: Vec<String>) -> String {
    let nums: Vec<i32> = lines[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards = parse_input(lines[2..].to_vec());

    let mut score: i32 = 0;
    'outer: for num in nums {
        for board in boards.iter_mut() {
            board.play(num);
            if board.has_won() {
                score = board.score(num);
                break 'outer;
            }
        }
    }
    format!("{}", score)
}

fn part2(lines: Vec<String>) -> String {
    let nums: Vec<i32> = lines[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards = parse_input(lines[2..].to_vec());

    let mut score: i32 = 0;
    for num in nums {
        let mut done = true;
        for board in boards.iter_mut() {
            if board.has_won() {
                continue;
            }
            board.play(num);
            if board.has_won() {
                score = board.score(num);
            }
            done &= board.has_won();
        }
        if done {
            break;
        }
    }
    format!("{}", score)
}

fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
