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

    fn score(&self) -> i32 {
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
        res
    }

    async fn play(&mut self, nums: Vec<i32>, sx: tokio::sync::mpsc::Sender<i32>) {
        let bsize = self.cells.len();
        for num in nums {
            for i in 0..bsize {
                for j in 0..bsize {
                    let cell = self.cells.get_mut(i).unwrap().get_mut(j).unwrap();
                    if cell.num == num {
                        cell.marked = true;
                    }
                    if self.has_won() {
                        sx.send(num * self.score()).await;
                        return;
                    }
                }
            }
        }
    }

    fn playnum(&mut self, num: i32) {
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

fn part1(lines: Vec<String>) -> String {
    let nums: Vec<i32> = lines[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards: Vec<Board> = vec![];
    let mut idx = 2;
    while idx < lines.len() {
        let mut board = Board::default();
        board.id = (idx as i32 - 2) / 6; // 6 due to the empty line
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

    // let (sx, mut rx) = tokio::sync::mpsc::channel::<i32>(1);
    // for mut board in boards {
    //     let nums = nums.clone();
    //     let sender = sx.clone();
    //     tokio::spawn(async move {
    //         board.play(nums.clone(), sender).await;
    //     });
    // }
    // format!("{}", rx.recv().await.unwrap())
    for num in nums {
        for board in boards.iter_mut() {
            board.playnum(num);
            if board.has_won() {
                return format!("{}", num * board.score());
            }
        }
    }
    format!("part1")
}

fn part2(lines: Vec<String>) -> String {
    let nums: Vec<i32> = lines[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards: Vec<Board> = vec![];
    let mut idx = 2;
    while idx < lines.len() {
        let mut board = Board::default();
        board.id = (idx as i32 - 2) / 6; // 6 due to the empty line
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
    let mut last_winning_num: i32 = 0;
    let mut last_winning_score: i32 = 0;
    for num in nums {
        for board in boards.iter_mut() {
            if board.has_won() {
                continue;
            }
            board.playnum(num);
            if board.has_won() {
                last_winning_num = num;
                last_winning_score = board.score();
            }
        }
    }
    return format!("{}", last_winning_num * last_winning_score)
}

#[tokio::main]
async fn main() {
    let lines = read_input();
    println!("{}", part1(lines.clone()));
    println!("{}", part2(lines.clone()));
}
