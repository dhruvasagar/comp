use std::cell::RefCell;
use std::io;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone, Debug)]
struct File {
    #[allow(dead_code)]
    name: String,
    size: u32,
}

#[derive(Clone, Debug)]
struct Folder {
    name: String,
    parent: Option<Rc<RefCell<Folder>>>,

    files: Vec<File>,
    folders: Vec<Rc<RefCell<Folder>>>,
}

impl Folder {
    fn total_size(&self) -> u32 {
        self.files.iter().map(|f| f.size).sum::<u32>()
            + self
                .folders
                .iter()
                .map(|f| f.borrow().total_size())
                .collect::<Vec<u32>>()
                .iter()
                .sum::<u32>()
    }

    fn flatten_folders(&self) -> Vec<Rc<RefCell<Folder>>> {
        let mut folders = vec![];
        for fold in self.folders.iter() {
            folders.push(Rc::clone(fold));
            folders.append(&mut Rc::clone(fold).borrow().flatten_folders())
        }
        return folders;
    }
}

fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

fn parse_input(lines: Vec<String>) -> Rc<RefCell<Folder>> {
    let root = Rc::new(RefCell::new(Folder {
        name: String::from("/"),
        parent: None,
        files: vec![],
        folders: vec![],
    }));

    let mut f = Rc::clone(&root);
    let mut idx: usize = 1;
    while idx < lines.len() {
        let mut line = lines.get(idx).unwrap();

        if line.starts_with("$ ls") {
            idx += 1;
            while idx < lines.len() {
                line = lines.get(idx).unwrap();
                if line.starts_with("$") {
                    break;
                }

                if line.starts_with("dir") {
                    let (_, dname) = line.split_once("dir").unwrap();
                    let folder = Folder {
                        name: dname.trim().to_string(),
                        parent: Some(Rc::clone(&f)),
                        files: vec![],
                        folders: vec![],
                    };
                    f.borrow_mut().folders.push(Rc::new(RefCell::new(folder)));
                } else {
                    let lp: Vec<&str> = line.split_whitespace().collect();
                    f.borrow_mut().files.push(File {
                        name: lp[1].trim().to_string(),
                        size: lp[0].parse::<u32>().unwrap(),
                    });
                }
                idx += 1;
            }
        }

        if line.starts_with("$ cd") {
            if line == "$ cd .." {
                let fclone = Rc::clone(&f);
                f = Rc::clone(fclone.borrow().parent.as_ref().unwrap());
            } else {
                let (_, dname) = line.split_once("$ cd").unwrap();
                let fclone = Rc::clone(&f);
                for fold in fclone.borrow().folders.iter() {
                    if fold.borrow().name == dname.trim().to_string() {
                        f = Rc::clone(fold);
                        break;
                    }
                }
            }
        }

        idx += 1;
    }
    root
}

fn part1(root: Rc<RefCell<Folder>>, limit: u32) -> u32 {
    let mut res: u32 = 0;
    for fold in root.borrow().flatten_folders() {
        let ts = fold.borrow().total_size();
        if ts < limit {
            res += ts;
        }
    }
    res
}

fn part2(root: Rc<RefCell<Folder>>, total_space: u32, required_space: u32) -> u32 {
    let free_space = total_space - root.borrow().total_size();
    let limit = required_space - free_space;
    let mut folders = root.borrow_mut().flatten_folders();
    folders.sort_by(|a, b| a.borrow().total_size().cmp(&b.borrow().total_size()));
    for fold in folders {
        let ts = fold.borrow().total_size();
        if ts > limit {
            return ts;
        }
    }
    0
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let root = parse_input(lines.clone());
    let s1 = Instant::now();
    println!("{}", part1(Rc::clone(&root), 100000));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(Rc::clone(&root), 70000000, 30000000));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!("Time for part1: {:?}, part2: {:?}, total: {:?}", e1, e2, e);
}
