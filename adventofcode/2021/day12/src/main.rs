use std::collections::{HashMap, HashSet};
use std::io;
use std::time::Instant;

#[allow(dead_code)]
fn read_input() -> Vec<String> {
    use std::io::prelude::*;
    let stdin = io::stdin();
    stdin.lock().lines().map(|x| x.unwrap()).collect()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Node(String);

impl Node {
    fn small(&self) -> bool {
        self.0 == self.0.to_lowercase()
    }
}

#[derive(Clone, Debug)]
struct Graph {
    vertices: HashSet<Node>,
    edges: HashMap<Node, Vec<Node>>,
}

impl Graph {
    fn paths(
        &self,
        start: Node,
        end: Node,
        mut path: Vec<Node>,
        paths: &mut Vec<Vec<Node>>,
        f: fn(Vec<Node>, Node) -> bool,
    ) {
        path = path.clone();
        path.push(start.clone());
        // println!("{:?}", path);

        if start == end {
            paths.push(path.clone());
            path.clear();
            return;
        }

        if !self.edges.contains_key(&start) {
            path.clear();
            return;
        }

        for edge in self.edges.get(&start).unwrap() {
            if !edge.small() || f(path.clone(), edge.clone()) {
                self.paths(edge.clone(), end.clone(), path.clone(), paths, f)
            }
        }
    }
}

fn part1(graph: Graph) -> String {
    let start = Node("start".to_string());
    let end = Node("end".to_string());
    let paths: &mut Vec<Vec<Node>> = &mut vec![];
    graph.paths(start, end, vec![], paths, |path, np| !path.contains(&np));
    format!("{}", paths.len())
}

fn valid_path(path: Vec<Node>, node: Node) -> bool {
    if !path.contains(&node) {
        return true;
    }

    let mut hc: HashMap<Node, i32> = HashMap::new();
    for n in path {
        if n.small() {
            match hc.get_mut(&n) {
                None => {
                    hc.insert(n, 1);
                }
                Some(c) => {
                    *c += 1;
                }
            };
        }
    }

    if let Some(c) = hc.get(&node) {
        if *c > 1 {
            return false;
        }
    }

    let vals: Vec<&i32> = hc.values().collect();
    let mut sum = 0;
    for &v in vals.clone() {
        sum += v;
    }
    let diff: i32 = sum - vals.len() as i32;
    diff == 0
}

fn part2(graph: Graph) -> String {
    let start = Node("start".to_string());
    let end = Node("end".to_string());
    let paths: &mut Vec<Vec<Node>> = &mut vec![];
    graph.paths(start, end, vec![], paths, |path, np| valid_path(path, np));
    format!("{}", paths.len())
}

fn parse_input(lines: Vec<String>) -> Graph {
    let mut vertices: HashSet<Node> = HashSet::new();
    let mut edges: HashMap<Node, Vec<Node>> = HashMap::new();
    for line in lines {
        let ns: Vec<String> = line.split('-').map(|s| s.to_string()).collect();

        let s = ns[0].clone();
        vertices.insert(Node(s.clone()));
        let e = ns[1].clone();
        vertices.insert(Node(e.clone()));

        if s != "end".to_string() && e != "start".to_string() {
            let ns = Node(s.clone());
            let ne = Node(e.clone());
            match edges.get_mut(&ns) {
                None => {
                    edges.insert(ns, vec![ne]);
                }
                Some(es) => {
                    es.push(ne);
                }
            };
        }
        if s != "start".to_string() && e != "end".to_string() {
            let ns = Node(s);
            let ne = Node(e);
            match edges.get_mut(&ne) {
                None => {
                    edges.insert(ne, vec![ns]);
                }
                Some(es) => {
                    es.push(ns);
                }
            };
        }
    }
    Graph { vertices, edges }
}

fn main() {
    let s = Instant::now();
    let lines = read_input();
    let graph = parse_input(lines.clone());
    // println!("{:?}", graph);
    let s1 = Instant::now();
    println!("{}", part1(graph.clone()));
    let e1 = s1.elapsed();
    let s2 = Instant::now();
    println!("{}", part2(graph.clone()));
    let e2 = s2.elapsed();
    let e = s.elapsed();
    println!(
        "Time for part1: {:?}, part2: {:?}, total with IO: {:?}",
        e1, e2, e
    );
}
