//! Framework <https://github.com/vain0x/procon>

#![allow(non_snake_case)]
#![allow(unused_imports)]

use std::cmp::Reverse;
use std::collections::*;

pub struct Scan(Box<dyn Iterator<Item = &'static str>>); // '

impl Scan {
    fn new() -> Self {
        let mut buf = String::new();
        let read_line = move || {
            std::io::stdin().read_line(&mut buf).unwrap();
            Box::leak(buf.split_off(0).into_boxed_str()).split_whitespace()
        };
        Scan(Box::new(std::iter::repeat_with(read_line).flatten()))
    }

    pub fn word<T: std::str::FromStr>(&mut self) -> T {
        self.0.next().unwrap().parse().ok().unwrap()
    }

    pub fn list<T: std::str::FromStr>(&mut self, len: usize) -> Vec<T> {
        std::iter::repeat_with(|| self.word()).take(len).collect()
    }
}

fn main() {
    let mut scan = Scan::new();

    let N = scan.word::<usize>();
    let M = scan.word::<usize>();
    let S = scan.word::<usize>();

    let mut graph = vec![vec![]; N];
    for _ in 0..M {
        let U = scan.word::<usize>() - 1;
        let V = scan.word::<usize>() - 1;
        let A = scan.word::<usize>();
        let B = scan.word::<usize>();
        graph[U].push((V, A, B));
        graph[V].push((U, A, B));
    }

    let mut C = vec![];
    let mut D = vec![];
    for _ in 0..N {
        C.push(scan.word::<usize>());
        D.push(scan.word::<usize>());
    }

    let mut queue = BinaryHeap::new();
    let mut maximals = vec![];
    let max_cash = (0..N)
        .filter_map(|u| (0..graph[u].len()).map(|i| graph[u][i].1).max())
        .max()
        .unwrap()
        * N;

    for t in 1..N {
        let mut best = None;

        queue.clear();
        queue.push((Reverse(0), S, 0));

        maximals.clear();
        maximals.resize(N, BTreeMap::new());
        maximals[0].insert(0, S);

        while let Some((Reverse(time), cash, u)) = queue.pop() {
            if u == t {
                best = Some(time);
                break;
            }

            if cash + C[u] <= max_cash {
                queue.push((Reverse(time + D[u]), cash + C[u], u));
            }

            for i in 0..graph[u].len() {
                let (v, A, B) = graph[u][i];
                if cash < A {
                    continue;
                }

                let x = time + B;
                let y = cash - A;
                if maximals[v]
                    .range(..=x)
                    .last()
                    .map_or(false, |(_, &vy)| vy >= y)
                {
                    continue;
                }

                maximals[v].insert(x, y);
                queue.push((Reverse(x), y, v));
            }
        }

        println!("{}", best.unwrap());
    }
}
