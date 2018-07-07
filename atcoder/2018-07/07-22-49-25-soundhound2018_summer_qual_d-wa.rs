#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Formatter};
use std::io::*;
use std::ops::*;
use std::str::FromStr;
use std::*;

// -----------------------------------------------
// Framework
// -----------------------------------------------

#[allow(unused_macros)]
macro_rules! debug {
    ($($arg:expr),*) => {
        #[cfg(debug_assertions)]
        {
            let entries = [$(&stringify!([$arg]:), &$arg as &Debug),*];
            stderr().write_fmt(format_args!("{:#?}\n", entries)).unwrap();
        }
    };
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

trait IteratorExt: Iterator + Sized {
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

impl<T: Iterator> IteratorExt for T {}

// -----------------------------------------------
// Solution
// -----------------------------------------------

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rev<T>(pub T);

impl<T: PartialOrd> PartialOrd for Rev<T> {
    fn partial_cmp(&self, other: &Rev<T>) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<T: Ord> Ord for Rev<T> {
    fn cmp(&self, other: &Rev<T>) -> Ordering {
        other.0.cmp(&self.0)
    }
}

fn p<T>(s: &str) -> T
where
    T: FromStr,
    T::Err: Debug,
{
    T::from_str(s).unwrap()
}

fn dijkstra(g: &Vec<Vec<(usize, usize, i64)>>, N: usize, start: usize) -> Vec<i64> {
    let mut dist = vec![std::i64::MAX; N];
    dist[start] = 0;

    let mut heap = BinaryHeap::new();
    heap.push(Rev((0, start)));

    for _ in 0..N {
        let Rev((d1, u)) = heap.pop().unwrap();

        for &(_, v, cost) in g[u].iter() {
            let d = d1 + cost;

            if d < dist[v] {
                dist[v] = d;
                heap.push(Rev((d, v)));
            }
        }
    }
    // debug!(heap.len());

    dist
}

pub fn main() {
    let stdin = stdin();
    let mut stdin = stdin.lock();
    let stdout = stdout();
    let mut stdout = stdout.lock();

    let mut line = String::new();

    stdin.read_line(&mut line).unwrap();
    let l: Vec<usize> = line.split_whitespace().map(p).vec();
    let (n, m, s, t) = (l[0], l[1], l[2], l[3]);
    let s = s - 1;
    let t = t - 1;

    // 両替前グラフ
    let mut g1 = vec![vec![]; n];
    let mut g2 = g1.clone();

    for _ in 0..m {
        line.clear();
        stdin.read_line(&mut line).unwrap();
        let l = line.split_whitespace().vec();
        let (u, v, a, b): (usize, usize, i64, i64) = (p(l[0]), p(l[1]), p(l[2]), p(l[3]));

        let u = u - 1;
        let v = v - 1;

        g1[u].push((u, v, a));
        g1[v].push((v, u, a));

        g2[u].push((u, v, b));
        g2[v].push((v, u, b));
    }

    let dist1 = dijkstra(&g1, n, s);
    let dist2 = dijkstra(&g2, n, t);
    // debug!(dist1, dist2);

    let mut costs = vec![0_i64; n];
    let mut current_cost = dist1[n - 1] + dist2[n - 1];
    for y in (0..n).rev() {
        if y < n - 1 {
            // 両替所 y が解禁
            // 現在の両替所と比べて安い方を選ぶ

            let new_cost = dist1[y] + dist2[y];
            if new_cost < current_cost {
                current_cost = new_cost;
            }
        }

        costs[y] = 1_000_000_000_000_000_i64 - current_cost;
    }

    for cost in costs {
        stdout.write(cost.to_string().as_bytes()).unwrap();
        stdout.write(&[b'\n']).unwrap();
    }
    stdout.flush().unwrap();

    return;
}
