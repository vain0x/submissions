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
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($t:ty) =>
        (rl().parse::<$t>().unwrap());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

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

    while let Some(Rev((d1, u))) = heap.pop() {

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
    let (n, m, s, t) = read!(usize, usize, usize, usize);
    let s = s - 1;
    let t = t - 1;

    let mut g1 = vec![vec![]; n];
    let mut g2 = g1.clone();

    for _ in 0..m {
        let (u, v, a, b) = read!(usize, usize, i64, i64);

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

    let mut costs = (0..n).map(|i| dist1[i] + dist2[i]).vec();
    for y in (1..n).rev() {
        costs[y - 1] = min(costs[y - 1], costs[y]);
    }

    let money = 1_000_000_000_000_000_i64;
    for cost in costs {
        println!("{}", (money - cost).to_string());
    }
    return;
}
