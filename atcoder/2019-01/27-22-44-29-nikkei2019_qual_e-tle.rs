// -----------------------------------------------
// Framework <https://github.com/vain0x/procon>
//
// See the bottom of file for solution.
// -----------------------------------------------

#![allow(unused_imports)]
#![allow(non_snake_case)]

use std::cell::RefCell;
use std::cmp::{max, min, Ordering};
use std::collections::*;
use std::fmt::{Debug, Display, Formatter, Write as FmtWrite};
use std::io::{stderr, stdin, BufRead, Write};
use std::mem::{replace, swap};
use std::ops::*;
use std::rc::Rc;

#[allow(unused_macros)]
macro_rules! debug {
    ($($e:expr),*) => {
        #[cfg(debug_assertions)]
        $({
            let (e, mut err) = (stringify!($e), stderr());
            writeln!(err, "\x1B[33m{}\x1B[0m = {:?}", e, $e).unwrap()
        })*
    };
}

#[allow(unused_macros)]
macro_rules! read {
    ([$t:ty] ; $n:expr) =>
        ((0..$n).map(|_| read!([$t])).collect::<Vec<_>>());
    ($($t:ty),+ ; $n:expr) =>
        ((0..$n).map(|_| read!($($t),+)).collect::<Vec<_>>());
    ([$t:ty]) =>
        (rl().split_whitespace().map(|w| w.parse().unwrap()).collect::<Vec<$t>>());
    ($($t:ty),*) => {{
        let buf = rl();
        let mut w = buf.split_whitespace();
        ($(w.next().unwrap().parse::<$t>().unwrap()),*)
    }};
}

#[allow(dead_code)]
fn rl() -> String {
    let mut buf = String::new();
    stdin().read_line(&mut buf).unwrap();
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

pub struct UnionFindForest {
    nodes: Vec<UffNode>,
}

enum UffNode {
    Root(usize),
    Child(usize),
}

impl UnionFindForest {
    pub fn new(size: usize) -> Self {
        UnionFindForest {
            nodes: (0..size).map(|_| UffNode::Root(1)).collect::<Vec<_>>(),
        }
    }

    pub fn root_node(&mut self, v: usize) -> (usize, usize) {
        match self.nodes[v] {
            UffNode::Root(rank) => (v, rank),
            UffNode::Child(u) => {
                let (u, rank) = self.root_node(u);
                self.nodes[v] = UffNode::Child(u);
                (u, rank)
            }
        }
    }

    pub fn root(&mut self, v: usize) -> usize {
        self.root_node(v).0
    }

    pub fn connects(&mut self, u: usize, v: usize) -> bool {
        self.root(u) == self.root(v)
    }

    pub fn merge(&mut self, u: usize, v: usize) {
        let (u, u_rank) = self.root_node(u);
        let (v, v_rank) = self.root_node(v);

        if u == v {
            return;
        }

        if u_rank < v_rank {
            self.merge(v, u);
            return;
        }

        self.nodes[v] = UffNode::Child(u);
        self.nodes[u] = UffNode::Root(u_rank + 1);
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let X = read![[i64]];
    let T = read![usize, usize, i64; M];

    let mut K = 0;
    let XS = X.iter().sum::<i64>();
    let mut done = vec![false; M];

    let mut uff = UnionFindForest::new(N);

    for e in 0..T.len() {
        let (u, v, w) = T[e];
        if w > XS {
            K += 1;
            done[e] = true;
            continue;
        }

        uff.merge(u - 1, v - 1);
    }

    loop {
        let mut uff_next = UnionFindForest::new(N);

        let mut stuck = true;
        let mut components = vec![0; N];
        for u in 0..N {
            let v = uff.root(u);
            components[v] += X[u];
        }

        for e in 0..T.len() {
            if done[e] {
                continue;
            }

            let (u, v, w) = T[e];
            if w > components[uff.root(u - 1)] {
                K += 1;
                done[e] = true;
                stuck = false;
                continue;
            }

            uff_next.merge(u - 1, v - 1);
        }

        if stuck {
            break;
        }

        uff = uff_next;
    }

    println!("{}", K)
}
