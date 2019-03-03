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

/// Print values to standard error if debug mode.
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

/// Read from standard input and parse each word.
/// - `read!(T, U, ..)` parses a line as a tuple of words.
/// - `read![[T]]` parses a line as an array of words.
/// - `read![..; N]` parses `N` lines, using `read!(..)` repeatedly.
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

/// Read a line from standard input.
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
    let T = read!(usize, usize; M);
    let mut C = vec![0; M];

    let mut uff = UnionFindForest::new(N);
    let mut size = vec![1; N];

    let mut c = N * (N - 1) / 2;
    for m in (0..M).rev() {
        C[m] = c;

        let (u, v) = T[m];
        let (u, v) = (u - 1, v - 1); // 1-indexed -> 0-indexed

        if uff.connects(u, v) {
            continue;
        }

        c -= size[uff.root(u)] * size[uff.root(v)];

        let mut n = size[uff.root(u)] + size[uff.root(v)];
        uff.merge(u, v);
        size[uff.root(u)] = n;
    }

    for m in 0..M {
        println!("{}", C[m]);
    }
}
