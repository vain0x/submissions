//! ----------------------------------------------
//! Framework <https://github.com/vain0x/procon>
//!
//! See the bottom of file for solution.
//! ----------------------------------------------

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

    #[allow(deprecated)]
    buf.trim_right().to_owned()
}

// -----------------------------------------------
// Solution
// -----------------------------------------------

struct Solver {
    N: usize,
    G: Vec<Vec<usize>>,
    root: Vec<usize>,
    size: Vec<usize>,
    friendship: Vec<(usize, usize)>,
    block: Vec<(usize, usize)>,
}

impl Solver {
    fn new(N: usize, friendship: Vec<(usize, usize)>, block: Vec<(usize, usize)>) -> Solver {
        let mut G = vec![vec![]; N];

        for &(u, v) in &friendship {
            G[u].push(v);
            G[v].push(u);
        }

        Solver {
            N: N,
            G: G,
            root: vec![N; N],
            size: vec![0; N],
            friendship: friendship,
            block: block,
        }
    }

    fn dfs(&mut self, u: usize, root: usize) {
        if self.root[u] != self.N {
            return;
        }

        self.root[u] = root;
        self.size[root] += 1;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];

            self.dfs(v, root);
        }
    }

    fn solve(&mut self) {
        let N = self.N;

        for u in 0..N {
            self.dfs(u, u);
        }

        let mut friend_count = vec![0; N];
        let mut block_count = vec![0; N];

        for &(u, v) in &self.friendship {
            if self.root[u] == self.root[v] {
                friend_count[u] += 1;
                friend_count[v] += 1;
            }
        }

        for &(u, v) in &self.block {
            if self.root[u] == self.root[v] {
                block_count[u] += 1;
                block_count[v] += 1;
            }
        }

        let mut candidate_count = vec![0; N];
        for u in 0..N {
            candidate_count[u] = self.size[self.root[u]] - friend_count[u] - block_count[u] - 1;
        }

        println!(
            "{}",
            (0..N)
                .map(|u| candidate_count[u].to_string())
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
}

fn main() {
    let (N, M, K) = read!(usize, usize, usize);

    let mut friendship = vec![];
    for _ in 0..M {
        let (u, v) = read!(usize, usize);
        friendship.push((u - 1, v - 1));
    }

    let mut block = vec![];
    for _ in 0..K {
        let (u, v) = read!(usize, usize);
        block.push((u - 1, v - 1));
    }

    let mut solver = Solver::new(N, friendship, block);
    solver.solve();
}
