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
    M: usize,
    K: usize,
    F: Vec<Vec<usize>>,
    B: Vec<Vec<usize>>,
    done: Vec<bool>,
    components: Vec<usize>,
    sizes: Vec<usize>,
    friends: Vec<usize>,
    blocks: Vec<usize>,
}

impl Solver {
    fn dfs(&mut self, u: usize, c: usize) {
        self.done[u] = true;
        self.components[u] = c;
        self.sizes[c] += 1;

        for i in 0..self.F[u].len() {
            let v = self.F[u][i];

            if self.done[v] {
                continue;
            }

            self.dfs(v, c);
        }
    }

    fn solve(&mut self) {
        for u in 0..self.N {
            if !self.done[u] {
                self.dfs(u, u);
            }
        }

        for u in 0..self.N {
            for i in 0..self.F[u].len() {
                let v = self.F[u][i];
                if self.components[v] == self.components[u] {
                    self.friends[u] += 1;
                    self.friends[v] += 1;
                }
            }

            for i in 0..self.B[u].len() {
                let v = self.B[u][i];
                if self.components[v] == self.components[u] {
                    self.blocks[u] += 1;
                    self.blocks[v] += 1;
                }
            }
        }

        debug!(self.components, self.sizes, self.friends, self.blocks);

        println!(
            "{}",
            (0..self.N)
                .map(|u| {
                    let x = self.sizes[self.components[u]]
                        - (self.friends[u] / 2 + self.blocks[u] / 2 + 1);
                    x.to_string()
                })
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
}

fn main() {
    let (N, M, K) = read!(usize, usize, usize);

    let mut F = vec![vec![]; N];
    for (u, v) in read![usize, usize; M] {
        F[u - 1].push(v - 1);
        F[v - 1].push(u - 1);
    }

    let mut B = vec![vec![]; N];
    for (u, v) in read![usize, usize; K] {
        B[u - 1].push(v - 1);
        B[v - 1].push(u - 1);
    }

    Solver {
        N: N,
        M: M,
        K: K,
        F: F,
        B: B,
        done: vec![false; N],
        components: vec![N; N],
        sizes: vec![0; N],
        friends: vec![0; N],
        blocks: vec![0; N],
    }
    .solve()
}
