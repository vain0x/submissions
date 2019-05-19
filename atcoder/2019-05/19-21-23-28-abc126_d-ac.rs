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
    G: Vec<Vec<(usize, i64)>>,
    color: Vec<bool>,
    done: Vec<bool>,
}

impl Solver {
    fn dfs(&mut self, u: usize, W: i64) {
        if self.done[u] {
            return;
        }
        self.done[u] = true;
        self.color[u] = W % 2 == 0;

        for i in 0..self.G[u].len() {
            let (v, w) = self.G[u][i];
            self.dfs(v, W + w);
        }
    }

    fn solve(mut self) {
        self.dfs(0, 0);

        for i in 0..self.N {
            println!("{}", if self.color[i] { '0' } else { '1' })
        }
    }
}

fn main() {
    let N = read!(usize);
    let T = read![usize, usize, i64; N - 1];
    let mut G = vec![vec![]; N];
    for (u, v, w) in T {
        G[u - 1].push((v - 1, w));
        G[v - 1].push((u - 1, w));
    }
    Solver {
        N: N,
        G: G,
        done: vec![false; N],
        color: vec![false; N],
    }
    .solve()
}
