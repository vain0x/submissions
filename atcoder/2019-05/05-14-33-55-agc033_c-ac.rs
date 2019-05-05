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

#[derive(Clone)]
struct Solver {
    N: usize,
    G: Vec<Vec<usize>>,
    done: Vec<bool>,
}

impl Solver {
    fn reset(&mut self) {
        for i in 0..self.N {
            self.done[i] = false;
        }
    }

    fn dfs(&mut self, u: usize) -> (usize, usize) {
        self.done[u] = true;

        let mut D = 0;
        let mut W = u;

        for i in 0..self.G[u].len() {
            let v = self.G[u][i];
            if self.done[v] {
                continue;
            }

            let (w, d) = self.dfs(v);
            if D < d + 1 {
                D = d + 1;
                W = w;
            }
        }

        (W, D)
    }

    fn solve(mut self) {
        let (u, _) = self.dfs(0);
        self.reset();
        let (v, d) = self.dfs(u);
        debug!(u, v, d);

        println!("{}", if d % 3 != 1 { "First" } else { "Second" })
    }
}

fn main() {
    let N = read!(usize);
    let T = read![usize, usize; N - 1];

    let mut G = vec![vec![]; N];
    for (u, v) in T {
        G[u - 1].push(v - 1);
        G[v - 1].push(u - 1);
    }

    Solver {
        N: N,
        G: G,
        done: vec![false; N],
    }
    .solve()
}
