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

pub fn lower_bound<T: Ord>(xs: &[T], y: &T) -> usize {
    // True-side.
    let mut l = 0;
    let mut r = xs.len() + 1;

    while r - l > 1 {
        let m = l + (r - l) / 2;
        if &xs[m - 1] < y {
            l = m;
        } else {
            r = m;
        }
    }

    l
}

struct Solver {
    H: usize,
    W: usize,
    A: Vec<Vec<i64>>,
    B: Vec<Vec<i64>>,
    forward_balances: Vec<HashSet<i64>>,
    backward_balances: Vec<HashSet<i64>>,
}

impl Solver {
    fn forward_dfs(&mut self, y: usize, x: usize, balance: i64) {
        let (H, W) = (self.H, self.W);

        if y == H / 2 {
            if self.forward_balances[x].insert(balance) {
                debug!(("forward", x, balance));
            }
            return;
        }

        let d = self.A[y][x] - self.B[y][x];

        for &t in &[balance + d, balance - d] {
            if y + 1 <= H / 2 {
                self.forward_dfs(y + 1, x, t);
            }

            if x + 1 < W {
                self.forward_dfs(y, x + 1, t);
            }
        }
    }

    fn backward_dfs(&mut self, y: usize, x: usize, balance: i64) {
        let (H, _) = (self.H, self.W);
        let d = self.A[y][x] - self.B[y][x];

        if y == H / 2 {
            if self.backward_balances[x].insert(balance + d) {
                debug!(("backward", balance));
            }
            if self.backward_balances[x].insert(balance - d) {
                debug!(("backward", balance));
            }
        }

        for &t in &[balance + d, balance - d] {
            if y >= H / 2 + 1 {
                self.backward_dfs(y - 1, x, t);
            }

            if x >= 1 {
                self.backward_dfs(y, x - 1, t);
            }
        }
    }

    fn solve(mut self) -> i64 {
        let (H, W) = (self.H, self.W);

        self.forward_dfs(0, 0, 0);
        self.backward_dfs(H - 1, W - 1, 0);

        let mut m = std::i64::MAX;

        for x in 0..W {
            let mut backward_balances = self.backward_balances[x]
                .iter()
                .cloned()
                .collect::<Vec<_>>();
            backward_balances.sort();

            for &f in &self.forward_balances[x] {
                let i = lower_bound(&backward_balances, &f);
                if i < backward_balances.len() {
                    let b = backward_balances[i];
                    let balance = f - b;
                    debug!((f, b, balance));
                    m = min(m, balance.abs());
                }
            }
        }

        m
    }
}

fn main() {
    let (H, W) = read!(usize, usize);
    let A = read![[i64]; H];
    let B = read![[i64]; H];

    let s = Solver {
        H: H,
        W: W,
        A: A,
        B: B,
        forward_balances: vec![HashSet::new(); W],
        backward_balances: vec![HashSet::new(); W],
    }
    .solve();

    println!("{}", s)
}
