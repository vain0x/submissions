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
    A: Vec<Vec<i64>>,
    dp: Vec<Option<i64>>,
    group: Vec<i64>,
}

impl Solver {
    fn dfs(&mut self, s: usize) -> i64 {
        if let Some(x) = self.dp[s] {
            return x;
        }

        let x = self.do_dfs(s);
        self.dp[s] = Some(x);
        x
    }

    fn do_dfs(&mut self, s: usize) -> i64 {
        // debug!(s);

        let mut x = self.group[s];

        for t in (1..s).rev() {
            if s & t != t {
                continue;
            }

            x = max(x, self.dfs(t) + self.dfs(s ^ t));
        }

        // debug!((s, x));
        x
    }

    fn solve(mut self) {
        let N = self.N;

        for s in 0..(1 << N) {
            let mut sum = 0;
            for i in 0..N {
                if s & (1 << i) == 0 {
                    continue;
                }

                for j in i + 1..N {
                    if s & (1 << j) == 0 {
                        continue;
                    }

                    // debug!((s, i, j, self.A[i][j]));
                    sum += self.A[i][j];
                }
            }
            // debug!((s, sum));
            self.group[s] = sum;
        }

        println!("{}", self.dfs((1 << N) - 1))
    }
}

fn main() {
    let N = read!(usize);
    let A = read![[i64]; N];

    Solver {
        N: N,
        A: A,
        dp: vec![None; 1 << N],
        group: vec![0; 1 << N],
    }
    .solve()
}
