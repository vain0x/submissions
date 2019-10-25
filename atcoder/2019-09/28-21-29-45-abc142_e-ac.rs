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

fn min_opt<T: Ord>(x: Option<T>, y: Option<T>) -> Option<T> {
    match (x, y) {
        (x, None) => x,
        (None, y) => y,
        (Some(x), Some(y)) => Some(min(x, y)),
    }
}

fn main() {
    let (N, M) = read!(usize, usize);
    let mut keys = vec![];
    for _ in 0..M {
        let (a, _) = read!(i64, usize);
        let C = read![[usize]];

        let mut s = 0;
        for c in C {
            s |= 1 << (c - 1);
        }

        keys.push((a, s));
    }

    let mut dp = vec![vec![None; 1 << N]; M + 1];
    dp[0][0] = Some(0);

    for i in 0..M {
        let (a, t) = keys[i];

        for s in 0..1 << N {
            dp[i + 1][s] = min_opt(dp[i + 1][s], dp[i][s]);

            dp[i + 1][s | t] = min_opt(dp[i + 1][s | t], dp[i][s].map(|x| x + a));
        }
    }

    let min_opt = dp[M][(1 << N) - 1];

    match min_opt {
        None => println!("-1"),
        Some(x) => println!("{}", x),
    }
}
