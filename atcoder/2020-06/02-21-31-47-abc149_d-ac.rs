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

const RO: usize = 0;
const SI: usize = 1;
const PA: usize = 2;

fn win(x: usize, c: char) -> bool {
    match (x, c) {
        (RO, 's') | (SI, 'p') | (PA, 'r') => true,
        _ => false,
    }
}

fn main() {
    let (N, K) = read!(usize, usize);
    let (R, S, P) = read!(usize, usize, usize);
    let T = rl().chars().collect::<Vec<_>>();
    let score = [R, S, P];

    let M = N + K + 1;
    let mut t = vec![];
    let mut dp = vec![vec![0; 3]; M];
    let mut total = 0;

    for k in 0..K {
        t.clear();
        let mut i = k;
        while i < N {
            t.push(T[i]);
            i += K;
        }
        let n = t.len();

        for i in 0..n + 1 {
            for x in 0..3 {
                dp[i][x] = 0;
            }
        }

        for y in 0..3 {
            dp[1][y] = if win(y, t[0]) { score[y] } else { 0 };
        }

        for i in 1..n {
            for x in 0..3 {
                for y in 0..3 {
                    if y == x {
                        continue;
                    }

                    dp[i + 1][y] = max(
                        dp[i + 1][y],
                        dp[i][x] + if win(y, t[i]) { score[y] } else { 0 },
                    );
                }
            }
        }

        total += (0..3).map(|y| dp[n][y]).max().unwrap();
    }

    println!("{}", total)
}
