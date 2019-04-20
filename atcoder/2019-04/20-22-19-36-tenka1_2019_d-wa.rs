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

const P: i64 = 998244353;

fn main() {
    let N = read!(usize);
    let A = read![usize; N];
    let S = A.iter().sum::<usize>();
    let M = 300;

    let mut dp = vec![vec![vec![0; M + 1]; M + 1]; N + 1];
    for r in 0..M + 1 {
        for g in 0..M + 1 {
            if r + g > S {
                break;
            }

            let b = S - (r + g);
            if r + g > b && g + b > r && b + r > g {
                dp[N][r][g] = 1;
            }
        }
    }

    for k in (0..N).rev() {
        let a = A[k];

        for r in 0..M + 1 {
            for g in 0..M + 1 {
                if r >= a {
                    dp[k][r - a][g] += dp[k + 1][r][g];
                    dp[k][r - a][g] %= P;
                }
                if g >= a {
                    dp[k][r][g - a] += dp[k + 1][r][g];
                    dp[k][r][g - a] %= P;
                }
                dp[k][r][g] += dp[k + 1][r][g];
                dp[k][r][g] %= P;
            }
        }
    }

    let n = dp[0][0][0];

    println!("{}", n)
}
