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

fn main() {
    let N = rl().chars().collect::<Vec<char>>();
    let K = read!(usize);

    let h = N.len();

    let mut dp = vec![vec![vec![0; 2]; K + 2]; h + 1];
    dp[0][0][1] = 1;

    for i in 0..h {
        for k in 0..K + 1 {
            let d = (N[i] as u8 - b'0') as usize;

            // 1 以上を立てる
            dp[i + 1][k + 1][0] += 9 * dp[i][k][0];

            // 0 を立てる
            dp[i + 1][k][0] += dp[i][k][0];

            if d == 0 {
                // d=0 を立てる
                dp[i + 1][k][1] += dp[i][k][1];
            } else {
                // d を立てる
                dp[i + 1][k + 1][1] += dp[i][k][1];

                // 1 以上 d 未満を立てる
                dp[i + 1][k + 1][0] += (d - 1) * dp[i][k][1];

                // 0 を立てる
                dp[i + 1][k][0] += dp[i][k][1];
            }
        }
    }

    println!("{}", dp[h][K][1] + dp[h][K][0])
}
