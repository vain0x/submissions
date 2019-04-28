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

fn option_max<T: Ord>(l: Option<T>, r: Option<T>) -> Option<T> {
    match (l, r) {
        (None, x) | (x, None) => x,
        (Some(l), Some(r)) => Some(max(l, r)),
    }
}

fn main() {
    let N = read!(usize);
    let A = read![[i64]];

    // dp[i][b] = s
    // 0..i に総和が決まっている
    // b = 1 なら要素 i に -1 がかかる
    // 0..i の最大の総和が s である
    let mut dp = vec![vec![None, None]; N + 1];
    dp[0][0] = Some(0);

    for i in 0..N {
        for b in 0..2 {
            let sign = if b == 0 { 1 } else { -1 };
            dp[i + 1][0] = option_max(dp[i + 1][0], dp[i][b].map(|s| s + sign * A[i]));
            dp[i + 1][1] = option_max(dp[i + 1][1], dp[i][b].map(|s| s - sign * A[i]));
        }
    }

    debug!(dp);

    println!("{}", dp[N][0].unwrap())
}
