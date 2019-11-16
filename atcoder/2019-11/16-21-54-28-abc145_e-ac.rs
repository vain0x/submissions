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
    let (N, T) = read!(usize, usize);
    let mut A = vec![];
    let mut B = vec![];

    for (a, b) in read![usize, i64; N] {
        A.push(a);
        B.push(b);
    }

    let mut dp = vec![vec![0; T]; N + 1];
    dp[0][0] = 0;

    let mut prev = vec![vec![None; T]; N + 1];

    for i in 0..N {
        for t in 0..T {
            if dp[i + 1][t] < dp[i][t] {
                dp[i + 1][t] = dp[i][t];
                prev[i + 1][t] = prev[i][t];
            }

            if t + A[i] < T && dp[i + 1][t + A[i]] < dp[i][t] + B[i] {
                dp[i + 1][t + A[i]] = dp[i][t] + B[i];
                prev[i + 1][t + A[i]] = Some((i, t));
            }
        }
    }

    for t in 0..T - 1 {
        if dp[N][t + 1] < dp[N][t] {
            dp[N][t + 1] = dp[N][t];
            prev[N][t + 1] = prev[N][t];
        }
    }

    let mut dishes = BTreeSet::new();
    {
        let mut i = N;
        let mut t = T - 1;
        while let Some((j, u)) = prev[i][t] {
            dishes.insert(j);
            i = j;
            t = u;
        }
    }

    let mut max_b = 0;
    for i in 0..N {
        if !dishes.contains(&i) && max_b < B[i] {
            max_b = B[i];
        }
    }

    println!("{}", dp[N][T - 1] + max_b)
}
