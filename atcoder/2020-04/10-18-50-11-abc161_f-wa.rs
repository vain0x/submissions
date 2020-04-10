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
    let N = read!(i64);

    // N = q・K^(m+1) + K^m となる K の集合。(K = 1 を含む。)
    let mut k_set = HashSet::new();

    // m = 0 のケース
    // N = q・K + 1
    // K = (N-1)/q
    {
        let M = N - 1;
        let mut d = 1_i64;
        while d * d <= M {
            if M % d == 0 {
                k_set.insert(d);
                k_set.insert(M / d);
            }

            d += 1;
        }
    }

    // m = 1, q = 0 のケース
    // N = K
    k_set.insert(N);

    // m = 1 かつ q >= 1 のケース
    // N = q・K^2 + K (q >= 1)
    let mut K = 2;
    while K * K <= N {
        if N - K >= K * K && (N - K) % (K * K) == 0 {
            k_set.insert(K);
        }

        K += 1;
    }

    // m >= 2 のケース
    let mut K = 2;
    while K * K <= N {
        // K^m
        let mut km = K * K;
        while km <= N {
            // K^(m+1)
            let km1 = km * K;

            // (N - K^m) = q・K^(m+1)
            if N - km >= km1 && (N - km) % km1 == 0 {
                k_set.insert(K);
                break;
            }

            km *= K;
        }

        K += 1;
    }

    k_set.remove(&1);

    println!("{}", k_set.len())
}
