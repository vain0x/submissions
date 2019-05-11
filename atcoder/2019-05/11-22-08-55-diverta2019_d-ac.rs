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

fn brute_force(N: i64) -> i64 {
    let mut sum = 0;
    for m in 1..N {
        if N / m == N % m {
            sum += m;
        }
    }
    sum
}

fn solve(N: i64) -> i64 {
    let mut sum = 0;

    let mut Q = 1;
    while Q * Q < N {
        if N / Q > 0 && N % Q == 0 {
            let m = N / Q - 1;
            if N / m == N % m {
                debug!(m);
                sum += m;
            }
        }
        Q += 1;
    }
    debug!(Q);

    let mut m = N / 2;
    while m >= 2 {
        let q = N / m;
        let r = N % m;

        if q <= Q {
            break;
        }
        if q > r {
            m -= 1;
            continue;
        }
        if q == r {
            sum += m;
            debug!(m);
        }
        debug_assert!(q <= r);
        // m を減らすごとに r は大きくなるので q が等しい範囲での探索は打ち切ってよい
        // 商が q+1 以上になる最小の m にジャンプ
        // debug!((m, q, r, N / q));
        m = min(m - 1, N / q);
    }

    sum
}

fn main() {
    // for N in 1..1000 {
    //     debug!(N);
    //     debug_assert_eq!(solve(N), brute_force(N), "N={}", N);
    // }

    let N = read!(i64);
    println!("{}", solve(N))
}
