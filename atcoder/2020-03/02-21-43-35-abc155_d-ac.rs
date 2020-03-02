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

pub fn upper_bound<T: Ord>(xs: &[T], y: &T) -> usize {
    let mut l = 0;
    let mut r = xs.len() + 1;

    while r - l > 1 {
        let m = l + (r - l) / 2;
        if &xs[m - 1] <= y {
            l = m;
        } else {
            r = m;
        }
    }

    l
}

fn div_floor(x: i64, y: i64) -> i64 {
    if y < 0 {
        return div_floor(-x, -y);
    }

    if x >= 0 {
        x / y
    } else {
        (x - y + 1) / y
    }
}

fn div_ceil(x: i64, y: i64) -> i64 {
    if y < 0 {
        return div_floor(-x, -y);
    }

    if x > 0 {
        (x + y - 1) / y
    } else {
        x / y
    }
}

fn main() {
    let (N, mut K) = read!(usize, usize);
    let mut A = read![[i64]];

    K -= 1;
    A.sort();

    // K 番目の積を X とおく。
    // X 未満の積は K 個以下。X 以上の積は K 個より多い。

    // A(i) A(j) < m となるペア i, j (i < j) の個数を n(m) とおく。
    // n(X) <= K, n(X+1) > K となるはず。
    // n(m) <= K となる m の最大値を二分探索する。

    let mut l = -1_000_000_000_000_000_001_i64;
    let mut r = 1_000_000_000_000_000_001_i64;

    while r - l > 1 {
        let m = (l + r) / 2;

        let mut n = 0_usize;

        for j in 1..N {
            if A[j] == 0 {
                if m > 0 {
                    n += j;
                }
                continue;
            }

            if A[j] < 0 {
                // A(i) > m/A(j) となる i が条件を満たす。
                n += j - upper_bound(&A[..j], &div_floor(m, A[j]));
                continue;
            }

            // A(i) < m/A(j) となる i が条件を満たす。
            n += lower_bound(&A[..j], &div_ceil(m, A[j]));
        }

        debug!((l, r, m, n));

        let ok = n <= K;
        if ok {
            l = m;
        } else {
            r = m;
        }
    }

    println!("{}", l)
}
