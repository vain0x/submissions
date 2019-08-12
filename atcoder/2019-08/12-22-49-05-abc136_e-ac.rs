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

fn divisors(x: i64) -> Vec<i64> {
    let mut d = 1;
    let mut ds = vec![];
    while d * d <= x {
        if x % d == 0 {
            ds.push(d);
            ds.push(x / d);
        }
        d += 1
    }
    ds.sort();
    ds.dedup();
    ds
}

const INF: i64 = 1 << 50;

fn dfs(i: usize, x: i64, A: &[i64], K: i64, d: i64, dp: &mut BTreeMap<(usize, i64), i64>) -> i64 {
    if i == A.len() && x == 0 {
        return 0;
    }

    if i == A.len() || x.abs() > K {
        return INF;
    }

    if let Some(&k) = dp.get(&(i, x)) {
        return k;
    }

    let k = {
        let a = A[i] % d;
        if a == 0 {
            dfs(i + 1, x, A, K, d, dp)
        } else {
            min(
                dfs(i + 1, x - a, A, K, d, dp) + a,
                dfs(i + 1, x + (d - a), A, K, d, dp) + (d - a),
            )
        }
    };

    debug!((d, i, x, k));
    dp.insert((i, x), k);
    k
}

fn good(A: &[i64], K: i64, d: i64) -> bool {
    let mut dp = BTreeMap::new();
    dfs(0, 0, A, K, d, &mut dp) <= 2 * K
}

fn main() {
    let (_, K) = read!(usize, i64);
    let A = read![[i64]];
    let sum = A.iter().sum::<i64>();

    let d = divisors(sum)
        .into_iter()
        .rev()
        .skip_while(|&d| !good(&A, K, d))
        .next()
        .unwrap();

    println!("{}", d)
}
