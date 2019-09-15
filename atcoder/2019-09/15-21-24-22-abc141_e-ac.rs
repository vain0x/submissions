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

// based on: https://codeforces.com/blog/entry/3107

// z(s) = x is max s.t. T[0..x] = T[n - x..]
fn z(T: &[char]) -> Option<usize> {
    let n = T.len();

    // z[i] = z (i > 0) where
    //   z is max s.t. T[0..z] = T[i..i + z]
    let mut z = vec![0; n];

    // z-algorithm
    let mut l = 0;
    let mut r = 0;
    for i in 1..n {
        if i >= r {
            l = i;
            r = l;
            while r < n && T[r - l] == T[r] {
                r += 1;
            }
            z[i] = r - l;
            continue;
        }

        let k = i - l;
        if z[k] < r - i {
            z[i] = z[k];
            continue;
        }

        l = i;
        while r < n && T[r - l] == T[r] {
            r += 1
        }
        z[i] = r - l;
    }

    // debug!(z);

    (1..n).map(|k| min(k, z[k])).max()
}

fn main() {
    read!(usize);
    let S = rl().chars().collect::<Vec<_>>();

    let mut m = 0_usize;

    for i in 0..S.len() {
        m = max(m, z(&S[i..]).unwrap_or(m));
    }

    println!("{}", m)
}
