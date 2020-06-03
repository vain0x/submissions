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

fn dist(p: (usize, usize), q: (usize, usize)) -> i64 {
    let (px, py) = p;
    let (qx, qy) = q;

    (px as i64 - qx as i64).abs() + (py as i64 - qy as i64).abs()
}

fn main() {
    let (H, W, D) = read!(usize, usize, usize);
    let mut A = read![[usize]; H];
    let Q = read!(usize);

    let mut rev = vec![(0, 0); H * W];
    for y in 0..H {
        for x in 0..W {
            A[y][x] -= 1;
            rev[A[y][x]] = (y, x);
        }
    }

    let mut acc = vec![0; H * W + D];
    for i in D..H * W {
        acc[i] += acc[i - D] + dist(rev[i - D], rev[i]);
    }

    debug!(rev, acc);

    for _ in 0..Q {
        let (L, R) = read!(usize, usize);
        let (L, R) = (L - 1, R - 1);

        let mut total = acc[R];
        total -= acc[L];

        println!("{}", total)
    }
}
