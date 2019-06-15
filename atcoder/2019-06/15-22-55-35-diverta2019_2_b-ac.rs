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
    let N = read!(usize);
    let mut T = read![i64, i64; N];
    T.sort();

    let mut PQ = vec![];
    for s in 0..N {
        let (sx, sy) = T[s];

        for t in s + 1..N {
            let (tx, ty) = T[t];

            let u = (sx - tx, sy - ty);
            let v = (tx - sx, ty - sy);

            PQ.push(u);
            PQ.push(v);
        }
    }
    PQ.sort();
    PQ.dedup();

    let mut min_cost = std::i64::MAX;

    if N == 1 {
        min_cost = 1;
    }

    for &(p, q) in &PQ {
        let mut set = BTreeSet::new();
        for i in 0..N {
            set.insert(T[i]);
        }

        let mut cost = 0;
        while let Some(&(x, y)) = set.iter().next() {
            set.remove(&(x, y));
            cost += 1;

            let (mut x, mut y) = (x, y);
            loop {
                x += p;
                y += q;

                if !set.remove(&(x, y)) {
                    break;
                }
            }
        }

        min_cost = min(min_cost, cost);
    }

    println!("{}", min_cost)
}
