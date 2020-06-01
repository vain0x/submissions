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
    read!(usize);
    let A = read![[usize]];

    static COLORS: &'static [(usize, usize, &'static str)] = &[
        (1, 399, "灰色"),
        (400, 799, "茶色"),
        (800, 1199, "緑色"),
        (1200, 1599, "水色"),
        (1600, 1999, "青色"),
        (2000, 2399, "黄色"),
        (2400, 2799, "橙色"),
        (2800, 3199, "赤色"),
    ];

    let mut colors = vec![];
    let mut wild = 0;

    for &a in &A {
        if a >= 3200 {
            wild += 1;
            continue;
        }

        for &(lb, ub, name) in COLORS {
            if lb <= a && a <= ub {
                colors.push(name);
                break;
            }
        }
    }

    colors.sort();
    colors.dedup();
    let min_count = max(colors.len(), min(wild, 1));
    let max_count = min(colors.len() + wild, COLORS.len());

    println!("{} {}", min_count, max_count)
}
